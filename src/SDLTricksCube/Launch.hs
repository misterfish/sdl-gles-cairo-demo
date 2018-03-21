{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | notes:
--
-- • here we demo several ways of drawing on a texture, for rendering to
--   e.g. a cube face.
--
-- • single image: the texture is prepared once and bound to a named texture
-- object (actually an integer internally).
--
-- • multiple images: the texture is prepared on each iteration (or multiple
-- thereof) of the render loop.
--
-- • single image with cairo: draw on top of the image using cairo.
--
-- • no image with cairo: special case of previous one -- just clear the
-- cairo surface before drawing.
--
-- • multiple images with cairo.
--
-- • we manually prepare the backing arrays and copy pixels in using poke.
-- when using cairo, the same backing array (with caveats) is tied to a
-- cairo surface, and then we can just draw on it.
--
-- • the arrays are either CUShort or CUChar arrays. on my android the final
-- internal format has to be '565' format, which is one short per pixel. on
-- the computer, '8888' also works (4 bytes per pixel). so that explains the
-- 555 and 8888 modes.
--
-- • with cairo there's another complication: you can't draw on 565 arrays.
-- so for cairo surfaces on mobile, we use 8888_565: first draw in a 8888
-- array and then copy it to a 565 array. performance is reasonable, even
-- when doing the copy in the render loop.
--
-- • the famous texImage2D call is the one which gets the pixel array into
-- the texture, either once (for single images) or in the render loop (for
-- moving images and/or cairo).
--
--   autogen for textures can be convenient: you can leave off the texCoord
--   calls in between vertices when drawing the polygon.
--   but you lose a lot flexibilty, so we do it manually.
--
--   draw a plane, cube, or other polygon
--   hang the texture onto the polygon
--
--   with gles, a few other caveats:
--   keep the Floats as they are: changing to Double can cause immediate
--   crashes
--   glBegin/glEnd (renderPrimitive) doesn't work: have to push (optionally)
--
--   Quads doesn't exist, but TriangleFan does the same.
--
--   this seems like an unintuitive way to go about this, especially for
--   haskell, and it's not immediately clear why we use Cairo's
--   createImageSurfaceForData function when we're not necessarily dealing
--   with an image and we don't have any data. image surface is just the
--   generic way to refer to a surface backed by a pixel array. ok but why
--   not just use createImageSurface? it's because you can't (quickly) get
--   the underlying pixels inside the render loop, and GL needs them (in the
--   copyTexImage2D function). the function which gets the pixels is
--   extremely slow and couldn't be used in the render loop.
--
-- • we use the programmable pipeline (shaders, vertex buffers, and no
--   matrix stacks). before all drawing commands, all the matrices need to
--   be passed as uniforms.
-- • we keep model & view separate to allow for better lighting control.
-- • we keep track of all the matrices manually as we do not use any non
--   GLES-2.0 compatible features, and we do not use any glu* functions.
--   we use stacks to do this, in case it's useful at some point, though up
--   until now the last one on the stack has always been enough.
-- • the old 'preservingMatrix' (= glPushMatrix / glPopMatrix) behavior is
--   basically the default, since our structures are immutable of course:
--   pop the model (view / projection), do what you want with it, and pass
--   it to the shaders.
--   for the opposite case, where you want the changes to the matrices to
--   persist, be sure to create a new App structure with the updates.
-- • use frustumF or orthoF to set up the projection, and lookatF to set up
--   the view. lots of manual matrix math to get it working with float and
--   avoiding glu* functions, but it works on android, and gives us a
--   proper right-handed coordinate system.
-- • matrix howto: given fixed-function ('old-style') transforms:
--       T1
--       T2
--       T3
--       <draw>
--   new-style is
--       let model' = multMatrices [ M3, M2, M1, model ]
--       <draw with model>
--   to rotate the flowerpot, then push it down the x-axis:
--   multMatrices [ rotateZ, translateX, model ]
--   either read it forwards in grand, fixed coordinates or backwards in
--   local coordinates.
-- • matrix how to: column-/row-major confusion.
--   • consider a matrix in the natural order, where multiplication goes to
--     the right.
--   • write as a list in natural order.
--   • invert the list using invertMajor'.
--   • load into MD using fromList 4 4
--   • make into MG using toMGC before sending to shader.

module SDLTricksCube.Launch ( launch, launch_ ) where

import           Prelude hiding ( log )

import           Data.Char ( isDigit )
import           Data.List ( zip4, findIndex, zip5, intercalate )
import           Debug.Trace ( trace )
import           Data.Function ( (&) )
import           Data.Maybe ( isNothing, isJust, fromJust )
import           Data.Either ( isLeft )
import qualified Data.StateVar      as STV ( get )
import           Data.StateVar      as STV ( makeStateVar )
import           Data.Word ( Word8 )
import           Data.ByteString    as BS ( ByteString )
import qualified Data.ByteString.Char8  as BS8 ( pack )
import qualified Data.ByteString    as BS ( take )
import           System.Environment ( getArgs )
import           Control.Applicative ( empty )
import           System.Info   as I           ( os, arch )
import           Data.Monoid ( (<>) )
import           Foreign ( ForeignPtr
                         , Ptr
                         , Int32
                         , castPtr
                         , free
                         , mallocForeignPtrArray
                         , malloc
                         , mallocArray
                         , peek
                         , pokeArray
                         , pokeElemOff
                         , peekElemOff
                         , withForeignPtr
                         , withArray )
import qualified Foreign as F ( with )
import           Foreign.C.Types ( CUChar, CUShort, CInt )
import           Foreign.Marshal.Array as FMA ( callocArray )
import           GHC.Float ( double2Float, float2Double )
import           Data.Bits ( (.&.), (.|.), shiftL, shiftR )
import           Text.Printf ( printf )
import           Data.Foldable ( find , foldl', foldr )
import           Control.Monad ( (<=<), unless, guard, when, forM_, forM )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Concurrent ( threadDelay )
import qualified Data.Vector          as DV  ( Vector
                                             , fromList )
import qualified Data.Vector.Storable as DVS ( (!)
                                             , Vector
                                             , length
                                             , fromList )
import           Data.Vector.Storable as DVS ( unsafeWith
                                             , unsafeToForeignPtr0 )

import qualified Data.ByteString.Base64 as B64 ( decode )

import           Data.Stack ( Stack, stackNew, stackPush, stackPop )
import           Control.Conditional ( ifM )

import "matrix"  Data.Matrix        as DMX ( (!)
                                           , Matrix
                                           , fromList
                                           , toList
                                           , multStd
                                           , multStd2
                                           , transpose )

import           Codec.Picture      as JP ( decodePng )

import           SDL as S
                 ( ($=)
                 , Window
                   -- Linear.Affine
                 , Point (P)
                 , Renderer
                 , Surface
                 , RendererType (SoftwareRenderer, AcceleratedRenderer)
                 , Mode (Normal, Debug)
                 , Profile (Compatibility, ES)
                 , Texture
                 , BlendMode (BlendNone, BlendAlphaBlend, BlendAdditive, BlendMod)
                 , createRGBSurface
                 , destroyTexture
                 , glColorPrecision
                 , initializeAll
                 , createWindow
                 , defaultWindow
                 , windowInitialSize
                 , defaultRenderer
                 , rendererType
                 , rendererTargetTexture
                 , get
                 , rendererDrawColor
                 , defaultOpenGL
                 , glColorPrecision
                 , glDepthPrecision
                 , glStencilPrecision
                 , glMultisampleSamples
                 , glProfile
                 , glCreateContext
                 , glSwapWindow
                 , windowOpenGL
                 , createTextureFromSurface
                 , createRenderer )

import           Graphics.GL ( glDepthRangef
                               -- glFrustrumf is not exported
                             )

import           Graphics.Rendering.OpenGL as GL
                 ( Color4 (Color4)
                 , Color3 (Color3)
                 , GLint
                 , Matrix
                 , MatrixOrder ( RowMajor, ColumnMajor )
                 , MatrixComponent
                 , Program
                 , GLubyte
                 , Face (Front, Back)
                   -- no constructors, just synonym for Int32
                 , GLsizei
                 , Light (Light)
                 , ClientArrayType
                   ( VertexArray, ColorArray, TextureCoordArray )
                 , PolygonMode ( Point, Line, Fill )
                 , ComparisonFunction ( Less, Always )
                 , ShadingModel ( Smooth, Flat )
                 , PixelStoreDirection ( Unpack, Pack )
                 , BufferTarget ( ArrayBuffer )
                 , ClearBuffer ( ColorBuffer, DepthBuffer )
                 , TextureTarget2D ( TextureRectangle, Texture2D )
                 , Vertex2 ( Vertex2 )
                 , Vertex3 ( Vertex3 )
                 , Vertex4 ( Vertex4 )
                 , PrimitiveMode
                   ( Quads, Points, Triangles, TriangleFan, QuadStrip, Lines )
                 , TwoDimensionalTextureTarget
                 , Proxy ( NoProxy, Proxy )
                 , TextureTargetCubeMap ( TextureCubeMap )
                 , TextureTargetCubeMapFace
                   ( TextureCubeMapPositiveX
                   , TextureCubeMapPositiveY
                   , TextureCubeMapPositiveZ
                   , TextureCubeMapNegativeX
                   , TextureCubeMapNegativeY
                   , TextureCubeMapNegativeZ )
                 , TextureTarget2D
                 , PixelFormat ( BGR, RGB, RGBA, BGRA, DepthComponent )
                 , Position ( Position )

                  -- Not Ptr CUChar <that's the *Cairo* PixelData>
                 , PixelData ( PixelData )
                 , DataType ( Double, UnsignedByte, Float, UnsignedShort565 )
                 , Capability ( Enabled, Disabled )
                 , TextureSize2D ( TextureSize2D )
                 , TextureFilter ( Nearest, Linear' )
                 , Size ( Size )
                 , TextureCoordName ( S, T, R, Q )
                 , TextureFunction ( Modulate, Decal, Replace )
                 , Clamping ( Repeat, ClampToEdge, Clamp )
                 , GLsizeiptr
                 , GLfloat
                 , GLdouble
                 , Vector3 ( Vector3 )
                 , Vector4 ( Vector4 )
                 , BufferUsage ( StaticDraw )
                 , IntegerHandling ( ToFloat, ToNormalizedFloat, KeepIntegral )
                 , HintTarget (PolygonSmooth)
                 , HintMode (Fastest, Nicest, DontCare)
                 , BlendingFactor ( One, OneMinusSrcAlpha, SrcAlpha, DstAlpha, SrcAlphaSaturate, OneMinusDstAlpha )
                 , PixelInternalFormat ( RGBA'
                                       , RGBA8
                                       , RGB8
                                       , Luminance'
                                       , RGB' -- GL_RGB
                                       )
                 , currentProgram
                 , hint
                 , newMatrix
                 , vertex
                 , renderPrimitive
                 , activeTexture
                 , bindVertexArrayObject
                 , polygonMode
                 , cullFace
                 , matrix
                 , drawArrays
                 , flush
                 , arrayPointer
                 , uniformLocation
                 , attribLocation
                 , multMatrix
                 , matrixMode
                 , getMatrixComponents
                 , translate
                 , rotate
                 , scale
                 , perspective
                 , clientState
                 , bindBuffer
                 , genObjectNames
                 , vertexAttribPointer
                 , bufferData
                 , vertexAttribArray
                 , texture
                 , viewport
                 , position
                 , attenuation
                 , readPixels
                 , spotDirection
                 , spotCutoff
                 , spotExponent
                 , textureBinding
                 , textureResident
                 , textureFilter
                 , rowAlignment
                 , lighting
                 , materialSpecular
                 , materialDiffuse
                 , materialShininess
                 , lightModelAmbient
                 , lightModelLocalViewer
                 , lightModelTwoSide
                 , specular
                 , ambient
                 , normal
                 , diffuse
                 , specular
                 , light
                 , autoNormal
                 , blendFunc
                 , blend
                 , translate
                 , textureGenMode
                 , textureWrapMode
                 , textureLevelRange
                 , textureFunction
                 , copyTexImage2D
                 , texImage2D
                 , depthFunc
                 , shadeModel
                 , renderPrimitive
                 , loadIdentity
                 , matrixMode
                 , frustum
                 , lookAt
                 , clearDepth
                 , withMatrix
                 , withNewMatrix
                 , rect
                 , clearColor )

import qualified Graphics.Rendering.OpenGL as GL
                 ( clear
                   -- use our depthRange instead for compatibility with
                   -- Android, because depthRange uses doubles.
                 , project
                 , unProject
                 , depthRange )

import qualified SDL.Raw.Error as SRE ( getError, clearError )

import           Linear ( V2 (..)
                        , V4 (..)
                        , _x, _y )

import qualified Graphics.Rendering.Cairo as C
                 ( Surface
                 , Render
                 , Format (FormatARGB32, FormatA8, FormatRGB24)
                 , Operator (OperatorClear, OperatorSource)
                 -- only exported for certain cairo versions <see source>
                 -- formatStrideForWidth
                 , setOperator
                 , getOperator
                 , renderWith
                 , scale
                 , translate
                 , save
                 , restore
                 , transform
                 , rotate
                 , createImageSurfaceForData
                 , withImageSurfaceFromPNG
                 , imageSurfaceCreateFromPNG
                 , withPatternForSurface
                 , setLineWidth
                 , createImageSurface
                 , setSourceRGBA
                 , setSourceRGB
                 , setSource
                 , moveTo
                 , lineTo
                 , rectangle
                 , fill
                 , arc
                 , surfaceFlush
                 , stroke )

import qualified Graphics.Rendering.Cairo.Matrix as CM
                 ( Matrix ( Matrix )
                 , identity
                 , scale
                 , translate
                 )

import           SDLTricksCat.Render.Render     as SCC ( renderFrames )
import           SDLTricksCube.ImageMock ( imageBase64_1MOCK
                                         , imageBase64_2MOCK
                                         , imageBase64_WolfMOCK
                                         , movieMock )

import           SDLTricksCube.Draw ( pushVertices
                                    , pushColors
                                    , rectangle
                                    , triangle
                                    , sphere
                                    , cylinder
                                    , cylinderTex
                                    , coneSection
                                    , coneSectionTex
                                    , torus
                                    , pushTexCoords
                                    , pushNormals
                                    , lineStroke
                                    , rectangleStroke
                                    )

import           SDLTricksCube.Coords ( vec3
                                      , verl3
                                      , vec4
                                      , vec3gld
                                      , invertMajor'
                                      , ver3
                                      , ver3gld
                                      , frustumF
                                      , orthoF
                                      , lookAtF
                                      , normalize
                                      , unProjectItF
                                      , rotateX
                                      , rotateY
                                      , rotateZ
                                      , scaleX
                                      , scaleY
                                      , scaleZ
                                      , verple3
                                      , invertMatrix
                                      , identityMatrix
                                      , multMatrices
                                      , vecl4 )

import           SDLTricksCube.Shader ( uniform
                                      , uniformsMatrix
                                      , activateTexture
                                      , getShadersInline
                                      , initShaderColor
                                      , initShaderTexture
                                      , useShaderM
                                      , useShader
                                      , getShadersFilesystem
                                      , attrib )

import           SDLTricksCube.Wolf ( wolf )

import           SDLTricksCube.Util ( checkSDLError
                                    , hsvCycle
                                    , nAtATime
                                    , glTrueF
                                    , glFalseF
                                    , checkGLErrors
                                    , color
                                    , col8
                                    , color4
                                    , color3
                                    , stackPush'
                                    , map3
                                    , stackPop'
                                    , stackReplace'
                                    , replaceModel
                                    , replaceView
                                    , replaceProj
                                    , pushModel
                                    , pushView
                                    , pushProj
                                    , concatTuples2
                                    , concatTuples3
                                    , concatTuples4
                                    , frint
                                    , inv
                                    , randoms
                                    , deg2rad
                                    , vcross
                                    , vmag
                                    , v3x
                                    , v3y
                                    , v3z
                                    , vdot
                                    , vdiv
                                    , float
                                    , toDeg
                                    , fst3
                                    , snd3
                                    , thd3
                                    , appReplaceModel
                                    , appReplaceView
                                    , appReplaceProj
                                    , appUpdateMatrix
                                    , appMultiplyModel
                                    , appMultiplyRightModel
                                    , appMultiplyView
                                    , wrapGL )

import           SDLTricksCube.Pixels ( copyJPImageToPixelArray8888_RGBA
                                      , copyJPImageToPixelArray565
                                      , copyRaw8888To565 )

import           SDLTricksCube.Coords ( toMGC
                                      , toMGCD
                                      , translateX
                                      , translateY
                                      , translateZ
                                      , ver3
                                      , ver4
                                      , vec3 )

import           SDLTricksCube.Config ( doDebug
                                      , isEmbedded
                                      , colorProfile32
                                      , useGLES
                                      , openGLESVer
                                      , mvpConfig
                                      , output565 )

import           SDLTricksCube.CubeFaces ( drawCubeFaces )

import           SDLTricksCube.Types ( App (App)
                                     , Color
                                     , Bubble (Bubble)
                                     , Log (Log, info, warn, err)
                                     , Logger
                                     , GraphicsData (GraphicsSingle, GraphicsSingleCairo, GraphicsMoving)
                                     , Shader (ShaderC, ShaderT)
                                     , ShaderD (ShaderDC, ShaderDT)
                                     , Tex (NoTexture)
                                     , Flipper (NotFlipping, Flipping)
                                     , FlipDirection (FlipAscending, FlipDescending)
                                     , FlipHemisphere (FlipUpper, FlipLower)
                                     , GraphicsTextureMapping (GraphicsTextureMapping)
                                     , ProjectionType (ProjectionFrustum, ProjectionOrtho)
                                     , VertexData (VertexDataC, VertexDataT)
                                     , appLog
                                     , isFlipping
                                     , appMatrix
                                     , appUser1
                                     , appUser2
                                     , appUser3
                                     , shaderMatrix
                                     , shaderProgram
                                     , shaderExtra
                                     , shaderVertexData
                                     , shaderDCProgram
                                     , shaderDCUniformModel
                                     , shaderDCUniformView
                                     , shaderDCUniformProjection
                                     , shaderDCAttributePosition
                                     , shaderDCAttributeColors
                                     , shaderDTProgram
                                     , shaderDTUniformModel
                                     , shaderDTUniformView
                                     , shaderDTUniformProjection
                                     , shaderDTUniformTexture
                                     , shaderDTAttributePosition
                                     , shaderDTAttributeTexCoord
                                     , bubbleXVelocity
                                     , bubbleYVelocity
                                     , bubbleXPos
                                     , bubbleYPos
                                     , bubbleRadius
                                     , bubbleColor
                                     , bubbleLineWidth
                                     , tex565GLPixelData
                                     , tex565RawPixelData
                                     , tex8888GLPixelData
                                     , tex8888RawPixelData
                                     , tex8888_LuminanceRawPixelData
                                     , tex8888_LuminanceGLPixelData
                                     , tex8888_565GLPixelData565
                                     , tex8888_565RawPixelData565
                                     , tex8888_565GLPixelData8888
                                     , tex8888_565RawPixelData8888
                                     , newTex8888WithCairo
                                     , newTex8888NoCairo
                                     , newTex8888_565WithCairo
                                     , newTex565
                                     , getCSurf
                                     , graphicsTextureMappingGraphicsData
                                     , graphicsTextureMappingTexture
                                     , graphicsTextureMappingTextureObject
                                     , isTex8888
                                     , isTex565
                                     , isTex8888_565
                                     , mvpConfigProjectionType
                                     , mvpConfigCubeScale
                                     , mvpConfigTranslateZ
                                     , flHemisphere
                                     , flDirection
                                     , flAngleDeg
                                     , flAngleDegPrev
                                     -- , flAngleInc
                                     , flTick
                                     )
import           SDLTricksCube.Events ( processEvents )

-- gnuplot> plot [x=0:20] [0:180] x ** 1.75
-- gnuplot> plot [x=0:20] [0:180] (x / 5) ** 4
-- gnuplot> plot [x=0:20] [0:180] 1.5 ** x - 1.5
-- gnuplot> plot [x=0:20] [0:180] 1.3 ** x - 1.5
-- flipTickToAng = min 180 . (^^ 14) . (/ 20.0) . fromIntegral
-- flipTickToAng = min 180 . (** (1.75 :: Float)) . fromIntegral
-- flipTickToAng = min 180 . (** 4) . (/ 5) . fromIntegral
flipTickToAng x = min 180 . (+ inv 1.5) $ (1.3 ** fromIntegral x)
numTicksPerFlip = 20

doInitRotate   = True
doBorders      = True
doCube         = True
doCylinder     = False
doConeSection  = True
doTorus        = False
doWolf         = True
doBackground   = False

doLinesTest    = False
doShadersTest  = False
doFlowerpotTest = False

coneSectionSpinPeriod = 40
coneSectionSpinFactor = 120

mouseWheelFactor = (1 / 4.0)
maxTranslateZ = inv 2.5

-- must be Modulate if trying to use lighting on a texture.
textureFunction'              = Modulate
blendFunc'                    = ( SrcAlpha, OneMinusSrcAlpha )
blendFuncPolygonAntialiasing' = ( SrcAlphaSaturate, One )
hints' | useGLES              = []
       | otherwise            = [(PolygonSmooth, Nicest)]
shadeModel'                   = Smooth

sphereColor = color 0 57 73 255

faceSpec args rands = do
    let (withCairo, noCairo) =
            if output565 then (newTex8888_565WithCairo, newTex565)
                         else (newTex8888WithCairo, newTex8888NoCairo)

    catFrames <- concat . repeat <$> SCC.renderFrames False

    let g1 = GraphicsSingle (decodeImage' imageBase64_1MOCK) True
        g2 = GraphicsSingle (decodeImage' imageBase64_2MOCK) True
        g3 = GraphicsMoving (concat . repeat . map decodeImage' $ movieMock) 1 0
        g4 = GraphicsSingleCairo Nothing catFrames
        g5 = GraphicsSingleCairo (Just $ decodeImage' imageBase64_2MOCK) (bubbleFrames rands 0)
        g6 = GraphicsSingleCairo Nothing (bubbleFrames rands 0)
        gwolf = GraphicsSingle (decodeImage' imageBase64_WolfMOCK) True

        -- tn = NoTexture

    t1 <- noCairo textureWidth textureHeight
    t2 <- noCairo textureWidth textureHeight
    t3 <- noCairo textureWidth textureHeight
    t4 <- withCairo textureWidth textureHeight
    t5 <- withCairo textureWidth textureHeight
    t6 <- withCairo textureWidth textureHeight
    twolf <- noCairo textureWidth textureHeight

    -- outer / inner
    let faceSpecExtreme = [ (t1, g1), (t1, g1), (t1, g1), (t1, g1)
                          , (t2, g2), (t3, g3), (t4, g4), (t5, g5) ]

    let faceSpecSimple1 = [ (t1, g1), (t1, g1), (t1, g1), (t1, g1)
                          , (t1, g1), (t1, g1), (t1, g1), (t1, g1) ]

    let faceSpecSimple2 = [ (t2, g2), (t2, g2), (t2, g2), (t2, g2)
                          , (t2, g2), (t2, g2), (t2, g2), (t2, g2) ]

    let faceSpecSimple3 = [ (t1, g1), (t1, g1), (t1, g1), (t1, g1)
                          , (t2, g2), (t2, g2), (t2, g2), (t2, g2) ]

    let faceSpecSimple4 = [ (t2, g2), (t2, g2), (t2, g2), (t2, g2)
                          , (t1, g1), (t1, g1), (t1, g1), (t1, g1) ]

    let faceSpecOneMovie = [ (t1, g1), (t1, g1), (t1, g1), (t1, g1)
                           , (t2, g2), (t2, g2), (t2, g2), (t3, g3) ]

    let faceSpecNoMovie = [ (t1, g1), (t1, g1), (t1, g1), (t1, g1)
                          , (t2, g2), (t2, g2), (t4, g4), (t5, g5) ]

    -- mooi + snel, ook op 565 (computer)
    let faceSpecEightMovies = [ (t3, g3), (t3, g3), (t3, g3), (t3, g3)
                             , (t3, g3), (t3, g3), (t3, g3), (t3, g3) ]

    -- mooi
    let faceSpecOneCat = [ (t4, g4), (t1, g1), (t1, g1), (t1, g1)
                         , (t1, g1), (t1, g1), (t1, g1), (t1, g1) ]

    -- mooi
    let faceSpecTwoCats = [ (t4, g4), (t4, g4), (t1, g1), (t1, g1)
                          , (t1, g1), (t1, g1), (t1, g1), (t1, g1) ]

    -- mooi op 8888
    let faceSpecFourCats = [ (t4, g4), (t4, g4), (t4, g4), (t4, g4)
                           , (t1, g1), (t1, g1), (t1, g1), (t1, g1) ]

    -- mooi + snel, ook op 565 (computer)
    let faceSpecEightCats = [ (t4, g4), (t4, g4), (t4, g4), (t4, g4)
                            , (t4, g4), (t4, g4), (t4, g4), (t4, g4) ]

    let faceSpecEightBubbles = [ (t6, g6), (t6, g6), (t6, g6), (t6, g6)
                               , (t6, g6), (t6, g6), (t6, g6), (t6, g6) ]

    let faceSpecWolf    = [ (twolf, gwolf), (t1, g1), (t1, g1), (t1, g1)
                          , (t2, g2), (t2, g2), (t2, g2), (t2, g2) ]

    let arg  | args == [] = ""
             | otherwise = head args

    let spec | arg == "extreme"       =  faceSpecExtreme
             | arg == "simple1"       =  faceSpecSimple1
             | arg == "simple2"       =  faceSpecSimple2
             | arg == "simple3"       =  faceSpecSimple3
             | arg == "simple4"       =  faceSpecSimple4
             | arg == "onecat"        =  faceSpecOneCat
             | arg == "twocats"       =  faceSpecTwoCats
             | arg == "fourcats"      =  faceSpecFourCats
             | arg == "eightcats"     =  faceSpecEightCats
             | arg == "eightbubbles"  =  faceSpecEightBubbles
             | arg == "onemovie"      =  faceSpecOneMovie
             | arg == "nomovie"       =  faceSpecNoMovie
             | arg == "eightmovies"   =  faceSpecEightMovies
             | arg == "wolf"          =  faceSpecWolf
             | otherwise              =  faceSpecSimple3

    pure spec

textureDimension = 512
textureWidth = textureDimension
textureHeight = textureDimension
dimension = 0.5
frameInterval = 50
viewportWidth  = frint windowWidth  :: GLsizei
viewportHeight = frint windowHeight :: GLsizei
windowWidth  = 480 :: Int
windowHeight = 800 :: Int

aspectRatio = fromIntegral viewportWidth / fromIntegral viewportHeight :: Float

launch :: (Logger, Logger, Logger) -> IO ()
launch x = launch_ x []

launch_ :: (Logger, Logger, Logger) -> [String] -> IO ()
launch_ (androidLog, androidWarn, androidError) args = do
    let log = Log androidLog androidWarn androidError
        checkSDLError' = checkSDLError log
        info' = info log
        debug' = debug log

    debug' $ "embedded: " ++ show isEmbedded
    checkSDLError' "begin"
    debug' "initialising"
    initializeAll
    debug' "initialised"
    checkSDLError' "initializeAll"
    window <- createWindow "wuddup" $
        defaultWindow { windowInitialSize = V2 (frint windowWidth) (frint windowHeight)
                      , windowOpenGL = Just openGLConfig}
    checkSDLError' "createWindow"
    debug' "created window"

    -- debug' $ "image: " ++ (show . BS.take 30 $ imageBase64) ++ "..."

    rands <- randoms
    faceSpec' <- faceSpec args rands

    texNames <- initGL log window (length faceSpec') args

    debug' "initShaders"
    (colorShader, textureShader) <- initShaders log

    let map' (name, (graphics, tex)) = GraphicsTextureMapping tex graphics name
        texMaps                      = map map' $ zip texNames faceSpec'

        rotations | doInitRotate     = multMatrices [ rotateX 15, rotateY $ inv 15 ]
                  | otherwise        = identityMatrix

        appmatrix = ( stackNew, stackNew, stackNew )
                    & pushModel rotations
                    & pushView identityMatrix
                    & pushProj identityMatrix

        app = App { appLog = log
                  , appMatrix = appmatrix
                  , appUser1 = args
                  , appUser2 = []
                  , appUser3 = [] }

        app' = app & (appReplaceView $   initView app)
                   & (appReplaceProj =<< initProjection app)

    debug' "starting loop"
    appLoop window app' (colorShader, textureShader) texMaps (NotFlipping FlipUpper) 0 rands args

appLoop window app shaders texMaps flipper t rands args = do
    let log = appLog app
        info' = info log
        debug' = debug log
        dim = dimension

    debug' "* looping"

    debug' "updating textures"
    texMaps' <- updateTextures log texMaps
    debug' "done updating textures"

    -- let app' = app & (appReplaceView $   initView app)
    --               & (appReplaceProj =<< initProjection app)

    ( qPressed, click, dragAmounts, wheelOrPinchAmount ) <- processEvents log ( viewportWidth, viewportHeight )

    -- info' $ printf "wheel or pinch: %s" (show wheelOrPinchAmount)

    let remainingTranslateZ' = getRemainingTranslateZ app

    -- info' $ printf "cur translateZ %s" (show curTranslateZ)
    -- info' $ printf "remaining translateZ %.1f" remainingTranslateZ'

    app' <- ifM (pure $ isJust wheelOrPinchAmount)
                (do  let view' = translateZ dz''
                         dz' = (* mouseWheelFactor) . frint . fromJust $ wheelOrPinchAmount
                         dz'' = min dz' remainingTranslateZ'
                     pure $ app & appMultiplyView view')
                (pure app)

    wrapGL log "clear" $ do GL.clear [ ColorBuffer, DepthBuffer ]

    let ( colorShader, textureShader ) = shaders

    when doBackground $ do
        let app'' = app' & appReplaceModel identityMatrix
        drawBackground app'' colorShader args

    let rotationsMouse' = rotationsForDrag dragAmounts
        rotateForFlipper (NotFlipping hemi) = 0
        rotateForFlipper _ = flAngleDeg flipper - flAngleDegPrev flipper
        rotationFlipper' = rotateZ . rotateForFlipper $ flipper

    app'' <- do pure $ app'
                    & appMultiplyRightModel rotationsMouse'
                    & appMultiplyModel rotationFlipper'

    when doFlowerpotTest $ do
        testFlowerPot app'' colorShader      1  45
        testFlowerPot app'' colorShader (inv 1) 20

    when doShadersTest $ testShaders log colorShader args
    when doLinesTest   $ testLineStroke app'' colorShader args
    -- shouldn't these all be texMaps' ?
    when doConeSection $ coneSectionTest app'' (colorShader, textureShader) texMaps t args
    hit <- ifNotFalseM doCube $ do
        _app <- cube app'' (colorShader, textureShader) texMaps' t flipper args
        checkVertexHit _app click
    when doCylinder    $ cylinderTest app'' (colorShader, textureShader) texMaps 1 t args
    when doTorus       $ torusTest app'' (colorShader, textureShader) texMaps t args
    when doWolf        $ do
        let _app = app'' & appMultiplyModel model'
            model' = multMatrices [ scaleY 2.0
                                  , scaleX 2.0
                                  , scaleZ 2.0 ]
            appmatrix = appMatrix _app
            (model, view, proj) = map3 stackPop' appmatrix
            unvertex3' (Vertex3 x y z) = (x, y, z)
            unvertex4' (Vertex4 x y z w) = (x, y, z, w)

            -- VertexDataC ap ac an = shaderVertexData colorShader
            VertexDataT vp vt vn utt = shaderVertexData textureShader

            theShader = textureShader
            prog         = shaderProgram theShader
            (um, uv, up) = shaderMatrix theShader

            texNames = map graphicsTextureMappingTextureObject texMaps
            (texName0:_) = texNames
            theTexName = texName0

            (wolfVert', wolfTexCoords') = wolf
            -- triangles' = nAtATime 3 wolfVert'

            len' = length wolfVert'

        useShader log prog
        uniform log "model" um =<< toMGC model
        uniform log "view" uv =<< toMGC view
        uniform log "proj" up =<< toMGC proj
        activateTexture log theTexName utt
        vPtr <- pushVertices log vp . map unvertex3' $ wolfVert'
        let map' n' = [ (m', m', 0.0, 1.0)
                      , (m' + 0.1, m' + 0.1, 0.0, 1.0)
                      , (1.0, 1.0, 0.0, 1.0) ] where
                l' = frint $ len' - 1
                m' = (/ l') . frint $ n'
        tcPtr <- pushTexCoords log vt . map unvertex4' $ wolfTexCoords'
        nPtr <- pushNormals log vn . map (const $ vec4 0 0 1.0 1.0 ) $ [ 1 .. len' ]
        attrib log "vp" vp Enabled
        attrib log "vt" vt Enabled
        attrib log "vn" vn Enabled
        wrapGL log "drawArrays" . drawArrays Triangles 0 . frint $ len'
        attrib log "vn" vn Disabled
        attrib log "vp" vp Disabled
        attrib log "vt" vt Disabled
        free vPtr
        free tcPtr

    wrapGL log "swap window" $ glSwapWindow window

    let reloop = do let flipper' = updateFlipper flipper hit
                    threadDelayMs frameInterval
                    appLoop window app'' shaders texMaps' flipper' (t + 1) (tail rands) args

    if qPressed then pure () else reloop

initGL log window numTextures args = do
    let debug' = debug log
        info' = info log

    glCreateContext window

    wrapGL log "clearColor"   $ clearColor               $= Color4 0 0 0 0
    -- intuitive view: block greater depth points with lesser depth ones.
    wrapGL log "depthFunc"    $ depthFunc                $= Just Less

    when (not useGLES) . wrapGL log "shade model"        $ shadeModel      $= shadeModel'
    when (not useGLES) . wrapGL log "texture function"   $ textureFunction $= textureFunction'

    wrapGL log "enable blend" $ blend                                      $= Enabled
    wrapGL log "blendfunc"    $ blendFunc                                  $= blendFunc'

    forM_ hints' $ \(t, m) -> wrapGL log ("hint " <> show t) $ hint t   $= m

    -- wrapGL log "get active texture" $ do
    --     t <- STV.get activeTexture
    --     info' $ printf "active texture: %s" (show t)

    do p <- STV.get $ rowAlignment Pack
       u <- STV.get $ rowAlignment Unpack
       debug'       $ printf "row alignment: pack = %d, unpack = %d" p u

    -- How OpenGL transfers pixels to/from raw memory. Default is 4.
    -- Doesn't seem to be necessary.
    -- wrapGL log "row alignment unpack" $ rowAlignment Unpack      $= 1

    texNames <- wrapGL log "generating texture objects" $ genObjectNames numTextures

    forM_ texNames $ \texName -> do
        textureBinding Texture2D         $= Just texName
        -- important to set a filter.
        -- Nearest: quick & dirty.
        wrapGL log "texture filter"      $ textureFilter Texture2D  $= ((Nearest, Nothing), Nearest)

    -- ter info.
    -- depthRange doesn't seem to be necessary in conjunction with
    -- glFrustrum.
    -- if you do want to set it, be sure to use our (float) version, not
    -- GL.depthrange.
    when (not useGLES) $ do
       (dr1, dr2) <- wrapGL log "getting depthRange" $ STV.get GL.depthRange
       info' $ printf "dr1 %s dr2 %s" (show dr1) (show dr2)

    -- ter info.
    -- viewport is automatically initialised to pos 0, 0 and size 480 800
    -- ok to call on android too.
    do  vp <- wrapGL log "get viewport" $ STV.get viewport
        info' $ printf "viewport: %s" (show vp)

    debug' "testing doubles"
    let a = 0.34    :: Double
        b = 123.202 :: Double
    debug' $ printf "%.1f * %.1f = %.1f" a b (a * b)

    pure texNames

threadDelayMs = threadDelay . (* 1000)

openGLConfig = defaultOpenGL { glColorPrecision = color'
                             , glDepthPrecision = depth'
                             , glStencilPrecision = 8
                             , glMultisampleSamples = samples'
                             , glProfile = profile' } where

    (glesMaj, glesMin)         = openGLESVer

    -- GL_RED_SIZE etc.
    color'    | colorProfile32 = V4 8 8 8 8
              -- | output565      = V4 5 6 5 0
              | output565      = V4 8 8 8 0
              | otherwise      = V4 8 8 8 0
    profile'  | useGLES        = ES Normal glesMaj glesMin
              | otherwise      = Compatibility Normal 2 1
    -- n > 1: sets num buffers to 1 and num samples to n.
    samples'  | isEmbedded     = 4 -- anti-alias 4x FSAA
              | otherwise      = 4
    depth'    | isEmbedded     = 24 -- 16 was default
              | otherwise      = 24

-- cairo.
cFlipHorizontal :: Int -> C.Render ()
cFlipHorizontal = C.transform . cFlipHorizontalMatrix
cFlipHorizontalMatrix width = CM.Matrix (-1) 0 0 1 (fromIntegral width) 0

cube :: App -> (Shader, Shader) -> [GraphicsTextureMapping] -> Int -> Flipper -> [String] -> IO App
cube app shaders texMaps t flipper args = do
    let log          = appLog app
        appmatrix    = appMatrix app
        info'        = info log
        debug'       = debug log
        isFlipping'  = isFlipping flipper
        dim          = dimension
        shader       = fst shaders
        prog         = shaderProgram shader
        (um, uv, up) = shaderMatrix shader
        VertexDataC ap ac an = shaderVertexData shader
        shader'     = ShaderDC (Just prog) um uv up ap ac an
        tos = map graphicsTextureMappingTextureObject texMaps
        scale' = mvpConfigCubeScale mvpConfig
        model' = multMatrices [ scaleY scale'
                              , scaleX scale'
                              , scaleZ scale' ]
        app' = app & appMultiplyModel model'

    debug' $ "drawing cube"
    drawCubeFaces app' shaders dimension flipper t tos args

    debug' $ "drawing sphere"
    when (not isFlipping') $ drawSphere app' shader' dim

    pure app'

drawSphere app shader dim = sphere' where
    sphere' = sphere app' shader (slices', stacks') col r
    app'    = app & appMultiplyModel model'
    model'  = multMatrices [ translateZ dim
                           , translateY dim
                           , translateX dim ]
    col     = sphereColor
    r       = 0.025
    slices' = 10
    stacks' = slices'

renderToCairoSurface :: Bool -> Int -> Int -> C.Render () -> C.Render ()
renderToCairoSurface clear width height frame = do
    -- operatorSave <- C.getOperator
    let clear' = do
            C.setSourceRGBA 1 1 1 1
            C.rectangle 0 0 (fromIntegral width) (fromIntegral height)
            C.fill
    let clear'' = do
            C.setOperator C.OperatorClear
            C.rectangle 0 0 (fromIntegral width) (fromIntegral height)
            C.fill
            C.setOperator C.OperatorSource

    when clear clear''
    frame

debug | doDebug == True = info
      | otherwise       = const . const . pure $ ()

toRight (Right x) = x
toRight _         = error "not Right"
toLeft (Left x)   = x
toLeft _          = error "not Left"

-- (arbitrary choice of whether x is first or y, but these are small amounts
-- so it shouldn't matter too much)
rotationsForDrag (Just (x, y)) = do
    let rx' = frint y
        ry' = frint x
    multMatrices [ rotateX rx', rotateY ry' ]
rotationsForDrag Nothing       = identityMatrix

drawBackground app shader args = do
    let log       = appLog app
        appmatrix = appMatrix app
        prog = shaderProgram shader
        (um, uv, up) = shaderMatrix shader
        VertexDataC ap ac an = shaderVertexData shader
        (model, view, proj) = map3 stackPop' appmatrix

    useShader log prog

    uniform log "model" um =<< toMGC model
    uniform log "view"  uv =<< toMGC view
    uniform log "proj"  up =<< toMGC proj

    rectangle app (ap, ac, an) (c1, c2, c3, c4) (v1, v2, v3, v4) where
        a  = 60
        z  = inv 2
        dim'  = 5
        c1 = color 100 10 10 a
        c2 = color 90 10 10 a
        c3 = color 80 10 10 a
        c4 = color 70 10 10 a
        v1 = verple3 (inv dim', inv dim', z)
        v2 = verple3 (inv dim',     dim', z)
        v3 = verple3 (    dim',     dim', z)
        v4 = verple3 (    dim', inv dim', z)

updateFlipper flipper hit = flip' flipper where
    initFlip' FlipUpper = FlipDescending
    initFlip' FlipLower = FlipAscending
    flip' flipper'@(NotFlipping hemi)
      | hit == False = flipper'
      | otherwise = let angleDeg = if hemi == FlipUpper then 0 else 180 in
                    Flipping { flHemisphere = hemi
                             , flDirection = initFlip' hemi
                             , flAngleDeg     = angleDeg
                             , flAngleDegPrev = angleDeg
                             , flTick = 0 }
    flip' flipper'@(Flipping hemi dir ang _ tick)
      | hit == True = flipper'
      | tick == end' = NotFlipping hemi
      | otherwise = flipper' { flHemisphere = if ang < 90 then FlipUpper else FlipLower
                            , flAngleDeg = toAng' dir tick
                            , flAngleDegPrev = ang
                            , flTick = tick + 1 }
    end' = numTicksPerFlip
    toAng' dir tick = flipTickToAng $ u' dir tick
    u' FlipDescending tick = tick
    u' FlipAscending  tick = numTicksPerFlip - tick

checkVertexHit app click = do
    let isClick' = isJust click
    if  isClick' then checkVertexHit'' app click
                 else pure False

-- reading the depth buffer is not reliable (doesn't work on android and
-- supposedly causes a big performance hit), so unprojecting the mouse click
-- to look for the vertex becomes more difficult.
-- we do it by testing at a number of different depths (currently 10).
-- seems to work.
-- downside: if the vertex is behind other fragments it can still be clicked
-- on.
-- doesn't need to be impure, but easy to show debug statements.

checkVertexHit'' app click = do
    let log = appLog app
        debug'      = debug log
        depthMax = 1.0
        depthMin = 0.1
        check' = map (checkVertexHit' app click) [0, depthMin .. depthMax]
        find' = findIndex id check'

    -- not reliable.
    -- depth' <- getClickDepth log click

    let ret' Nothing = do
            debug' "No hits"
            pure False
        ret' (Just x) = do
            let y = frint x * 0.1 :: Float
            debug' $ printf "Got a hit at %.1f" y
            pure True
    ret' find'

checkVertexHit' app click depth' = maybe invalid' valid' clickLocation where
    log = appLog app
    dim = dimension
    Just (x, y) = click
    x' = frint x
    y' = frint y

    clickLocation = unProjectItF app (viewportWidth, viewportHeight) $ Vertex3 x' y' depth'

    -- debug' "uncounterintracontravertible"
    invalid' = False

    valid' (Vertex3 xup yup zup) = isClickHit (xup, yup, zup) (dim, dim, dim)

    -- debug' $ printf "unproject (%d, %d, %.1f) => %.1f %.1f %.1f" x y depth' xup yup zup

isClickHit (vert1X, vert1Y, vert1Z) (targetX, targetY, targetZ) = vertXHit && vertYHit && vertZHit where
    closeTo l m = (< 0.1) (abs $ l - m)
    vertXHit = vert1X `closeTo` targetX
    vertYHit = vert1Y `closeTo` targetY
    vertZHit = vert1Z `closeTo` targetZ

testLineStroke :: App -> Shader -> [String] -> IO ()
testLineStroke app shader args
  | args == ["|"] = pure ()
  | args == [] = testLineStroke app shader $
    [ "r", "-1", "-1", "-1", "1", "1", "-1", "|" ]
  | otherwise = do
      let log = appLog app
          ( [col, a, b, c, d, e, f], rest ) = splitAt 7 args
          thickness = 0.05
          c' | col == "r" = color  255  0    0    255
             | col == "g" = color  0    255  0    255
             | col == "b" = color  0    0    255  255
             | otherwise  = color  0    255  255  255
          v1 = verple3 (read a, read b, read c)
          v2 = verple3 (read d, read e, read f)
          prog         = shaderProgram shader
          (um, uv, up) = shaderMatrix shader
          VertexDataC ap ac an = shaderVertexData shader
          shader' = ShaderDC (Just prog) um uv up ap ac an

      info log $ printf "col: <%s>" col
      lineStroke app shader' (c', c') (v1, v2) thickness
      testLineStroke app shader rest

a `between` (b, c) = a >= l && a <= r where
    (l, r) | b <= c       = (b, c)
           | otherwise    = (c, b)

decodeImage imageBase64 = do
    let imageBase64Decoded = B64.decode imageBase64
    (when . isLeft $ imageBase64Decoded) $ do
        err' . toLeft $ imageBase64Decoded
    let imageBase64Decoded' = toRight imageBase64Decoded
        dynamicImage = decodePng imageBase64Decoded'

    (when . isLeft $ dynamicImage) $ do
        err'' . toLeft $ dynamicImage
    dynamicImage where
        err' e = Left $ "Couldn't decode base64, " ++ e
        err'' e = Left $ "Couldn't decode png, " ++ e

decodeImage' imageBase64 =
    if (isLeft img) then error "Bad image"
                    else toRight img where
    -- img = trace' "decoding!" $ decodeImage imageBase64
    img = decodeImage imageBase64
    trace' x = trace $ "look " ++ show x

updateTextures :: Log -> [GraphicsTextureMapping] -> IO [GraphicsTextureMapping]
updateTextures log texMaps = mapM map' texMaps where
    map' (GraphicsTextureMapping graphicsData tex texobj) = do
        graphicsData' <- updateTexture log graphicsData tex texobj
        pure $ GraphicsTextureMapping graphicsData' tex texobj

updateTexture log graph@(GraphicsSingle img False) tex texobj = pure graph
updateTexture log       (GraphicsSingle img True) tex texobj = do
    transferImageToTexture log img tex texobj
    texImage' log tex texobj
    pure $ GraphicsSingle img False

updateTexture log       (GraphicsMoving imgs ticks ticksElapsed) tex texobj
  | ticksElapsed == 0 = do
        transferImageToTexture log (head imgs) tex texobj
        texImage' log tex texobj
        let imgs' = tail' imgs
        pure $ GraphicsMoving imgs' ticks te'
  | otherwise = do
        let imgs' = imgs
        pure $ GraphicsMoving imgs' ticks te' where
    te' = (ticksElapsed + 1) `mod` ticks

updateTexture log       (GraphicsSingleCairo Nothing cFrames) tex texobj = do
    updateTextureCairo True tex $ head cFrames
    texImage' log tex texobj
    pure $ GraphicsSingleCairo Nothing (tail' cFrames)

updateTexture log       (GraphicsSingleCairo (Just img) cFrames) tex texobj = do
    transferImageToTexture log img tex texobj
    updateTextureCairo False tex $ head cFrames
    texImage' log tex texobj
    pure $ GraphicsSingleCairo (Just img) (tail' cFrames)

updateTextureCairo clear tex cFrame = do
    let cSurf' = getCSurf tex
    when (isJust cSurf') .
        C.renderWith (fromJust cSurf') $ renderToCairoSurface clear textureWidth textureHeight cFrame

tail' [x] = [x]
tail' xs = tail xs

-- copy an image into the texture.
-- only used for faces which need a background image.

transferImageToTexture log img tex texobj
  | isTex8888 tex = do
      let raw = tex8888RawPixelData tex
      copyJPImageToPixelArray8888_RGBA img raw
  | isTex565 tex = do
      let raw = tex565RawPixelData tex
      copyJPImageToPixelArray565 log img raw
  | isTex8888_565 tex = do
      -- transfer the image to the 8888 array, so it can still be drawn on
      -- by cairo, and then all transferred togther to 565.
      let raw = tex8888_565RawPixelData8888 tex
      copyJPImageToPixelArray8888_RGBA img raw
  | otherwise = error "transferImageToTexture: unknown type"

-- actually transfer the pixel array into the texture.
-- all textures, thus all faces, need to do this at least once.

texImage' log tex texobj
  | isTex8888 tex     = let gl = tex8888GLPixelData tex in texImage8888' texobj gl
  | isTex565 tex      = let gl = tex565GLPixelData tex  in texImage565' texobj gl
                            -- | isTex8888_Luminance tex = let gl = tex8888_LuminanceGLPixelData tex  in texImage8888_Luminance' texobj gl
  | isTex8888_565 tex =                                    texImage8888_565' log texobj tex
  | otherwise         = error "texImage': unknown type"

texImage'' internalFormat texobj gl = do
    let level' = 0
        texDim' = frint textureDimension
        texImage2D' = texImage2D Texture2D NoProxy level' internalFormat (TextureSize2D texDim' texDim') 0
    textureBinding Texture2D $= Just texobj
    texImage2D' gl

texImage8888'     = texImage'' GL.RGBA8 -- GL_RGBA

texImage565'      = texImage'' GL.RGB' -- GL_RGB
texImage8888_565' log texobj tex = do
    let ary8888 = tex8888_565RawPixelData8888 tex
        ary565 = tex8888_565RawPixelData565 tex
        gl565 = tex8888_565GLPixelData565 tex
    copyRaw8888To565 log ary8888 ary565
    texImage565' texobj gl565

-- • mapping from eye coordinates to NDC.
-- • near plane of 1.5 gives a decent FOV (no fish-eye), but we have to push
--   the model away a bit down the z-axis during initView.
-- • eye coordinates are right-handed and go from [-1, -1, ?] to [1, 1, ?].
-- • NDC are also right-handed and go from [-1, -1, -1] to [1, 1, 1]
-- • far plane doesn't change the FOV, only how much depth there is:
--   keep it small because otherwise there is z-fighting and the depth buffer
--   gets messed up.
-- • the perspective / gluPerspective call is more intuitive, but we use
--   frustum.
-- • on android we don't have the frustum call, so we roll it by hand using
--   matrix math; plus it works nicely with the programmable pipeline.

initProjection app = do
    let log = appLog app
        info' = info log
        debug' = debug log
        project ProjectionOrtho   = orthoF   r t n f
        project ProjectionFrustum = frustumF r t n f

    -- can use this to check results against gluFrustum.
    -- wrapGL log "matrixMode Projection" $ matrixMode $= Projection
    -- wrapGL log "loadIdentity" $ loadIdentity
    -- wrapGL log "gluFrustum" $ frustum l r b t n f

    pure . project $ mvpConfigProjectionType mvpConfig

    where
        r =     1
        t =     1
        n =     1.5
        f =     10
        -- l = inv 1
        -- b = inv 1

-- initialise a right-handed view, with positive z out of the screen, and
-- push the model down the negative z axis a bit.

initView app = multMatrices m where
    log = appLog app
    m = [ lookAtF log v1 v2 v3
        , translateZ tz ]

    v1d = ver3gld 0 0 lzd
    v2d = ver3gld 0 0 0
    v3d = vec3gld 0 1 0

    v1 = ver3 0 0 lz
    v2 = ver3 0 0 0
    v3 = vec3 0 1 0

    lz = 0.4
    tz = mvpConfigTranslateZ mvpConfig

    lzd = 0.4

bubbleFrames :: [Double] -> Int -> [C.Render ()]
bubbleFrames rands t = bubbleFrames' rands t []

bubbleFrames' rands t [] = bubbleFrames' rands t bubbles' where
    bubbles' = [ Just $ Bubble 1 10 0 0 20 (1, 0, 1, 1) 5
               , Just $ Bubble 2 (inv 10) 20 0  5 (0, 1, 1, 1) 10
               , Just $ Bubble (inv 2) 10 300 200 15 (0, 0, 1, 1) 1
               , Just $ Bubble (inv 3) 10 300 200 25 (0, 0, 1, 1) 5
               , Just $ Bubble 2 10 300   0 10 (0, 0, 1, 1) 5 ]

bubbleFrames' rands t bubbles = frame : bubbleFrames' (tail rands) (t + 1) bubbles' where
    t' = frint t
    bubbles' = map (advanceBubble rands) bubbles
    rgba' (a, b, c, d) = C.setSourceRGBA a b c d
    drawBubble' bubble = do
        let x = bubbleXPos bubble
            y = bubbleYPos bubble
            r = bubbleRadius bubble
            c = bubbleColor bubble
            th = bubbleLineWidth bubble
        C.save
        C.translate x y
        C.moveTo r 0
        C.arc 0 0 r 0 (2 * pi)
        C.setLineWidth th
        rgba' c
        C.stroke
        C.restore
    frame = do
        let doBubble' Nothing = pure ()
            doBubble' (Just bubble) = drawBubble' bubble
        C.translate 512 512
        C.rotate pi
        mapM_ doBubble' bubbles

advanceBubble rands Nothing = Nothing
advanceBubble rands (Just bubble) = do
    let xv = bubbleXVelocity bubble
        yv = bubbleYVelocity bubble
        x  = bubbleXPos bubble
        y  = bubbleYPos bubble
        x' = x + xv
        y' = y + yv
        yv' = yv * yv''
        yv'' | r1 < 0.3 = 0.8
             | r1 > 0.6 = 1.2
             | otherwise = 1
        xv' = xv + xv''
        xv'' = 0
        r1 = (!! 0) rands
        bubble' | x < 0 || x > 512 || y < 0 || y > 512 = Nothing
                | otherwise = pure $ bubble { bubbleXPos = x'
                                            , bubbleYPos = y'
                                            , bubbleXVelocity = xv'
                                            , bubbleYVelocity = yv' }
    bubble'

-- note: only setter.
-- depthRange :: (Float, Float) -> IO ()
depthRange :: (Float, Float) -> IO ()
depthRange = uncurry glDepthRangef

getOrder m = withMatrix m $ \order _ -> pure order

-- not possible with GLES.
-- also, supposedly really bad for performance: forces pipeline flush.
getClickDepth log click = do
    let info' = info log
        debug' = debug log
    ptr' <- mallocArray $ 1 :: IO (Ptr GLfloat)
    let pos' = Position x' y'
        x' = fst . fromJust $ click
        y' = frint viewportHeight - yy'
        yy' = snd . fromJust $ click
        size' = Size 1 1
        pd' = PixelData GL.DepthComponent GL.Float ptr'
    wrapGL log "readPixels" $ readPixels pos' size' pd'
    p <- peekElemOff ptr' 0
    free ptr'
    info' $ printf "DEPTH: %.3f" p
    pure p

initShaders log = do
    let getShaders | isEmbedded == True = pure getShadersInline
                   | otherwise = getShadersFilesystem

    ( vShaderColor, fShaderColor, vShaderTexture, fShaderTexture ) <- getShaders

    let colorShader   = initShaderColor log vShaderColor fShaderColor mvp1 v1 Nothing
        mvp1 = ("model", "view", "projection")
        v1 = ("a_position", "a_color", "a_normal")

        textureShader = initShaderTexture log vShaderTexture fShaderTexture mvp2 v2 extra2
        mvp2 = ("model", "view", "projection")
        v2 = ("a_position", "a_texcoord", "a_normal", "texture")
        extra2 = Just (["transpose_inverse_model", "do_vary_opacity"], [])

    (,) <$> colorShader <*> textureShader

testShaders log shader args = do
    let prog         = shaderProgram shader
        (um, uv, up) = shaderMatrix shader
        VertexDataC ap ac an = shaderVertexData shader
        dim = dimension
        nx1 = inv dim
        py1 = dim
        px1 = dim
        frontFace' z' = [ ( nx1, 0, z' )
                        , ( nx1, py1, z' )
                        , ( px1, py1, z' )
                        , ( px1, 0, z' ) ]

        z = -3
        -- each one is a 4-ple => one vertex
        col1 = (float 1, float 0, float 0, float 1)
        col2 = (float 0, float 1, float 0, float 1)
        col3 = (float 0, float 0, float 1, float 1)
        col4 = (float 1, float 1, float 0, float 1)

    useShader log prog

    vPtr <- pushVertices log ap $ frontFace' z
    cPtr <- pushColors log ac [col1, col2, col3, col4]

    -- xxx
    let mv = undefined
        proj = undefined
    -- mv   <- modelview log
    -- proj <- projection log

    -- uniform log "um" um mv
    -- uniform log "up" up proj

    attrib log "ap" ap Enabled
    attrib log "ac" ac Enabled

    wrapGL log "drawArrays" $ drawArrays TriangleFan 0 4

    attrib log "ac" ac Disabled
    attrib log "ap" ap Disabled

    free vPtr
    free cPtr

    pure ()

testFlowerPot app shader dx thetaz = do
    let log = appLog app
        appmatrix = appMatrix app
        mp = shaderProgram shader
        (um, uv, up) = shaderMatrix shader
        VertexDataC ap ac an = shaderVertexData shader
        c1 = color 255 0 0 255
        c2 = color 0 255 0 255
        c3 = color 0 0 255 255
        v1 = verple3 (inv 0.1, 0, z)
        v2 = verple3 (0.1, 0, z)
        v3 = verple3 (0, 0.5, z)
        z = inv 2
        (model, view, proj) = map3 stackPop' appmatrix
        -- flowerpot rotated, still on world x
        mm2 = multMatrices [ rotateZ thetaz, translateX dx, model ]

    useShaderM log (Just mp)

    uniform log "model" um =<< toMGC mm2
    uniform log "view"  uv =<< toMGC view
    uniform log "proj"  up =<< toMGC proj

    triangle log (ap, ac, an) (c1, c2, c3) (v1, v2, v3)

coneSectionTest app shaders texMaps t args = do
    let app'    = app & appMultiplyModel model'
        model'  = multMatrices [ rotateY amount' ]
        period' = frint coneSectionSpinPeriod
        t'      = frint . (`mod` coneSectionSpinPeriod) $ t
        theta'  = t' / period' * 2 * pi
        amount' = frint coneSectionSpinFactor * sin theta'
    cylinderTest app' shaders texMaps 1.3 t args

-- texMaps in this app has 8 elements.
cylinderTest app shaders texMaps radiusRatio t args = do
    let log = appLog app
        doRotate   = False
        transform' | doRotate  = appMultiplyModel model'
                   | otherwise = id
        app'       = app & transform'
        model'     = multMatrices [ rotateX t'
                                  , rotateZ (t' * 1.1) ]
        t'         = 360 * frint t / 30
        shaderC'    = fst shaders
        shaderT'    = snd shaders

        shaderC    = toShaderDC True shaderC'
        shaderT    = toShaderDT False shaderT'
        progT      = shaderProgram shaderT'
        extraT     = shaderExtra shaderT'
        -- run-time
        (_:doVaryOpacityUniform':_) = fst . fromJust $ extraT
        udvot = doVaryOpacityUniform'

        npoly      = 60
        h          = 0.9
        circum     = 4 * h
        radius          = circum / 2 / pi
        texNames = map graphicsTextureMappingTextureObject texMaps
        -- [texName0, texName1] = [head texNames, last texNames]
        (texName0:texName1:_) = texNames

        tx00 = Vertex4 0 0 0 1
        tx01 = Vertex4 0 1 0 1
        tx10 = Vertex4 1 0 0 1
        tx11 = Vertex4 1 1 0 1

        (r1, g1, b1) = hsvCycle 200 $ t
        (r2, g2, b2) = hsvCycle 200 $ t + 30
        (r3, g3, b3) = hsvCycle 200 $ t + 60

        col1 = color r1 g1 b1 255
        col2 = color r2 g2 b2 255
        col3 = color r3 g3 b3 255

        angles a      = [0, 2 * pi / a ..]
        anglesShift a = [pi / 2, pi / 2 + 2 * pi / a ..]
        tx'0 = (tx00, tx01, tx10, tx11)
        tx'1 = (tx00, tx01, tx10, tx11)
        tx'2 = (tx00, tx01, tx10, tx11)
        tx'3 = (tx00, tx01, tx10, tx11)

    -- color sheet.
    forM_ (zip4 [1 .. 3] [col1, col2, col3] (anglesShift 3) (drop 1 $ anglesShift 3)) $ \(_, c, a, b) -> do
        let cylinder'    = cylinder app' shaderC npoly h (radius * 0.99) a b c
            coneSection' = do  let radius' = radius * 0.99
                               coneSection app' shaderC npoly h radius' (radiusRatio * radius') a b c
        if radiusRatio == 1 then cylinder' else coneSection'

    useShader log progT
    uniform log "udvot" udvot glFalseF

    let textureSheet radius' = do
        forM_ (zip5 [1 .. 4] [tx'0, tx'1, tx'2, tx'3] (angles 4) (drop 1 $ angles 4) [texName0, texName1, texName0, texName1]) $ \(_, tx, a, b, texName) -> do
            let cylinderTex'    = cylinderTex app' shaderT npoly h radius' a b texName tx
                coneSectionTex' = coneSectionTex app' shaderT npoly h radius' (radius' * radiusRatio) a b texName tx
            if radiusRatio == 1 then cylinderTex' else coneSectionTex'
    textureSheet radius
    textureSheet $ radius * 0.98

torusTest2 app shaders texMaps t args = do
    let shaderC = (toShaderDC True) . fst $ shaders
        ncylinders = 10
        innerRadius = 0.5
        thickness = 0.02
        draw' = torusTest' app shaderC innerRadius thickness ncylinders
    forM_ [0 .. 10] draw'

torusTest app shaders texMaps t args = do
    let shaderC = (toShaderDC True) . fst $ shaders
        ncylinders = 40
        innerRadius = 0.5
        thickness = 0.1
    torus app shaderC innerRadius thickness ncylinders

torusTest' app shaderC innerRadius thickness ncylinders n = do
    let app' = app & appMultiplyModel model'
        t' = 0.5 - (frint (n + 1) / 10)
        t'' = t' * 10
        model' = multMatrices [ translateY t'
                              , translateX t' ]
    torus app' shaderC innerRadius thickness ncylinders

toShaderDC shouldUseProgram shaderC' =
    ShaderDC ( prog )
             ( fst3  . shaderMatrix     $ shaderC' )
             ( snd3  . shaderMatrix     $ shaderC' )
             ( thd3  . shaderMatrix     $ shaderC' )
             vp vc vn where
    VertexDataC vp vc vn = shaderVertexData shaderC'
    prog | shouldUseProgram = Just . shaderProgram $ shaderC'
         | otherwise        = Nothing

toShaderDT shouldUseProgram shaderT' =
    ShaderDT ( prog )
             ( fst3  . shaderMatrix     $ shaderT' )
             ( snd3  . shaderMatrix     $ shaderT' )
             ( thd3  . shaderMatrix     $ shaderT' )
             utt vp vt vn where
    VertexDataT vp vt vn utt = shaderVertexData shaderT'
    prog | shouldUseProgram = Just . shaderProgram $ shaderT'
         | otherwise        = Nothing


-- ok, normal if would have been fine too xxx
ifNotFalseM p t = ifM (pure p) t (pure False)

getRemainingTranslateZ app = max 0 $ maxTranslateZ - curTranslateZ' where
    view' = app & appMatrix & snd3 & stackPop'
    curTranslateZ' = (DMX.!) view' (4, 3) -- row, col, 1-based

