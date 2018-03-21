{-# LANGUAGE PackageImports #-}

module Graphics.SGCDemo.CubeFaces ( drawCubeFaces ) where

import           Prelude hiding ( log )

import           Data.List ( zip4, unfoldr )
import           Debug.Trace ( trace )
import           Control.Monad ( (<=<), unless, guard, when, forM_ )
import           Text.Printf ( printf )
import           Data.Function ( (&) )
import           Foreign ( free )
import           Data.Maybe ( isNothing, isJust, fromJust )
import           Data.Time ( UTCTime, diffUTCTime, getCurrentTime )
import           Data.Either ( isLeft )
import qualified Data.StateVar      as STV ( get )
import           Data.StateVar      as STV ( ( $= ), makeStateVar )
import           Data.Word ( Word8 )
import           Data.ByteString    as BS ( ByteString )
import qualified Data.ByteString.Char8  as BS8 ( pack )
import qualified Data.ByteString    as BS ( take )
import           Control.Applicative ( empty )
import           Data.Monoid ( (<>) )
import           Data.Stack ( Stack, stackNew, stackPush, stackPop )
import "matrix"  Data.Matrix        as DMX ( Matrix
                                           , fromList
                                           , toList
                                           , multStd
                                           , multStd2
                                           , transpose )

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
                 , Vertex1 ( Vertex1 )
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
                 , TextureGenMode ( NormalMap, SphereMap, EyeLinear )
                 , Plane ( Plane )
                 , Repetition ( Repeated )
                 , Clamping ( Repeat, ClampToEdge, Clamp )
                 , VertexArrayObject
                 , GLsizeiptr
                 , GLfloat
                 , GLdouble
                 , GLmatrix
                 , Vector3 ( Vector3 )
                 , Vector4 ( Vector4 )
                 , BufferUsage ( StaticDraw )
                 , AttribLocation -- ( AttribLocation )
                 , UniformLocation
                 , VertexArrayDescriptor ( VertexArrayDescriptor )
                 , IntegerHandling ( ToFloat, ToNormalizedFloat, KeepIntegral )
                 , TexCoord4 ( TexCoord4 )
                 , MatrixMode ( Modelview, Texture, Projection )
                 , Normal3 ( Normal3 )
                 , BlendingFactor ( One, OneMinusSrcAlpha, SrcAlpha )
                 , PixelInternalFormat ( RGBA'
                                       , RGBA8
                                       , Luminance'
                                       , RGB' -- GL_RGB
                                       )
                 , TextureObject
                 , shaderSourceBS
                 , renderQuadric
                 , shaderInfoLog
                 , programInfoLog
                 , shaderCompiler
                 , multMatrix
                 , currentProgram
                 , clear
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
                 , textureResident
                 , bufferData
                 , project
                 , unProject
                 , vertexAttribArray
                 , texture
                 , lookAt
                 , clearDepth
                 , withMatrix
                 , withNewMatrix
                 , rect
                 , clearColor )

import           Graphics.SGCDemo.Draw ( pushVertices
                                       , pushColors
                                       , rectangle
                                       , triangle
                                       , sphere
                                       , pushTexCoords
                                       , pushNormals
                                       , lineStroke
                                       , rectangleStroke
                                       )

import           Graphics.SGCDemo.Coords ( vec3
                                         , vec4
                                         , vec3gld
                                         , invertMajor'
                                         , transposeInvert
                                         , dumpDMX
                                         , ver3
                                         , ver3gld
                                         , unProjectItF
                                         , rotateX
                                         , rotateY
                                         , rotateZ
                                         , verple3
                                         , invertMatrix
                                         , identityMatrix
                                         , multMatrices
                                         , vecl4 )

import           Graphics.SGCDemo.Shader ( uniform
                                         , getShadersInline
                                         , useShaderM
                                         , activateTexture
                                         , useShader
                                         , attrib )

import           Graphics.SGCDemo.Config ( useGLES )

import           Graphics.SGCDemo.Util ( checkSDLError
                                       , checkGLErrors
                                       , glTrueF
                                       , glFalseF
                                       , replaceModel
                                       , appReplaceModel
                                       , color
                                       , up3fst
                                       , up3snd
                                       , up3thd
                                       , col8
                                       , color4
                                       , color3
                                       , stackPush'
                                       , replicateX
                                       , map3
                                       , stackPop'
                                       , stackReplace'
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
                                       , wrapGL
                                       , wrapGLStart
                                       , wrapGLEnd )

import           Graphics.SGCDemo.Coords ( toMGC
                                         , translateX
                                         , translateY
                                         , translateZ
                                         , ver3
                                         , ver4
                                         , vec3 )

import           Graphics.SGCDemo.Types ( App (App)
                                        , Log (Log, info, warn, err)
                                        , Logger
                                        , DMat
                                        , GraphicsData (GraphicsSingle, GraphicsSingleCairo, GraphicsMoving)
                                        , Shader (ShaderC, ShaderT)
                                        , ShaderD (ShaderDC, ShaderDT)
                                        , Tex (NoTexture)
                                        , Flipper (NotFlipping, Flipping)
                                        , FlipDirection (FlipAscending, FlipDescending)
                                        , FlipHemisphere (FlipUpper, FlipLower)
                                        , GraphicsTextureMapping (GraphicsTextureMapping)
                                        , Shader' (Shader')
                                        , VertexData (VertexDataC, VertexDataT)
                                        , appLog
                                        , appMatrix
                                        , shaderProgram
                                        , shaderExtra
                                        , shaderVertexData
                                        , isFlipping
                                        , tex565GLPixelData
                                        , tex565RawPixelData
                                        , tex8888GLPixelData
                                        , tex8888RawPixelData
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
                                        , isTex8888_Luminance
                                        , isTex565
                                        , isTex8888_565
                                        , flHemisphere
                                        , flDirection
                                        , flAngleDeg
                                        , flTick )

    -- • to draw a face: line up the bottom edge with the x-axis, with the
    --   y-axis bisecting it.
    -- • rotate it around x to get the flipping effect.
    --   x = 0: normal, x = 180: inside-out
    -- • then translate y and z by 0.5 units each (in world coordinates) to
    --   get it where it should be.
    -- • multiple transformations happen in fixed world coordinates,
    --   backwards, or in object local coordinates, forwards, depending on how
    --   you want to look at it.

drawCubeFaces :: App -> (Shader, Shader) -> Float -> Flipper -> Int -> [TextureObject] -> [String] -> IO ()
drawCubeFaces app shaders dimension flipper t texNames args = do
    let log             = appLog app
        info'           = info log
        (model'', _, _) = appMatrix app
        dim             = dimension

        (colorShader, textureShader) = shaders

        prog         = shaderProgram textureShader
        VertexDataT vp vt vn _ = shaderVertexData textureShader

        model'          = model'' & stackPop'
        hemi'           = flHemisphere flipper
        ns1'            = [0,  4,  8,  12]
        ns2'            = [16, 20, 24, 28]
        px1             = dim
        nx1             = inv px1
        py1             = 0 + 2 * dim
        frontFace       = [ ( nx1 , 0   , 0 )
                          , ( nx1 , py1 , 0 )
                          , ( px1 , py1 , 0 )
                          , ( px1 , 0   , 0 ) ]

    useShader log prog

    let c1 = concat . replicate 4 $ texCoords hemi' 0 -- 16 vertices (4 faces)
        c2 = concat . replicate 4 $ texCoords hemi' 1 -- 16 vertices (4 faces)
        -- n1 = replicateX 32 $ vec3 0 0 1.0
        n1 = replicateX 32 $ vec4 0 0 1.0 1.0

    vPtr  <- pushVertices log vp  . concat . replicate 8 $ frontFace -- 32 vertices
    tcPtr <- pushTexCoords log vt $ c1 <> c2 -- 32 vertices
    nPtr <- pushNormals log vn n1

    attrib log "vp" vp Enabled
    attrib log "vt" vt Enabled
    attrib log "vn" vn Enabled

    mapM_ (drawFace app shaders) $ zip3 (drop 4 texNames) (faceTransformsInner model' flipper dim) ns2'
    mapM_ (drawFace app shaders) $ zip3 (take 4 texNames) (faceTransformsOuter model' flipper dim) ns1'

    attrib log "vn" vn Disabled
    attrib log "vt" vt Disabled
    attrib log "vp" vp Disabled

    free vPtr
    free tcPtr
    free nPtr

    mapM_ (drawBorder app shaders dim False) (faceTransformsOuter model' flipper dim)

texCoords = texCoords' where
    texCoords' FlipUpper 0 = [ tx01, tx00, tx10, tx11 ]
    texCoords' FlipUpper 1 = [ tx10, tx11, tx01, tx00 ]
    texCoords' FlipLower 0 = [ tx10, tx11, tx01, tx00 ]
    texCoords' FlipLower 1 = [ tx01, tx00, tx10, tx11 ]

    -- 3rd coord is often ignored.
    tx01 = (0, 1, 0, 1)
    tx00 = (0, 0, 0, 1)
    tx10 = (1, 0, 0, 1)
    tx11 = (1, 1, 0, 1)

rotateAndNudge :: Bool -> FlipHemisphere -> String -> DMat -> DMat
rotateAndNudge isInner hemi which model
  | which == "left" = multMatrices  [rl, nudge "left", model]
  | which == "right" = multMatrices [rr, nudge "right", model]
  | which == "front" = multMatrices [rf, nudge "front", model]
  | which == "back" = multMatrices  [rb, nudge "back", model]
  | otherwise = error "no match"
        where nudge = nudgeTowardsInner isInner hemi
              rl = rotateY 90
              rr = rotateY $ inv 90
              rf = rotateY 180
              rb = identityMatrix

nudgeTowardsInner isInner hemi which = nudgeTowardsInner' isInner which where
    nudgeTowardsInner' True "left"  = translateX       $ nudge'
    nudgeTowardsInner' True "right" = translateX . inv $ nudge'
    nudgeTowardsInner' True "front" = translateZ . inv $ nudge'
    nudgeTowardsInner' True "back"  = translateZ       $ nudge'
    nudgeTowardsInner' False _      = identityMatrix

    nudge' | hemi == FlipUpper = nudgeAmount'
           | otherwise         = inv nudgeAmount'
    nudgeAmount' = 0.002

-- assuming that the numbering of TextureObject and TextureUnit follow a
-- predictable pattern -- seems to work.
-- passTextureSamplerToShader log texName utt = do
--     let TextureObject id' = texName
--     uniform log "utt" utt $ TextureUnit (id' - 1)

-- color-specific.
strokeRect app dim progc umc uvc upc avc acc anc = rectangleStroke
    app (ShaderDC (Just progc) umc uvc upc avc acc anc)
    (rectc, rectc, rectc, rectc)
    (v1, v2, v3, v4) thickness' where

    v1 = ver3 (inv dim) 0         rectz
    v2 = ver3 (inv dim) (2 * dim) rectz
    v3 = ver3 dim       (2 * dim) rectz
    v4 = ver3 dim       0         rectz
    rectc           = color 0 0 0 255
    rectz           = inv 0.001
    thickness'      = 0.03

-- vertices have already been pushed.
drawFace app shaders (texName, model, n) = do
    let log = appLog app
        appmatrix = appMatrix app
        ( colorShader, textureShader ) = shaders
        ShaderT (Shader' progt (umt, uvt, upt) (VertexDataT _ _ _ utt) _) = textureShader
        extra        = shaderExtra textureShader

        -- achtung, run-time errors
        (transposeInvertModelUniform':doVaryOpacityUniform':_) = fst . fromJust $ extra

        utimt = transposeInvertModelUniform'
        udvot = doVaryOpacityUniform'

    when (not useGLES) $ do
        -- seems to give invalid enum errors on GLES.
        -- this can lie by the way (false positive)
        isResident <- STV.get . wrapGL log "texture resident" . textureResident $ Texture2D
        unless isResident $ printf "texture %s is not resident" (show texName)

    -- note: has already been applied to cur model (else rotations etc wouldn't
    -- work)
    -- passMatricesToShader log (appmatrix & replaceModel model) umt uvt upt utimt
    passMatricesToShader (app & appReplaceModel model) umt uvt upt utimt
    activateTexture log texName utt

    uniform log "udvot" udvot glTrueF

    wrapGL log "draw quad cube face" $ drawQuad n

calculateModel dim model flipper rotateAndNudge' = m3 where
    m1 = multMatrices [ translateY tr', model ]
    m2 = rotateAndNudge' m1
    m3 = multMatrices [ rotateFlip flipper
                      , translateY (inv dim)
                      , translateZ (inv dim)
                      , m2 ]
    -- translate to compensate for flipping effect
    tr' = (/ 180) $ ang flipper hemi'
    hemi' = flHemisphere flipper

rotateFlip flipper = rotateX ang' where
    ang' = inv $ ang flipper hemi'
    hemi' = flHemisphere flipper

ang flipper hemi = if isFlipping' then flAngleDeg flipper
                                   else if hemi == FlipUpper then 0
                                                             else 180 where
        isFlipping'                = isFlipping flipper

xang flipper hemi = ang' where
    ang'  | isFlipping' = flAngleDeg flipper
          | otherwise = ang'' where
    ang'' | hemi == FlipUpper = 0
          | otherwise = 180
    isFlipping' = isFlipping flipper

passMatricesToShader app umt uvt upt utimt = do
    let log = appLog app
        appmatrix = appMatrix app
        (model, view, proj) = map3 stackPop' appmatrix
        transposeInvertModel' = transposeInvert model
        -- nonsense fallback (model itself) for non-invertible matrix, but
        -- should be rare.
        transposeInvertModel = maybe model id transposeInvertModel'
        true = 1 :: GLint

    model' <- toMGC model
    view' <- toMGC view
    proj' <- toMGC proj
    timodel' <- toMGC transposeInvertModel

    uniform log "umt" umt model'
    uniform log "uvt" uvt view'
    uniform log "upt" upt proj'
    uniform log "utimt" utimt timodel'

drawQuad n                = drawArrays TriangleFan n 4

faceTransformsInner model flipper dim = faceTransforms' True model flipper dim
faceTransformsOuter model flipper dim = faceTransforms' False model flipper dim

faceTransforms' isInner model flipper dim = map map' [ "back", "left", "right", "front" ] where
    rotateAndNudge' = rotateAndNudge isInner hemi'
    map' which = calculateModel dim model flipper (rotateAndNudge' which)
    hemi' = flHemisphere flipper

drawBorder app shaders dim isInner model = do
    let log = appLog app
        appmatrix = appMatrix app
        ( colorShader, textureShader ) = shaders
        ShaderC (Shader' progc (umc, uvc, upc) (VertexDataC avc acc anc) _) = colorShader
        (m, v, p) = appmatrix
        appmatrix' = (m & stackReplace' model, v, p)
        app' = app { appMatrix = appmatrix' }

    strokeRect app' dim progc umc uvc upc avc acc anc
