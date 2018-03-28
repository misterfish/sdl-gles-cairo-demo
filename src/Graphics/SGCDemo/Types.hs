{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.SGCDemo.Types
    ( App (App)
    , Logger
    , Log (Log, info, warn, err)
    , Config
    , ConfigWolfFrames (ConfigWolfFramesStr, ConfigWolfFramesNum)
    , Bubble (Bubble)
    , DMat
    , GMat
    , GMatD
    , Color
    , Shader (ShaderC, ShaderT, ShaderM)
    , Shader' (Shader')
    , ShaderD (ShaderDC, ShaderDT)
    , Tex (NoTexture)
    , GraphicsData (GraphicsSingle, GraphicsSingleCairo, GraphicsMoving)
    , Flipper (NotFlipping, Flipping)
    , FlipDirection (FlipAscending, FlipDescending)
    , FlipHemisphere (FlipUpper, FlipLower)
    , GraphicsTextureMapping (GraphicsTextureMapping)
    , DrawInfo (DrawVertex, DrawColor, DrawTexCoord, DrawNormal)
    , MVPConfig (MVPConfig)
    , ProjectionType (ProjectionFrustum, ProjectionOrtho)
    , VertexData (VertexDataC, VertexDataT, VertexDataM)
    , drawInfoAttribLocation
    , drawInfoVertexCoords
    , drawInfoColorCoords
    , drawInfoTexCoords
    , drawInfoNormal
    , appLog
    , appMatrix
    , appUser1
    , appUser2
    , appUser3
--     , isShaderC
--     , isShaderT
    , shaderMatrix
    , shaderProgram
    , shaderVertexData
    , bubbleXVelocity
    , bubbleYVelocity
    , bubbleXPos
    , bubbleYPos
    , bubbleRadius
    , bubbleColor
    , bubbleLineWidth
    , texWidth
    , texHeight
    , shaderExtra
    , shaderDCProgram
    , shaderDCUniformModel
    , shaderDCUniformView
    , shaderDCUniformProjection
    , shaderDCAttributePosition
    , shaderDCAttributeNormal
    , shaderDCAttributeColors
    , shaderDTProgram
    , shaderDTUniformModel
    , shaderDTUniformView
    , shaderDTUniformProjection
    , shaderDTUniformTexture
    , shaderDTAttributePosition
    , shaderDTAttributeNormal
    , shaderDTAttributeTexCoord
    , tex565GLPixelData
    , tex565RawPixelData
    , tex8888GLPixelData
    , tex8888RawPixelData
--     , tex8888_LuminanceGLPixelData
--     , tex8888_LuminanceRawPixelData
    , tex8888_565GLPixelData565
    , tex8888_565RawPixelData565
    , tex8888_565GLPixelData8888
    , tex8888_565RawPixelData8888
    , tex8888_565CairoSurf
    , graphicsTextureMappingGraphicsData
    , graphicsTextureMappingTexture
    , graphicsTextureMappingTextureObject
    , graphicsSingleImage
    , graphicsSingleDirty
    , graphicsSingleCairoImage
    , graphicsSingleCairoFrames
    , graphicsMovingImages
    , getCSurf
    , newTex565
    , newTex8888WithCairo
    , newTex8888NoCairo
    -- , newTex8888_LuminanceNoCairo
    , newTex8888_565WithCairo
    , isTex8888
--     , isTex8888_Luminance
    , isTex565
    , isTex8888_565
    , flHemisphere
    , flDirection
    , flAngleDeg
    , flAngleDegPrev
    , flTick
    , mvpConfigProjectionType
    , mvpConfigTranslateZ
    , mvpConfigCubeScale
    , isFlipping
    , configDoWolf
    , configDoInitRotate
    , configDoBorders
    , configDoCube
    , configDoCylinder
    , configDoCarrousel
    , configDoTorus
    , configDoBackground
    , configDoTransformTest
    , configDoLinesTest
    , configDoShadersTest
    , configDoStars
    , configWolfFrames
    ) where

import           Text.Printf ( printf )
import           Data.Word (Word8)
import           Foreign.C.Types ( CUChar, CUShort, CInt )
import           Control.Applicative ( (<|>) )

import           Data.Yaml as Y ( (.:)
                                , FromJSON
                                , parseJSON
                                )
import qualified Data.Yaml as Y ( Value (Object)
                                , Parser )

import           Codec.Picture      as JP ( DynamicImage )
import qualified Graphics.Rendering.Cairo as C
                 ( Format (FormatARGB32, FormatA8, FormatRGB24)
                 , Surface
                 , Render
                 , createImageSurfaceForData
                 )

import "matrix"  Data.Matrix        as DMX ( Matrix )
import           Data.Stack ( Stack )
import           Graphics.Rendering.OpenGL as GL
                 ( PixelData ( PixelData )
                 , GLmatrix
                 , GLfloat
                 , GLdouble
                 , Vertex2
                 , Vertex3
                 , Vertex4
                 , Vector4
                 , Program
                 , UniformLocation
                 , AttribLocation
                 , PixelFormat ( BGR, RGB, RGBA, BGRA,
                                 Luminance -- GL_LUMINANCE
                               )
                 , DataType ( Double, UnsignedByte, Float, UnsignedShort565 )
                 , PixelInternalFormat ( RGBA'
                                       , RGBA8
                                       , RGB' -- GL_RGB
                                       )
                 , TextureObject )

import           Foreign ( ForeignPtr
                         , Ptr
                         , Int32
                         , mallocArray )

type DMat   = DMX.Matrix Float
type GMat   = GLmatrix GLfloat
type GMatD  = GLmatrix GLdouble
type Logger = String -> IO ()

data Log = Log { info :: Logger
               , warn :: Logger
               , err :: Logger }

data App = App { appLog :: Log
                 -- we store DMX matrices, which are easier to work with,
                 -- multiply etc. they need to be converted to GLmatrix
                 -- using toMGC before sending to shaders.
               , appMatrix :: ( Stack (Matrix Float)
                              , Stack (Matrix Float)
                              , Stack (Matrix Float) )

               -- useful for testing & debuggin
               , appUser1 :: [String]
               , appUser2 :: [String]
               , appUser3 :: [String] }

data MVPConfig = MVPConfig { mvpConfigProjectionType :: ProjectionType
                           , mvpConfigTranslateZ :: Float
                           , mvpConfigCubeScale :: Float }

data ProjectionType = ProjectionFrustum
                    | ProjectionOrtho

data VertexData = VertexDataC AttribLocation AttribLocation AttribLocation
                | VertexDataT AttribLocation AttribLocation AttribLocation UniformLocation
                | VertexDataM AttribLocation AttribLocation AttribLocation AttribLocation AttribLocation AttribLocation AttribLocation UniformLocation UniformLocation UniformLocation
                  deriving (Eq, Show)

data Shader = ShaderC Shader'
            | ShaderT Shader'
            | ShaderM Shader'
              deriving (Eq, Show)

data Shader' = Shader' { shader'Program    :: Program
                       , shader'Matrix     :: ( UniformLocation, UniformLocation, UniformLocation )
                       , shader'VertexData :: VertexData
                       , shader'Extra      :: Maybe ([UniformLocation], [AttribLocation]) }
               deriving (Eq, Show)

-- isShaderC shader@(ShaderC _ ) = True
-- isShaderC _                   = False
-- isShaderT shader@(ShaderT _ ) = True
-- isShaderT _                   = False

shader' (ShaderC x) = x
shader' (ShaderT x) = x
shader' (ShaderM x) = x

shaderProgram    = shader'Program    . shader'
shaderVertexData = shader'VertexData . shader'
shaderMatrix     = shader'Matrix     . shader'
shaderExtra      = shader'Extra      . shader'

-- | shader structures for sending to primitive drawing routines, intended
-- as a de-coupling from our main Shader type.
-- set program to Nothing if you don't want the lower-level routine to 'use'
-- it.

data ShaderD = ShaderDC { shaderDCProgram           :: Maybe Program
                        , shaderDCUniformModel      :: UniformLocation
                        , shaderDCUniformView       :: UniformLocation
                        , shaderDCUniformProjection :: UniformLocation
                        , shaderDCAttributePosition :: AttribLocation
                        , shaderDCAttributeColors   :: AttribLocation
                        , shaderDCAttributeNormal   :: AttribLocation }
             | ShaderDT { shaderDTProgram           :: Maybe Program
                        , shaderDTUniformModel      :: UniformLocation
                        , shaderDTUniformView       :: UniformLocation
                        , shaderDTUniformProjection :: UniformLocation
                        , shaderDTUniformTexture    :: UniformLocation
                        , shaderDTAttributePosition :: AttribLocation
                        , shaderDTAttributeTexCoord :: AttribLocation
                        , shaderDTAttributeNormal   :: AttribLocation }

         -- draw directly to 565 using JP (not for cairo).
data Tex = Tex565 { tex565GLPixelData :: PixelData CUShort
                  , tex565RawPixelData :: Ptr CUShort
                  , tex565Width :: Int
                  , tex565Height :: Int }
         -- draw to 8888 using JP and/or cairo (not for mobile).
         | Tex8888 { tex8888GLPixelData :: PixelData CUChar
                   , tex8888RawPixelData :: Ptr CUChar
                   , tex8888CairoSurf :: Maybe C.Surface
                   , tex8888Width :: Int
                   , tex8888Height :: Int }

         -- draw to 8888 using JP and/or cairo (not for mobile).
--          | Tex8888_Luminance { tex8888_LuminanceGLPixelData :: PixelData CUChar
--                        , tex8888_LuminanceRawPixelData :: Ptr CUChar
--                        , tex8888_LuminanceCairoSurf :: Maybe C.Surface }

         -- draw to 8888 using JP and/or cairo and copy to 565
         | Tex8888_565 { tex8888_565GLPixelData565 :: PixelData CUShort
                       , tex8888_565RawPixelData565 :: Ptr CUShort
                       , tex8888_565GLPixelData8888 :: PixelData CUChar
                       , tex8888_565RawPixelData8888 :: Ptr CUChar
                       , tex8888_565CairoSurf :: Maybe C.Surface
                       , tex8888_565Width :: Int
                       , tex8888_565Height :: Int }
         | NoTexture

texWidth tex@(Tex565 _ _ _ _) = tex565Width tex
texWidth tex@(Tex8888 _ _ _ _ _) = tex8888Width tex
texWidth tex@(Tex8888_565 _ _ _ _ _ _ _) = tex8888_565Width tex

texHeight tex@(Tex565 _ _ _ _) = tex565Height tex
texHeight tex@(Tex8888 _ _ _ _ _) = tex8888Height tex
texHeight tex@(Tex8888_565 _ _ _ _ _ _ _) = tex8888_565Height tex

type Color = (Float, Float, Float, Float)

-- These contain the image data and optionally the cairo frames and movie
-- timings.
-- They get coupled to a Tex (our structure which holds the texture arrays)
-- and a TextureObject (GL's "texture name" integer).

data GraphicsData
      -- single image, no cairo.
    = GraphicsSingle      { graphicsSingleImage       :: JP.DynamicImage
                          , graphicsSingleDirty       :: Bool }
      -- single image or no image, and cairo.
    | GraphicsSingleCairo { graphicsSingleCairoImage  :: Maybe JP.DynamicImage
                          , graphicsSingleCairoFrames :: [C.Render ()] }
      -- moving images, no cairo.
    | GraphicsMoving      { graphicsMovingImages      :: [JP.DynamicImage]
                          , graphicsMovingTicksPerImageFrame :: Int
                          , graphicsMovingTicksElapsed :: Int }
--       -- moving images, and cairo.
--     | GraphicsMovingCairo { graphicsMovingCairoImage  :: [JP.DynamicImage]
--                           , graphicsMovingCairoFrames :: [C.Render ()]
--                           , graphicsMovingCairoTicksPerImageFrame :: Int
--                           , graphicsMovingCairoTicksPerCairoFrame :: Int }

data GraphicsTextureMapping = GraphicsTextureMapping
    { graphicsTextureMappingGraphicsData :: GraphicsData
    , graphicsTextureMappingTexture :: Tex
    , graphicsTextureMappingTextureObject :: TextureObject }

data Flipper        = NotFlipping { flHemisphere   :: FlipHemisphere }
                    | Flipping    { flHemisphere   :: FlipHemisphere
                                  , flDirection    :: FlipDirection
                                  , flAngleDeg     :: Float
                                  , flAngleDegPrev :: Float
                                  , flTick         :: Int }
                      deriving Show

data FlipDirection  = FlipAscending
                    | FlipDescending
                      deriving (Show, Eq)

data FlipHemisphere = FlipUpper
                    | FlipLower
                      deriving (Show, Eq)

data DrawInfo       = DrawVertex   AttribLocation [Vertex3 Float]
                    | DrawColor    AttribLocation [Color]
                    | DrawTexCoord AttribLocation [Vertex4 Float]
                    | DrawNormal   AttribLocation [Vector4 Float]

drawInfoAttribLocation (DrawVertex   al _) = al
drawInfoAttribLocation (DrawColor    al _) = al
drawInfoAttribLocation (DrawTexCoord al _) = al
drawInfoAttribLocation (DrawNormal   al _) = al

drawInfoVertexCoords   (DrawVertex   _ c) = c
drawInfoColorCoords    (DrawColor    _ c) = c
drawInfoTexCoords      (DrawTexCoord _ c) = c
drawInfoNormal         (DrawNormal   _ c) = c

getCSurf (Tex565 _ _ _ _) = Nothing
getCSurf (Tex8888 _ _ Nothing _ _) = Nothing
getCSurf (Tex8888 _ _ (Just x) _ _) = Just x
-- getCSurf (Tex8888_Luminance _ _ Nothing) = Nothing
-- getCSurf (Tex8888_Luminance _ _ (Just x)) = Just x
getCSurf (Tex8888_565 _ _ _ _ Nothing _ _) = Nothing
getCSurf (Tex8888_565 _ _ _ _ (Just x) _ _) = Just x
getCSurf NoTexture = Nothing

newTex565 w h = do
    rawPixelData <- mallocArray $ w * h :: IO (Ptr CUShort)
    let glPixelData = PixelData GL.RGB GL.UnsignedShort565 rawPixelData
    pure $ Tex565 glPixelData rawPixelData w h

newTex8888WithCairo w h = do
    rawPixelData <- mallocArray $ w * h * 4 :: IO (Ptr CUChar)
    -- rawPixelData <- mallocArray $ w * h * 3 :: IO (Ptr CUChar)
    let glPixelData = PixelData GL.RGBA GL.UnsignedByte rawPixelData
        -- let glPixelData = PixelData GL.RGB GL.UnsignedByte rawPixelData
    cSurf' <- C.createImageSurfaceForData rawPixelData C.FormatARGB32 w h $ w * 4
    -- print "here"
    -- cSurf' <- C.createImageSurfaceForData rawPixelData C.FormatRGB24 w h $ w * 3
    -- print "there"
    pure $ Tex8888 glPixelData rawPixelData (Just cSurf') w h

newTex8888NoCairo w h = do
    rawPixelData <- mallocArray $ w * h * 4 :: IO (Ptr CUChar)
    -- rawPixelData <- mallocArray $ w * h * 3 :: IO (Ptr CUChar)
    -- let glPixelData = PixelData GL.RGB GL.UnsignedByte rawPixelData

    let glPixelData = PixelData GL.RGBA GL.UnsignedByte rawPixelData
        -- let glPixelData = PixelData GL.BGRA GL.UnsignedByte rawPixelData
    pure $ Tex8888 glPixelData rawPixelData Nothing w h


-- newTex8888_LuminanceWithCairo w h = undefined
-- newTex8888_LuminanceNoCairo w h = do
--     rawPixelData <- mallocArray $ w * h  :: IO (Ptr CUChar)
--     let glPixelData = PixelData GL.Luminance GL.UnsignedByte rawPixelData
--     pure $ Tex8888_Luminance glPixelData rawPixelData Nothing
--
-- newTex8888_565NoCairo is technically possible but not implemented because
-- it doesn't seem useful currently.

newTex8888_565WithCairo w h = do
    t565 <- newTex565 w h
    t8888 <- newTex8888WithCairo w h
    let a = tex565GLPixelData t565
        b = tex565RawPixelData t565
        c = tex8888GLPixelData t8888
        d = tex8888RawPixelData t8888
        e = tex8888CairoSurf t8888
    pure $ Tex8888_565 a b c d e w h

isTex8888 (Tex8888 _ _ _ _ _) = True
isTex8888 _ = False

-- isTex8888_Luminance (Tex8888_Luminance _ _ _) = True
-- isTex8888_Luminance _ = False

isTex565 (Tex565 _ _ _ _) = True
isTex565 _ = False

isTex8888_565 (Tex8888_565 _ _ _ _ _ _ _) = True
isTex8888_565 _ = False

isFlipping (NotFlipping _) = False
isFlipping _ = True

data Bubble = Bubble { bubbleXVelocity :: Double
                     , bubbleYVelocity :: Double
                     , bubbleXPos      :: Double
                     , bubbleYPos      :: Double
                     , bubbleRadius    :: Double
                     , bubbleColor     :: ( Double, Double, Double, Double )
                     , bubbleLineWidth :: Double }

data Config = Config { configDoWolf :: Bool
                     , configWolfFrames :: ConfigWolfFrames
                     , configDoInitRotate :: Bool
                     , configDoBorders :: Bool
                     , configDoCube :: Bool
                     , configDoCylinder :: Bool
                     , configDoCarrousel :: Bool
                     , configDoStars :: Bool
                     , configDoTorus :: Bool
                     , configDoBackground :: Bool
                     , configDoTransformTest :: Bool
                     , configDoLinesTest :: Bool
                     , configDoShadersTest :: Bool }

data ConfigWolfFrames = ConfigWolfFramesStr String
                      | ConfigWolfFramesNum Int

instance FromJSON Config where
    parseJSON (Y.Object v) = Config
        <$> v .: "doWolf"
        <*> (ConfigWolfFramesStr <$> v .: "wolfFrames" <|> ConfigWolfFramesNum <$> v .: "wolfFrames")
        <*> v .: "doInitRotate"
        <*> v .: "doBorders"
        <*> v .: "doCube"
        <*> v .: "doCylinder"
        <*> v .: "doCarrousel"
        <*> v .: "doStars"
        <*> v .: "doTorus"
        <*> v .: "doBackground"
        <*> v .: "doTransformTest"
        <*> v .: "doLinesTest"
        <*> v .: "doShadersTest"
    parseJSON _ = error "invalid type for parseJSON Config"
