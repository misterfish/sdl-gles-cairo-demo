module GraphicsDemo.Config ( useGLES
                            , isEmbedded
                            , doDebug
                            , doBench
                            , forceEmbedded
                            , openGLESCompatibility
                            , openGLESVer
                            , textureDimension
                            , dimension
                            , frameInterval
                            , viewportWidth
                            , viewportHeight
                            , windowWidth
                            , windowHeight
                            , output565
                            , colorProfile32
                            , mvpConfig
                            , aspectRatio ) where

import           Foreign.C.Types ( CInt )
import           System.Info   as I              ( os, arch )
import           Graphics.Rendering.OpenGL as GL ( GLsizei )

import           GraphicsDemo.Types
                 ( ProjectionType (ProjectionFrustum, ProjectionOrtho)
                 , MVPConfig (MVPConfig) )

useGLES       = isEmbedded || (not useCompatibility)
useCompatibility = False

isEmbedded                = forceEmbedded || I.os == "linux_android"
forceEmbedded             = False

openGLESCompatibility     = False
openGLESVer :: (CInt, CInt)
openGLESVer
  | openGLESCompatibility = (1, 1)
  | otherwise             = (2, 0)

-- 565 is necessary for some mobiles.
output565 = isEmbedded || force565
force565 = True

-- 24 is the default, seems to work for all devices.
colorProfile32            = False

doDebug                   = False
doBench                   = False

textureDimension          = 512
textureWidth              = textureDimension
textureHeight             = textureDimension
dimension                 = 0.5
frameInterval             = 50
viewportWidth             = fromIntegral windowWidth  :: GLsizei
viewportHeight            = fromIntegral windowHeight :: GLsizei
windowWidth               = 480 :: Int
windowHeight              = 800 :: Int

aspectRatio               = fromIntegral viewportWidth / fromIntegral viewportHeight :: Float

mvpConfigF = MVPConfig ProjectionFrustum (- 3) 1.9
mvpConfigO = MVPConfig ProjectionOrtho (- 2) 1
mvpConfig = mvpConfigF

