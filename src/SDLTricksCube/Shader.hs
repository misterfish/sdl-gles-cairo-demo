module SDLTricksCube.Shader ( uniform
                            , uniformsMatrix
                            , uniformsMatrixD
                            , attrib
                            , useShader
                            , getShadersInline
                            , getShadersFilesystem
                            , initShaderColor
                            , initShaderTexture
                            , activateTexture
                            , useShaderM ) where

import           Prelude hiding ( log )

import           Text.Printf ( printf )
import           Data.Maybe ( isNothing, isJust, fromJust )
import qualified Data.ByteString.Char8  as BS8 ( pack )
import           Control.Monad ( (>=>), (<=<), unless, guard, when, forM_, forM )
import           Data.Monoid ( (<>) )
import           Data.StateVar        as STV ( ($=) )
import qualified Data.StateVar        as STV ( get )

import qualified Graphics.Rendering.OpenGL as GL
                 ( uniform )

import           Graphics.Rendering.OpenGL as GL
                 ( Capability ( Enabled, Disabled )
                 , ShaderType ( VertexShader, FragmentShader )
                 , TextureObject (TextureObject)
                 , TextureUnit (TextureUnit)
                 , TextureTarget2D ( Texture2D )
                 , AttribLocation
                 , UniformLocation
                 , shaderSourceBS
                 , activeTexture
                 , textureBinding
                 , compileShader
                 , shaderInfoLog
                 , createProgram
                 , compileStatus
                 , attachShader
                 , shaderCompiler
                 , linkProgram
                 , linkStatus
                 , programInfoLog
                 , attribLocation
                 , uniformLocation
                 , createShader
                 , currentProgram
                 , vertexAttribArray )

import           SDLTricksCube.Coords ( toMGC )
import           SDLTricksCube.Util ( wrapGL
                                    , stackPop'
                                    , mapM3 )

import           SDLTricksCube.Config ( doDebug )

import           SDLTricksCube.Types ( Log ( Log, info, warn, err )
                                     , Shader ( ShaderC, ShaderT )
                                     , Shader' (Shader')
                                     , ShaderD ( ShaderDT, ShaderDC )
                                     , VertexData (VertexDataC, VertexDataT)
                                     , appLog
                                     , shaderMatrix
                                     , appMatrix )

uniformsMatrix log tag shader = uniformsMatrix' log tag um uv up where
    (um, uv, up) = shaderMatrix shader

uniformsMatrix' log tag um uv up app = do
    (model, view, proj) <- mapM3 (toMGC . stackPop') appmatrix
    uniform' um model
    uniform' uv view
    uniform' up proj where
        uniform' = uniform log tag
        appmatrix = appMatrix app

uniformsMatrixD = uniformsMatrixD' where
    uniformsMatrixD' log tag (ShaderDC _ um uv up _ _ _) app = uniformsMatrix' log tag um uv up app
    uniformsMatrixD' log tag (ShaderDT _ um uv up _ _ _ _) app = uniformsMatrix' log tag um uv up app

uniform log tag unif val = wrapGL log tag $ GL.uniform unif $= val
attrib  log tag attr state = wrapGL log str' $ vertexAttribArray attr $= state where
    str' | state == Enabled = "enable vertexAttribArray "  <> tag
         | otherwise        = "disable vertexAttribArray " <> tag

useShader log prog = wrapGL log "use shader" $ currentProgram $= Just prog

useShaderM log (Just prog) = useShader log prog
useShaderM log Nothing     = pure ()

initProgram log vShaderSrc fShaderSrc = do
    let err' = err log
        info' = info log
        info'' "" = pure ()
        info'' x = info' x

    canCompile <- wrapGL log "get shaderCompiler" shaderCompiler
    unless canCompile $ err' "Compiling shaders not supported!"
    guard canCompile

    vShader <- wrapGL log "createShader" $ createShader VertexShader
    wrapGL log "shaderSourceBS" $ shaderSourceBS vShader $= vShaderSrc
    wrapGL log "compileShader" $ compileShader vShader
    vcStatus <- STV.get . wrapGL log "compileStatus" $ compileStatus vShader
    info'' =<< STV.get (shaderInfoLog vShader)
    guard vcStatus

    fShader <- wrapGL log "createShader" $ createShader FragmentShader
    wrapGL log "shaderSourceBS" $ shaderSourceBS fShader $= fShaderSrc
    wrapGL log "compileShader" $ compileShader fShader
    fcStatus <- STV.get . wrapGL log "compileStatus" $ compileStatus fShader
    info'' =<< STV.get (shaderInfoLog fShader)
    guard fcStatus

    prog <- wrapGL log "createProgram" createProgram
    wrapGL log "attach vShader" $ attachShader prog vShader
    wrapGL log "attach fShader" $ attachShader prog fShader

    wrapGL log "linkProgram" $ linkProgram prog
    lStatus <- wrapGL log "linkStatus" $ linkStatus prog
    info'' =<< STV.get (programInfoLog prog)
    guard lStatus

    info'' =<< STV.get (shaderInfoLog vShader)
    info'' =<< STV.get (shaderInfoLog fShader)
    info'' =<< STV.get (programInfoLog prog)
    pure prog

initShaderColor log vShaderSrc fShaderSrc mvp (vp, vc, vn) extra = do
    shader' <- initShader' log "c" vShaderSrc fShaderSrc mvp (vp, vc, vn) Nothing extra
    pure $ ShaderC shader'

initShaderTexture log vShaderSrc fShaderSrc mvp (vp, vc, vn, utt) extra = do
    shader' <- initShader' log "t" vShaderSrc fShaderSrc mvp (vp, vc, vn) (Just utt) extra
    pure $ ShaderT shader'

initShader' log shaderType vShaderSrc fShaderSrc (um, uv, up) (vp, vc, vn) maybeUtt extra = do
    prog' <- initProgram log vShaderSrc fShaderSrc
    let unif' str  = wrapGL log ("uniformLocation " <> str) . STV.get $ uniformLocation prog' str
        att' str   = wrapGL log ("attribLocation "  <> str) . STV.get $ attribLocation  prog' str
    mvpMat'       <- (,,) <$> unif' um <*> unif' uv <*> unif' up
    let extra' :: Maybe ([String], [String]) -> IO (Maybe ([UniformLocation], [AttribLocation]))
        extra' Nothing = pure Nothing
        extra' (Just (extraU, extraA)) = Just <$> extra'' (mapM unif' extraU) (mapM att' extraA)
        extra'' u' a' = (,) <$> u' <*> a'
    let vertexDataC' = VertexDataC <$> att' vp <*> att' vc <*> att' vn
        vertexDataT' = VertexDataT <$> att' vp <*> att' vc <*> att' vn <*> unif' (fromJust maybeUtt)
        vertexData' = if shaderType == "t" then vertexDataT' else vertexDataC'
    Shader' <$> pure prog' <*> pure mvpMat' <*> vertexData' <*> extra' extra

-- no 'enable' necessary for uniforms.
-- attribs need to be enabled / disabled when drawArrays is called.
-- should 'use' the program in the render loop.

-- | bind the texture, set it to 'active', and pass its sampler to the
-- shader via a uniform.
--
-- assuming that the numbering of TextureObject and TextureUnit follow a
-- predictable pattern -- seems to work.
activateTexture log texName utt = do
    let TextureObject id' = texName
        tag1 = show $ id' - 1
        tag2 = show texName
    -- info log $ printf "texname %s texunit %s" tag2 tag1

    -- both calls are necessary.
    wrapGL log ("set activeTexture " <> tag1) $ activeTexture $= TextureUnit (id' - 1)
    wrapGL log ("textureBinding " <> tag2) $ textureBinding Texture2D $= Just texName

    uniform log ("utt " <> tag1) utt $ TextureUnit (id' - 1)

getShadersFilesystem = do
    vShaderColor   <- BS8.pack <$> readFile "vertex-color"
    fShaderColor   <- BS8.pack <$> readFile "fragment-color"

    vShaderTexture <- BS8.pack <$> readFile "vertex-texture"
    fShaderTexture <- BS8.pack <$> readFile "fragment-texture"

    pure ( vShaderColor, fShaderColor, vShaderTexture, fShaderTexture )

getShadersInline =
    ( BS8.pack getShaderInlineVertexColor
    , BS8.pack getShaderInlineFragmentColor
    , BS8.pack getShaderInlineVertexTexture
    , BS8.pack getShaderInlineFragmentTexture )

getShaderInlineVertexColor =
    "#version 100\n" <>
    "uniform mat4 model;\n" <>
    "uniform mat4 view;\n" <>
    "uniform mat4 projection;\n" <>
    "attribute vec4 a_position;\n" <>
    "attribute vec4 a_color;\n" <>
    "varying vec4 v_color;\n" <>
    "void main()\n" <>
    "{\n" <>
    "   v_color = a_color;\n" <>
    "   gl_Position = projection * view * model * a_position;\n" <>
    "}\n" <>
    "\n"

getShaderInlineVertexTexture =
    "#version 100\n" <>
    "uniform mat4 model;\n" <>
    "uniform mat4 view;\n" <>
    "uniform mat4 projection;\n" <>
    "attribute vec4 a_position;\n" <>
    "vec4 a_normal;\n" <>
    "varying vec4 v_normal;\n" <>
    "attribute vec2 a_texcoord;\n" <>
    "varying vec2 v_texcoord;\n" <>
    "varying vec4 v_position;\n" <>
    "mat4 model4 = mat4(1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0);\n" <>
    "void main()\n" <>
    "{\n" <>
    "    gl_Position = projection * view * model * a_position;\n" <>
    "    v_texcoord = a_texcoord;\n" <>
    "    v_position = vec4(model * a_position);\n" <>
    "    a_normal = vec4(0.0, 1.0, 0.0, 1.0);\n" <>
    "    v_normal = mat4(model4) * a_normal;\n" <>
    "}\n"

getShaderInlineFragmentColor =
    "#version 100\n" <>
    "#ifdef GL_ES\n" <>
    "precision mediump float;\n" <>
    "#endif\n" <>
    "varying vec4 v_color;\n" <>
    "void main()\n" <>
    "{\n" <>
    "   gl_FragColor = v_color;\n" <>
    "}\n"

getShaderInlineFragmentTexture =
    "#version 100\n" <>
    "#ifdef GL_ES\n" <>
    "precision mediump float;\n" <>
    "#endif\n" <>
    "uniform sampler2D texture;\n" <>
    "varying vec2 v_texcoord;\n" <>
    "varying vec4 v_position;\n" <>
    "varying vec4 v_normal;\n" <>
    "float ambientStrength = 0.4;\n" <>
    "float specularStrength = 0.2;\n" <>
    "vec4 lightColor = vec4 (1.0, 0.9, 0.5, 1.0);\n" <>
    "vec4 lightPos = vec4 (0.0, 0.0, 1.0, 1.0);\n" <>
    "vec4 viewPos = vec4 (0.0, 0.0, 1.0, 1.0);\n" <>
    "float fogDensity = 2.5;\n" <>
    "float perspectiveFar = 10.0;\n" <>
    "vec4 fogColor = vec4 (1.0, 0.3, 0.3, 1.0);\n" <>
    "float lightOpacity = 1.0;\n" <>
    "float finalOpacity = 0.8;\n" <>
    "void main()\n" <>
    "{\n" <>
    "    vec4 init = texture2D(texture, v_texcoord);\n" <>
    "    vec4 norm = normalize (v_normal);\n" <>
    "    vec4 lightDir = normalize (lightPos - v_position);\n" <>
    "    vec4 ambient = ambientStrength * lightColor;\n" <>
    "    vec4 diffuse = max (dot (norm, lightDir), 0.0) * lightColor;\n" <>
    "    vec4 viewDir = normalize (viewPos - v_position);\n" <>
    "    vec4 reflectDir = reflect (-lightDir, norm);\n" <>
    "    float spec = pow (max (dot (viewDir, reflectDir), 0.0), 32.0);\n" <>
    "    vec4 specular = specularStrength * spec * lightColor;\n" <>
    "    vec4 lightTotal = ambient + diffuse + specular;\n" <>
    "    lightTotal.x *= lightOpacity;\n" <>
    "    lightTotal.y *= lightOpacity;\n" <>
    "    lightTotal.z *= lightOpacity;\n" <>
    "    gl_FragColor = init * lightTotal;\n" <>
    "    float fragZ = gl_FragCoord.z / gl_FragCoord.w;\n" <>
    "    float fogCoord = fragZ / perspectiveFar;\n" <>
    "    float fog = fogCoord * fogDensity;\n" <>
    "    gl_FragColor = mix (fogColor, gl_FragColor, clamp (1.0 - fog, 0.0, 1.0));\n" <>
    "    gl_FragColor.w = finalOpacity;\n" <>
    "}\n"

debug | doDebug   = info
      | otherwise = const . const . pure $ ()

