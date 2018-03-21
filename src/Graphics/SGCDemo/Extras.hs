-- hlint 'reduce duplication' hints were driving me nuts, hence this module.

module Graphics.SGCDemo.Extras ( doVerticesCube
                               , doVerticesScreen ) where

import qualified Data.StateVar      as STV ( get )
import qualified Data.ByteString.Char8  as BS8 ( pack )

import Graphics.Rendering.OpenGL
    as GL ( PrimitiveMode (Quads, Triangles, TriangleFan, QuadStrip)
          , TexCoord2 (TexCoord2)
          , TexCoord3 (TexCoord3)
          , Normal3 (Normal3)
          , Vertex2 (Vertex2)
          , Vertex3 (Vertex3)
          , Vertex4 (Vertex4)
          , normal
          , vertex
          , texCoord
          , renderPrimitive )

info = undefined
aspectRatio = undefined
debug = undefined
pushColors = undefined
pushVertices = undefined
deg2rad = undefined
drawArrays = undefined

verScaleAmount = 5.0

-- | first block from the web
-- (http://www.informit.com/articles/article.aspx?p=770639&seqNum=4)
--
-- others by following the pattern.
doVerticesCube = renderPrimitive Quads $ do
    -- "-x" face
    texNNP
    verNNP
    texNNN
    verNNN
    texNPN
    verNPN
    texNPP
    verNPP

    texPNP
    verPNP
    texPNN
    verPNN
    texPPN
    verPPN
    texPPP
    verPPP

    texNNP
    verNNP
    texNNN
    verNNN
    texPNN
    verPNN
    texPNP
    verPNP

    texNPP
    verNPP
    texNPN
    verNPN
    texPPN
    verPPN
    texPPP
    verPPP

    texNPN
    verNPN
    texNNN
    verNNN
    texPNN
    verPNN
    texPPN
    verPPN

    texNPP
    verNPP
    texNNP
    verNNP
    texPNP
    verPNP
    texPPP
    verPPP


tex3 a b c = texCoord $ TexCoord3 (float a) (float b) (float c)
texNNN = tex3 (-1) (-1) (-1)
texPNN = tex3 1 (-1) (-1)
texNPN = tex3 (-1) 1 (-1)
texNNP = tex3 (-1) (-1) 1
texPPN = tex3 1 1 (-1)
texPNP = tex3 1 (-1) 1
texNPP = tex3 (-1) 1 1
texPPP = tex3 1 1 1

tex2 a b = texCoord $ TexCoord2 (float a) (float b)
texPN = tex2 1 (-1)
texPP = tex2 1 1
texNP = tex2 (-1) 1
texNN = tex2 (-1) (-1)

-- positive z block of texture coordinates, seems to work.
-- however this is the texture 1 binding so why isn't it 2d?
-- 2d coords don't seem to work at all. xxx
doVerticesScreen bottomForTexture2 t = GL.renderPrimitive GL.Quads $ do
    texNPP
    verNoScale nedge' (bottom' + edge') z'
    texNNP
    verNoScale nedge' bottom' z'
    texPNP
    verNoScale pedge' bottom' z'
    texPPP
    verNoScale pedge' (bottom' + edge') z' where
        z' = min pedge' . (+ nedge') . (* zVelocity') $ t'
        t' = fromIntegral t
        -- zVelocity' = 0.1
        zVelocity' = 0.01
        pedge' = 5
        nedge' = -5
        edge' = 2 * pedge'
        bottom' = pedge' - pedge' * bottomForTexture2

verNPP = ver (-1) 1 1
verPPP = ver 1 1 1
verNPN = ver (-1) 1 (-1)
verNNN = ver (-1) (-1) (-1)
verPPN = ver 1 1 (-1)
verNNP = ver (-1) (-1) 1
verPNN = ver 1 (-1) (-1)
verPNP = ver 1 (-1) 1

float = id :: Float -> Float
nor a b c = normal $ Normal3 (float a) (float b) (float c)
ver a b c = vertex $ Vertex3 (float a * verScaleAmount) (float b * verScaleAmount) (float c * verScaleAmount)

verNoScale a b c = vertex $ Vertex3 (float a) (float b) (float c)
verScale n a b c = vertex $ Vertex3 (float a * n) (float b * n) (float c * n)

{-
-- literal drawing of vertices
cubeOld log ( textureData0, textureData1 ) t = do
    let pz = dim
        nz = inv pz
        px1 = dim
        nx1 = inv px1
        py1 = px1
        ny1 = nx1
        px2 = px1 + shift
        nx2 = nx1 + shift
        py2 = py1 + shift
        ny2 = ny1 + shift
        shift = 0.1
        dim = 0.5

    -- z: nz for front face, pz for back face
    -- apparently positive z is into the screen.
    -- x & y: both have p and n versions, and each of those has two
    -- varieties ('1' for front face (nz), '2' for back face (pz))
    -- for the rest, 1 corresponds to nz, 2 to pz
    let frontFace   = [ (nx1, ny1, nz)
                      , (nx1, py1, nz)
                      , (px1, py1, nz)
                      , (px1, ny1, nz) ]

        backFace    = [ (nx2, ny2, pz)
                      , (nx2, py2, pz)
                      , (px2, py2, pz)
                      , (px2, ny2, pz) ]

        leftFace    = [ (nx1, ny1, nz)
                      , (nx1, py1, nz)
                      , (nx2, py2, pz)
                      , (nx2, ny2, pz) ]

        rightFace   = [ (px1, ny1, nz)
                      , (px1, py1, nz)
                      , (px2, py2, pz)
                      , (px2, ny2, pz) ]

        topFace     = [ (px1, py1, nz)
                      , (nx1, py1, nz)
                      , (nx2, py2, pz)
                      , (px2, py2, pz) ]

        bottomFace  = [ (px1, ny1, nz)
                      , (nx1, ny1, nz)
                      , (nx2, ny2, pz)
                      , (px2, ny2, pz) ]

    pushVertices log . concat $ [ frontFace, leftFace
                                , rightFace, topFace
                                , bottomFace, backFace ]

    pushTexCoords log [ (0, 1), (0, 0), (1, 0), (1, 1)
                      , (0, 1), (0, 0), (1, 0), (1, 1)
                      , (0, 1), (0, 0), (1, 0), (1, 1)
                      , (0, 1), (0, 0), (1, 0), (1, 1)
                      , (0, 1), (0, 0), (1, 0), (1, 1)
                      , (0, 1), (0, 0), (1, 0), (1, 1) ]

    -- GLES can not draw Quads, but TriangleFan is equivalent if sent in
    -- groups of four.
    -- (TriangleFan of vertices 1, 2, 3, 4 makes 2 triangles: 1, 2, 3; 1, 3,
    -- 4).

    let drawas = TriangleFan
        draw' n = drawArrays drawas n 4
        t' = t `mod` 120
        vertBackFace = [20]
        vertRest = [0, 4 .. 16]

    bindAndTransferTexture log textureData0
    mapM_ draw' vertRest

    bindAndTransferTexture log textureData1
    mapM_ draw' vertBackFace

    checkGlErrors log $ "after drawArrays blah"
-}

inv x = (-1) * x

{-

line1 log t = do
    let debug' = debug log
    let px = rcos tr
        nx = inv px
        py = rsin tr * aspectRatio
        ny = inv py
        r = 0.5
        rsin = (*r) . sin
        rcos = (*r) . cos
        t' = t `mod` 360
        td = fromIntegral t' :: Float
        tdeg = 3 * td
        tr = deg2rad tdeg

    debug' "doing colors"
    pushColors log [ (150, 80, 10, 255)
                   , (150, 80, 10, 255) ]

    debug' "doing vertices"
    pushVertices log [ (nx, ny, 0)
                     , (px, py, 0) ]

    debug' "drawArrays"
    drawArrays Lines 0 2
    checkGlErrors log "after drawArrays"

line2 log t = do
    let debug' = debug log
        px = rcos tr
        nx = inv px
        py = rsin tr * aspectRatio
        ny = inv py
        r = 0.5
        rsin = (*r) . sin
        rcos = (*r) . cos
        t' = t `mod` 360
        td = fromIntegral t' :: Float
        tdeg = (-5) * td
        tr = deg2rad tdeg

    pushColors log [ (0, 80, 10, 255)
                   , (0, 80, 10, 255) ]

    debug' "doing vertices"
    pushVertices log [ (nx, ny, 0)
                     , (px, py, 0) ]

    debug' "drawArrays"
    drawArrays Lines 0 2
    checkGlErrors log "after drawArrays"
-}

{-
-- not necessary, and wasn't working right ...
-- model view works fine ...
vertexShader log = do
    canCompile <- shaderCompiler
    -- die don't die?
    when (not canCompile) $ die log "Compiling shaders not supported!"
    guard canCompile
    vShader <- createShader VertexShader
    shaderSourceBS vShader $= sourceBS
    compileShader vShader
    checkGlErrors log "compile vertex shader"
    cStatus <- STV.get $ compileStatus vShader
    when (not cStatus) $ die log "Couldn't compile vertex shader"
    info log =<< (STV.get $ shaderInfoLog vShader)
    guard cStatus

    prog <- createProgram
    attachShader prog vShader
    linkProgram prog
    checkGlErrors log "link shaders"
    lStatus <- linkStatus prog
    when (not lStatus) $ die log "Couldn't link shaders"
    info log =<< (STV.get $ programInfoLog prog)
    guard lStatus

    currentProgram $= Just prog

    pure () where
        sourceBS = BS8.pack source
        -- gl_Position: special output var
        -- mvp_matrix
        source = "/* attribute vec4 vPosition; */ \
                 \void main() \
                 \{ \
                 \  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex; \
                 \}"
-}
