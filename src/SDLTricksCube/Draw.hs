module SDLTricksCube.Draw ( triangle
                          , sphere
                          , cylinder
                          , cylinderTex
                          , coneSection
                          , coneSectionTex
                          , rectangle
                          , triangleStrip
                          , rectangleStroke
                          , pushVertices
                          , pushTexCoords
                          , pushNormals
                          , pushColors
                          , torus
                          , lineStroke ) where

import           Prelude hiding ( log )

import           Data.Foldable ( find , foldl', foldr )
import           Data.Monoid ( (<>) )
import           Data.Function ( (&) )
import           Text.Printf ( printf )
import           Control.Monad ( (<=<), unless, guard, when, forM_ )
import           Data.Maybe ( isJust, fromJust )
import           Data.Word ( Word8 )
import           Data.StateVar        as STV ( ($=) )
import qualified Data.StateVar        as STV ( get )
import qualified Data.Vector.Storable as DVS ( (!) , Vector , length , fromList )
import           Data.Vector.Storable as DVS ( unsafeWith )
import           Foreign ( Ptr, free, mallocArray, pokeElemOff, pokeArray )

import           Data.Stack ( stackPop )
import           Graphics.Rendering.OpenGL as GL
                 ( Vertex3 ( Vertex3 )
                 , Vertex4 ( Vertex4 )
                 , Vector3 ( Vector3 )
                 , Vector4 ( Vector4 )
                 , TextureObject (TextureObject)
                 , TextureUnit (TextureUnit)
                 , TextureTarget2D ( TextureRectangle, Texture2D )
                 , VertexArrayDescriptor ( VertexArrayDescriptor )
                 , GLfloat
                 , AttribLocation
                 , DataType ( Double, UnsignedByte, Float, UnsignedShort565 )
                 , ClientArrayType
                   ( VertexArray, ColorArray, TextureCoordArray, NormalArray )
                 , PrimitiveMode
                   ( Quads, Points, Triangles, TriangleFan, QuadStrip, Lines, TriangleStrip )
                 , Capability ( Enabled, Disabled )
                 , IntegerHandling ( ToFloat, ToNormalizedFloat, KeepIntegral )
                 , vertexAttribPointer
                 , activeTexture
                 , arrayPointer
                 , textureBinding
                 , vertexAttribArray
                 , drawArrays
                 )

import           SDLTricksCube.Coords ( rotateX
                                      , rotateY
                                      , rotateZ
                                      , scaleX
                                      , scaleY
                                      , scaleZ
                                      , translateX
                                      , translateY
                                      , translateZ
                                      , identityMatrix
                                      , vec4
                                      , multMatrices
                                      , toMGC
                                      , verple3
                                      , ver3
                                      , vec3 )

import           SDLTricksCube.Shader ( uniform
                                      , uniformsMatrix
                                      , uniformsMatrixD
                                      , activateTexture
                                      , attrib
                                      , useShader
                                      , useShaderM )

import           SDLTricksCube.Util ( checkSDLError
                                    , checkGLErrors
                                    , hsvCycle
                                    , color
                                    , stackReplace'
                                    , appMultiplyModel
                                    , map3
                                    , stackPop'
                                    , float
                                    , frint
                                    , concatTuples2
                                    , concatTuples3
                                    , concatTuples4
                                    , concatVectors3
                                    , concatVectors4
                                    , toDeg
                                    , vcross
                                    , inv
                                    , vmag
                                    , vdiv
                                    , v3x
                                    , v3y
                                    , v3z
                                    , vdot
                                    , wrapGL )

import           SDLTricksCube.Config ( doDebug )
import           SDLTricksCube.Types ( Log (Log, info, warn, err)
                                     , App (App)
                                     , Shader (ShaderC, ShaderT)
                                     , ShaderD (ShaderDT, ShaderDC)
                                     , DrawInfo (DrawVertex, DrawColor, DrawTexCoord, DrawNormal)
                                     , appLog
                                     , drawInfoAttribLocation
                                     , drawInfoVertexCoords
                                     , drawInfoColorCoords
                                     , drawInfoTexCoords
                                     , drawInfoNormal
                                     , appMatrix )

-- not really useful apart from testing simple triangles.
triangle log (av, ac, an) (c1, c2, c3) (v1, v2, v3) = do
    let p (Vertex3 x y z) = (x, y, z)

    vPtr <- pushVertices log av [ p v1, p v2, p v3 ]
    cPtr <- pushColors   log ac [ c1, c2, c3 ]

    -- normals are not currently possible here.
    -- if needed, follow the pattern in triangleStrip.
    attrib log "av" av Enabled
    attrib log "ac" ac Enabled
    wrapGL log "drawArrays Triangles" $ drawArrays Triangles 0 3
    attrib log "av" av Disabled
    attrib log "ac" ac Disabled

    free cPtr
    free vPtr

getptrs app data'= do
    let unvertex3 (Vertex3 x y z)    = (x, y, z)
        unvertex4 (Vertex4 x y z w)  = (x, y, z, w)
        getptr' (DrawVertex av vs)   = pushVertices app av $
            map unvertex3 vs
        getptr' (DrawColor ac cs)    = pushColors app ac cs
        getptr' (DrawTexCoord at ts) = pushTexCoords app at $
            map unvertex4 ts
        getptr' (DrawNormal an ns)   = pushNormals app an ns
    mapM getptr' data'

notEqual []     = error "notEqual: empty list"
notEqual [x]  = False
notEqual (x:xs) = isJust . find (x /=) $ xs

triangleStrip log data' = do
    ptrs <- getptrs log data'
    let a v@(DrawVertex _ _)   = length . drawInfoVertexCoords $ v
        a v@(DrawColor _ _)    = length . drawInfoColorCoords  $ v
        a v@(DrawTexCoord _ _) = length . drawInfoTexCoords    $ v
        a v@(DrawNormal _ _)   = length . drawInfoNormal       $ v
        n = frint . a . head $ data'
    when (notEqual . map a $ data') $
        warn log "triangleStrip: unequal lists"
    mapM_ (attribEnable log)  $ zip data' [0 .. ]
    wrapGL log "drawArrays TriangleStrip"     $ drawArrays TriangleStrip 0 n
    mapM_ (attribDisable log) $ zip data' [0 .. ]
    mapM_ free ptrs

triangleStrip' log [] _ = pure []
triangleStrip' log data' partitions = do
    ptrs <- getptrs log data'
    let a v@(DrawVertex _ _)   = length . drawInfoVertexCoords $ v
        a v@(DrawColor _ _)    = length . drawInfoColorCoords  $ v
        a v@(DrawTexCoord _ _) = length . drawInfoTexCoords    $ v
        n = frint . a . head $ data'
    when (notEqual . map a $ data') $
        warn log "triangleStrip: unequal lists"
    let begin = mapM_ (attribEnable log)  $ zip data' [0 .. ]
        middle' Nothing = [wrapGL log "drawArrays TriangleStrip"     $ drawArrays TriangleStrip 0 n]
        middle' (Just m) = flip map m $ \(k, l) ->
            wrapGL log "drawArrays TriangleStrip"     $ drawArrays TriangleStrip k ( l - k  + 1 )
        middle = middle' partitions
        end = do
            mapM_ (attribDisable log) $ zip data' [0 .. ]
            mapM_ free ptrs
    pure $ [begin] <> middle <> [end]

rectangle app (av, ac, an) (c1, c2, c3, c4) (v1, v2, v3, v4) = do
    let log = appLog app
        p (Vertex3 x y z) = (x, y, z)

    vPtr <- pushVertices log av [ p v1, p v2, p v3, p v4 ]
    cPtr <- pushColors log ac [ c1, c2, c3, c4 ]

    attrib log "av" av Enabled
    attrib log "ac" ac Enabled
--     attrib log "an" an Enabled
    wrapGL log "drawArrays TriangleFan" $ drawArrays TriangleFan 0 4
--     attrib log "an" an Disabled
    attrib log "ac" ac Disabled
    attrib log "av" av Disabled

    free vPtr
    free cPtr

rectangleStroke app shader (c1, c2, c3, c4) (v1, v2, v3, v4) thickness = do
    lineStroke app shader (c1, c2) (v1, v2) thickness
    lineStroke app shader (c2, c3) (v2, v3) thickness
    lineStroke app shader (c3, c4) (v3, v4) thickness
    lineStroke app shader (c4, c1) (v4, v1) thickness

lineStroke app shader (c1, c2) (ver1, ver2) thickness = do
    let appmatrix = appMatrix app
        (model, view, proj) = appMatrix app

        dx = verx2 - verx1
        dy = very2 - very1
        dz = verz2 - verz1
        dr = sqrt $ (dx ** 2) + (dy ** 2) + (dz ** 2)
        verx1 = x ver1
        very1 = y ver1
        verz1 = z ver1
        verx2 = x ver2
        very2 = y ver2
        verz2 = z ver2
        x (Vertex3 x' _ _ ) = x'
        y (Vertex3 _ y' _ ) = y'
        z (Vertex3 _ _ z' ) = z'

        theta = asin $ inv dz / dr
        thetad = toDeg theta
        phi = asin $ dy / dr
        phid = toDeg phi

        rotatey' | dx < 0     = rotateY 180
                 | otherwise  = identityMatrix

        model' = multMatrices [ translateY . (/ inv 2) $ thickness
                              , rotateY thetad
                              , rotateZ phid
                              , rotatey'
                              , translateZ verz1
                              , translateY very1
                              , translateX verx1
                              , model & stackPop' ]
        appmatrix' = (model & stackReplace' model', view, proj)
        app' = app { appMatrix = appmatrix' }

    lineStroke' app' shader (c1, c2) dr thickness

lineStroke' app shader (c1, c2) dr thickness = do
    let log = appLog app
        appmatrix = appMatrix app
        cs' = (c1, c2, c2, c1)
        vs' = ( ver3 0 0 0
              , ver3 dr 0 0
              , ver3 dr thickness 0
              , ver3 0 thickness 0 )
        ShaderDC mp um uv up av ac an = shader
        (model, view, proj) = map3 stackPop' appmatrix

    useShaderM log mp

    uniform log "model" um =<< toMGC model
    uniform log "view"  uv =<< toMGC view
    uniform log "proj"  up =<< toMGC proj

    rectangle app (av, ac, an) cs' vs'

-- note that there is no distinction at this level between normal, texture,
-- and color arrays: they are all just attribute as far as we are concerned,
-- and the shaders give them meaning.
-- it's nice to have different function names, but we could reduce a lot of duplication here.
-- also pushNormals takes Vectors while the rest take tuples xxx

pushColors :: Log -> AttribLocation -> [(Float, Float, Float, Float)] -> IO (Ptr Float)
pushColors log attribLocation tuples = do
    let coords' = concatTuples4 tuples
        numComp' = 4 -- per vertex
        dataType' = GL.Float
        stride' = 0
        len' = length coords'
    tgt' <- mallocArray len' :: IO (Ptr Float)
    let colorArrayDescriptor = VertexArrayDescriptor numComp' dataType' stride' tgt'
    pokeArray tgt' coords'
    wrapGL log "vertexAttribPointer colors" $ vertexAttribPointer attribLocation $= (ToFloat, colorArrayDescriptor)
    pure tgt'

pushVertices :: Log -> AttribLocation -> [(Float, Float, Float)] -> IO (Ptr Float)
pushVertices log attribLocation tuples = do
    let coords' = concatTuples3 tuples
        numComp' = 3 -- per vertex
        dataType' = GL.Float
        stride' = 0
        len' = length coords'
    tgt' <- mallocArray len' :: IO (Ptr Float)
    let vertexArrayDescriptor = VertexArrayDescriptor numComp' dataType' stride' tgt'
    pokeArray tgt' coords'
    wrapGL log "vertexAttribPointer vertices" $ vertexAttribPointer attribLocation $= (ToFloat, vertexArrayDescriptor)
    pure tgt'

pushTexCoords :: Log -> AttribLocation -> [(Float, Float, Float, Float)] -> IO (Ptr Float)
pushTexCoords log attribLocation tuples = do
    let coords' = concatTuples4 tuples
        numComp' = 4 -- per vertex
        dataType' = GL.Float
        stride' = 0
        len' = length coords'
    tgt' <- mallocArray len' :: IO (Ptr Float)
    let texCoordArrayDescriptor = VertexArrayDescriptor numComp' dataType' stride' tgt'
    pokeArray tgt' coords'
    wrapGL log "texCoordAttribPointer vertices" $ vertexAttribPointer attribLocation $= (ToFloat, texCoordArrayDescriptor)
    pure tgt'

pushNormals :: Log -> AttribLocation -> [Vector4 Float] -> IO (Ptr Float)
pushNormals log attribLocation vectors = do
    let coords' = concatVectors4 vectors
        numComp' = 4 -- per vertex
        dataType' = GL.Float
        stride' = 0
        len' = length coords'
    tgt' <- mallocArray len' :: IO (Ptr Float)
    let normalArrayDescriptor = VertexArrayDescriptor numComp' dataType' stride' tgt'
    pokeArray tgt' coords'
    wrapGL log "normalAttribPointer vertices" $ vertexAttribPointer attribLocation $= (ToFloat, normalArrayDescriptor)
    pure tgt'

-- algorithm from StackOverflow.
-- color-specific, no texture mapping.

sphere :: App -> ShaderD -> (Int, Int) -> (Float, Float, Float, Float) -> Float -> IO ()
sphere app shader (slices, stacks) colour r = do
    let log = appLog app
        appmatrix = appMatrix app
        ShaderDC mp um uv up av ac an = shader
        (model, view, proj) = map3 stackPop' appmatrix

    useShaderM log mp

    uniform log "model" um =<< toMGC model
    uniform log "view"  uv =<< toMGC view
    uniform log "proj"  up =<< toMGC proj

    let draw' = sphere' log (av, ac, an) (slices, stacks) colour r
    mapM_ draw' [ (i, j) | i <- [ 0 .. slices - 1 ]
                         , j <- [ 0 .. stacks - 1 ] ]

    pure ()

sphere' log (av, ac, an) (slices, stacks) colour r (i, j) = do
    -- u: [0, 2pi], v: [0, pi]
    let surf' u' v' = ( cos u' * sin v' * r
                      , cos v' * r
                      , sin u' * sin v' * r )
        stepU = 2 * pi / (float . frint $ slices)
        stepV = pi / (float . frint $ stacks)
        if' = frint i
        jf' = frint j
        u = if' * stepU
        v = jf' * stepV
        un = if i == slices - 1 then 2 * pi -- round last one
                                else (if' + 1) * stepU
        vn = if j == stacks - 1 then pi -- round last one
                                else (jf' + 1) * stepV
        p0 = surf' u v
        p1 = surf' u vn
        p2 = surf' un v
        p3 = surf' un vn

    triangle log (av, ac, an) (colour, colour, colour) (verple3 p0, verple3 p2, verple3 p1)
    triangle log (av, ac, an) (colour, colour, colour) (verple3 p3, verple3 p1, verple3 p2)

cylinderTex app shaderT npoints height radius angBegin angEnd texName (tx00, tx01, tx10, tx11) = do
    let log = appLog app
        ShaderDT mp _ _ _ utt av at an = shaderT
        info' = (texName, utt)
        data' vs (Just ts) (Just ns)= [ DrawVertex av vs
                                      , DrawTexCoord at ts
                                      , DrawNormal an ns ]
    useShaderM log mp
    coneSection' app shaderT npoints height radius radius angBegin angEnd data' (Just info')

cylinder app shaderC npoints height radius angBegin angEnd colour = do
    let log = appLog app
        ShaderDC mp _ _ _ av ac _ = shaderC
        cs         = concat . replicate npoints $ [colour]
        data' vs _ _ = [ DrawVertex av vs
                     , DrawColor ac cs ]
    useShaderM log mp
    coneSection' app shaderC npoints height radius radius angBegin angEnd data' Nothing

coneSectionTex app shaderT npoints height topRadius bottomRadius angBegin angEnd texName (tx00, tx01, tx10, tx11) = do
    let log = appLog app
        ShaderDT mp _ _ _ utt av at an = shaderT
        info' = (texName, utt)
        data' vs (Just ts) (Just ns) = [ DrawVertex av vs
                                       , DrawTexCoord at ts
                                       , DrawNormal an ns ]
    useShaderM log mp
    coneSection' app shaderT npoints height topRadius bottomRadius angBegin angEnd data' (Just info')

coneSection app shaderC npoints height topRadius bottomRadius angBegin angEnd colour = do
    let log = appLog app
        ShaderDC mp _ _ _ av ac _ = shaderC
        cs         = concat . replicate npoints $ [colour]
        data' vs _ _ = [ DrawVertex av vs
                       , DrawColor ac cs ]
    useShaderM log mp
    coneSection' app shaderC npoints height topRadius bottomRadius angBegin angEnd data' Nothing

-- ~ 60 points looks alright.
coneSection' app shaderD npoints height topRadius bottomRadius angBegin angEnd data' texInfo = do
    let log         = appLog app
        app'        = app & appMultiplyModel model'
        model'      = multMatrices [ translateY $ inv 0.5
                                   , scaleY height
                                   , scaleX topRadius
                                   , scaleZ topRadius ]
        npoints'    | odd npoints = npoints - 3
                    | otherwise   = npoints - 2
        dn          = (angEnd - angBegin) / frint npoints'
        ang'        = (+ angBegin) . (* dn) . frint
        radiusRatio' = bottomRadius / topRadius
        vertex' n   = coneSectionVertex' radiusRatio' (ang' n) n
        vs          = map vertex'   [ 0 .. npoints - 1]
        texCoord' n = coneSectionVertexTexCoord' (ang' n) n (npoints - 1)
        ts          = map texCoord' [ 0 .. npoints - 1]
        ns          = concat . replicate npoints $ [vec4 0 0 1.0 1.0]
        doTex       = isJust texInfo
        texInfo'    = fromJust texInfo
        texName'    = fst texInfo'
        utt'        = snd texInfo'
        data''      | doTex     = data' vs (Just ts) (Just ns)
                    | otherwise = data' vs Nothing Nothing
    uniformsMatrixD log "cylinder" shaderD app'
    when doTex $ activateTexture log texName' utt'
    triangleStrip log data''

-- height = 1, axis = z-axis, bottom = x-axis.
-- top radius = 1
-- bottom radius = bottomRadius
coneSectionVertex' :: Float -> Float -> Int -> Vertex3 Float
coneSectionVertex' bottomRadius ang n = Vertex3 x y z where
    x = r * sin ang
    y | odd n = 0
      | otherwise = 1
    r | odd n = bottomRadius
      | otherwise = 1
    z = r * cos ang

-- @todo receive & deal with actual tex coords (tx00 etc.)
coneSectionVertexTexCoord' :: Float -> Int -> Int -> Vertex4 Float
coneSectionVertexTexCoord' ang n finalN = Vertex4 x y z w where
    perc = frint n / frint finalN
    x = perc
    -- are tex coords flipped?
    y | odd n = 1
      | otherwise = 0
    z = 0
    w = 1

torus app shaderC innerRadius thickness ncylinders = do
    let torus'' = torus' app shaderC innerRadius thickness ncylinders
    forM_ [0 .. ncylinders - 1] torus''

torus' app shaderC innerRadius thickness ncylinders n = do
    let app' = app & appMultiplyModel model'
        n' = frint n
        dt = 360 / frint ncylinders
        col = col' $ hsvCycle ncylinders n
        col' (r, g, b) = color r g b 255
        h = 2 * pi * innerRadius / frint ncylinders
        model' = multMatrices [ rotateZ 90
                              , translateZ innerRadius
                              , rotateY $ dt * n' ]
    -- need a skewed cylinder: cylinder with varying height xxx
    cylinder app' shaderC 20 h (thickness * 2) 0 (2 * pi) col

attribEnable = attribState Enabled
attribDisable = attribState Disabled
attribState s log (di, n) = attrib log tag loc s where
    tag = "attrib " <> show n
    loc = drawInfoAttribLocation di
