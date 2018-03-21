rectangle log (c1, c2, c3, c4) (v1, v2, v3, v4) = do
    let p (Vertex3 x y z) = (x, y, z)
    clientState VertexArray $= Enabled
    clientState ColorArray  $= Enabled
    pushColors   log [ c1, c2, c3, c4 ]
    -- criss-cross? xxx
    pushVertices log [ p v1, p v2, p v3, p v4 ]
    drawArrays TriangleFan 0 4
    checkGLErrors log "after drawArrays TriangleFan"
    clientState VertexArray $= Disabled
    clientState ColorArray  $= Disabled


-- fixed function.
triangle log (c1, c2, c3) (v1, v2, v3) = do
    let p (Vertex3 x y z) = (x, y, z)
    clientState VertexArray $= Enabled
    clientState ColorArray $= Enabled
    cptr <- pushColors log [ c1, c2, c3 ]
    vptr <- pushVertices log [ p v1, p v2, p v3 ]
    drawArrays Triangles 0 3
    checkGLErrors log "after drawArrays Triangles"
    clientState VertexArray $= Disabled
    clientState ColorArray $= Disabled
    free cptr
    free vptr

-- expects 4-ples of Word8.
-- the length of the outer array corresponds to the mode in drawArrays
-- (cube, lines, etc.)
-- for lines: line color will be a gradient from color1 to color2
pushColors :: Log -> [(Word8, Word8, Word8, Word8)] -> IO (Ptr Word8)
pushColors log tuples = do
    let list' = concatTuples4 tuples
        numComp' = 4 -- per vertex
        dataType' = GL.UnsignedByte
        stride' = 0
        colorArrayDescriptor = VertexArrayDescriptor numComp' dataType' stride'
        colorPointer = arrayPointer ColorArray
        ns' = zip list' [0 .. ]

    ptr <- mallocArray (length list') :: IO (Ptr Word8)
    forM_ ns' $ \(n, m) -> pokeElemOff ptr m n
    colorPointer $= colorArrayDescriptor ptr
    checkGLErrors log "after poking colorPointer"
    pure ptr


-- expects 3-ples of Float.
pushVertices :: Log -> [(Float, Float, Float)] -> IO (Ptr Float)
pushVertices log tuples = do
    let list' = concatTuples3 $ tuples
        numComp' = 3 -- per vertex
        dataType' = GL.Float
        stride' = 0
        vertexArrayDescriptor = VertexArrayDescriptor numComp' dataType' stride'
        vertexPointer = arrayPointer VertexArray
        ns' = zip list' [0 .. ]
        len' = length list'
    ptr <- mallocArray len' :: IO (Ptr Float)
    forM_ ns' $ \(n, m) -> pokeElemOff ptr m n
    vertexPointer $= vertexArrayDescriptor ptr
    checkGLErrors log "after poking vertexPointer"
    pure ptr

pushTexCoords :: Log -> [(Float, Float, Float, Float)] -> IO (Ptr Float)
pushTexCoords log tuples = do
    let list' = concatTuples4 tuples
        numComp' = 4 -- per vertex
        dataType' = GL.Float
        stride' = 0
        texArrayDescriptor = VertexArrayDescriptor numComp' dataType' stride'
        texCoordPointer = arrayPointer TextureCoordArray
        ns' = zip list' [0 .. ]
        len' = length list'
    ptr <- mallocArray len' :: IO (Ptr Float)
    forM_ ns' $ \(n, m) -> pokeElemOff ptr m n
    texCoordPointer $= texArrayDescriptor ptr
    checkGLErrors log "after poking texCoordPointer"
    pure ptr


-- old fixed function calls
        -- clientState VertexArray $= Enabled
        -- verticesPtr <- pushVertices log . concat . replicate 8 $ frontFace
        -- wrapGL log "clientState TextureCoordArray enabled"  $ clientState TextureCoordArray $= Enabled
        -- wrapGL log "texture Texture2D"                      $ texture Texture2D             $= Enabled
        -- wrapGL log "texture Texture2D disabled"             $ texture Texture2D             $= Disabled
        -- wrapGL log "clientState TextureCoordArray disabled" $ clientState TextureCoordArray $= Disabled
        --


pushNormals :: Log -> [(Float, Float, Float)] -> IO (Ptr Float)
pushNormals log tuples = do
    let list' = concatTuples3 tuples
        numComp' = 3 -- per vertex
        dataType' = GL.Float
        stride' = 0
        normalArrayDescriptor = VertexArrayDescriptor numComp' dataType' stride'
        normalPointer = arrayPointer NormalArray
        len' = length list'
        ns' = zip list' [0 .. ]
    ptr <- mallocArray len' :: IO (Ptr Float)
    forM_ ns' $ \(n, m) -> pokeElemOff ptr m n
    normalPointer $= normalArrayDescriptor ptr
    checkGLErrors log "after poking normalPointer"
    pure ptr


-- old-style: init view using GL matrices.
initViewG log args = do
    let debug' = debug log
    matrixMode $= Modelview 0
    loadIdentity

    m <- matrixMath log $ do
        multMatrix =<< lookAtFG log v1 v2 v3
        translateZG tz
        -- test
        translateXG 1.0

    DMX.fromList 4 4 <$> getMatrixComponents ColumnMajor m
    -- pure m where
    where
        v1d = ver3gld 0 0 lzd
        v2d = ver3gld 0 0 0
        v3d = vec3gld 0 1 0

        v1 = ver3 0 0 lz
        v2 = ver3 0 0 0
        v3 = vec3 0 1 0

        lz = 0.4
        tz = inv 2

        lzd = 0.4

-- lighting happens in eye coordinates.
-- positive z is out of the screen, positive x is left, positive y is down.
-- x: [-240, 240], y: [-400, 400], z: [?] (also big numbers)
-- but x = -1 is the right.

initLightingFixed log args = do
    wrapGL log "enable lighting"         $ lighting                $= Enabled

    -- colors here don't do shit?
    -- and these don't work on android anyway (invalid enumerant, but no
    -- crash).
    -- wrapGL log "material specular front" $ materialSpecular Front  $= color4 200 30 10   200
    -- wrapGL log "material specular back"  $ materialSpecular Back   $= color4 200 30 10   200
    -- wrapGL log "material diffuse front"  $ materialDiffuse Front   $= color4 200 30 10   200
    -- wrapGL log "material diffuse back"   $ materialDiffuse Back    $= color4 200 30 10   200
    -- wrapGL log "material shininess"      $ materialShininess Front $= 0.5

    -- global ambient
    wrapGL log "light model ambient"     $ lightModelAmbient       $= color4 255 51 51 255

    -- each of these crashes android.
    -- wrapGL log "light model local view"  $ lightModelLocalViewer   $= Disabled
    -- wrapGL log "light model two side"    $ lightModelTwoSide       $= Disabled

    let aa = 255
        ba = 0

    do  let [r, g, b, a] = [200, 100, 50, 255]

        wrapGL log "enable light 0"          $ light    (Light 0)      $= Enabled
        wrapGL log "light 0 ambient"         $ ambient  (Light 0)      $= color4 r g b aa
        wrapGL log "light 0 diffuse"         $ diffuse  (Light 0)      $= color4 r g b a
        wrapGL log "light 0 specular"        $ specular (Light 0)      $= color4 r g b a

        wrapGL log "enable light 2"          $ light    (Light 2)      $= Enabled
        wrapGL log "light 2 ambient"         $ ambient  (Light 2)      $= color4 r g b aa
        wrapGL log "light 2 diffuse"         $ diffuse  (Light 2)      $= color4 r g b a
        wrapGL log "light 2 specular"        $ specular (Light 2)      $= color4 r g b a

    do  let [r, g, b, a] = [50, 150, 200, 150]

        wrapGL log "enable light 1"          $ light    (Light 1)      $= Enabled
        wrapGL log "light 1 ambient"         $ ambient  (Light 1)      $= color4 r g b ba
        wrapGL log "light 1 diffuse"         $ diffuse  (Light 1)      $= color4 r g b a
        wrapGL log "light 1 specular"        $ specular (Light 1)      $= color4 r g b a

        wrapGL log "enable light 3"          $ light    (Light 3)      $= Enabled
        wrapGL log "light 3 ambient"         $ ambient  (Light 3)      $= color4 r g b ba
        wrapGL log "light 3 diffuse"         $ diffuse  (Light 3)      $= color4 r g b a
        wrapGL log "light 3 specular"        $ specular (Light 3)      $= color4 r g b a

    -- w = 0: light is directional, infinitely far away, no
    -- attenuation, ignore position: x y z specify direction, and it is
    -- affected by modelview.
    -- w is nonzero (use 1, because w controls the homogeneity of the other
    -- coords): light is positional, attenuated,
    -- x y z is position, and the location is transformed by modelview.
    -- attenuation: ( constant, linear, quadratic )

    let ca = 0.5
        la = 0

    do  let [x,y,z,w,qa] = [1, 0, inv 3, 1, 0.1]
            x' = inv x
            y' = y + 2
            z' = inv z
        wrapGL log "light 0 position" $ position (Light 0) $= ver4 x y z w
        wrapGL log "light 0 attenuation" $ attenuation (Light 0) $= (ca, la, qa)

        wrapGL log "light 2 position" $ position (Light 2) $= ver4 x' y' z' w
        wrapGL log "light 2 attenuation" $ attenuation (Light 2) $= (ca, la, qa)

    do  let [x,y,z,w,qa] = [inv 2, 0, inv 3, 1, 0.1]
            x' = inv x
            y' = y + 2
            z' = inv z
        wrapGL log "light 1 position"    $ position (Light 1) $= ver4 x y z w
        wrapGL log "light 1 attenuation" $ attenuation (Light 1) $= (ca, la, qa)

        wrapGL log "light 3 position"    $ position (Light 3) $= ver4 x' y' z' w
        wrapGL log "light 3 attenuation" $ attenuation (Light 3) $= (ca, la, qa)

-- set frustum using old-style GL.
-- symmetrical: l = -r & t = -b
frustumFMG log r t n f = do

    wrapGL log "matrixMode Projection" $ matrixMode $= Projection
    wrapGL log "loadIdentity"          $ loadIdentity
    wrapGL log "frustumFG mult matrix" $
        frustumFG r t n f >>= multMatrix

-- symmetrical: l = -r & t = -b
-- use ColumnMajor for transform matrices; also RowMajor crashes android.
-- returns foreign ptr.
frustumFG :: Float -> Float -> Float -> Float -> IO (GLmatrix GLfloat)
frustumFG r t n f = withNewMatrix ColumnMajor ptrAct' where
    ptrAct' = (flip pokeArray) $ frustumF' r t n f

-- returns foreign ptr.
lookAtFG :: Log -> Vertex3 Float -> Vertex3 Float -> Vector3 Float -> IO (GLmatrix GLfloat)
lookAtFG log v1@(Vertex3 ex ey ez) v2@(Vertex3 cx cy cz) v3@(Vector3 ux uy uz) =
    matrixMath log $ do
        multMatrix =<< lookAtFStage1G v1 v2 v3
        translateXG . inv $ ex
        translateYG . inv $ ey
        translateZG . inv $ ez

-- use ColumnMajor for transform matrices; also RowMajor crashes android.
lookAtFStage1G :: Vertex3 Float -> Vertex3 Float -> Vector3 Float -> IO (GLmatrix GLfloat)
lookAtFStage1G v1 v2 v3 =
    withNewMatrix ColumnMajor ptrAct' where
        ptrAct' = (flip pokeArray) ns
        ns = lookAtFStage1' v1 v2 v3

-- set frustum using old-style DMX.
frustumFMD :: Log -> Float -> Float -> Float -> Float -> IO ()
frustumFMD log r t n f = do
    m <- toMGC $ frustumF log r t n f

    wrapGL log "matrixMode Projection" $ matrixMode $= Projection
    wrapGL log "loadIdentity"          $ loadIdentity
    wrapGL log "mult"                  $ multMatrix m

-- rotateN = rotate about N-axis, in degrees.
rotateXG :: Float -> IO Float
rotateXG x' = do
    rotate x' $ vec3 1 0 0
    pure x'

rotateYG :: Float -> IO Float
rotateYG y' = do
    rotate y' $ vec3 0 1 0
    pure y'

rotateZG :: Float -> IO Float
rotateZG z' = do
    rotate z' $ vec3 0 0 1
    pure z'

-- useful for tests -- will not work on android.
-- needs updating to get the matrices
projectItGLUDouble log click args dim (viewportWidth, viewportHeight) = do
    let info' = info log
        debug' = debug log
        vecd' = Vertex3 dimd' dimd' dimd' :: Vertex3 GLdouble
        dimd' = float2Double dim

    -- modelViewDouble'  <- STV.get $ matrix (Just $ Modelview 0) :: IO (GLmatrix GLdouble)
    -- projectionDouble' <- STV.get $ matrix (Just $ Projection)  :: IO (GLmatrix GLdouble)
    let modelViewDouble'  = undefined
        projectionDouble' = undefined

    (vpx, vpy) <- projectItGLU log modelViewDouble' projectionDouble' vecd' (viewportWidth, viewportHeight)
    when (isJust click) $ do
        debug' $ printf "control project => %.1f %.1f" vpx vpy

--        depth' <- getClickDepth log click
--        let (x, y) = fromJust click
--            ver' = Vertex3 (frint x) (frint y) (float2Double depth')
--            verf' = Vertex3 (frint x) (frint y) depth'
--         let [va, vb, vc] = take 3 . map (read :: String -> Float) $ args
--         let ver' = Vertex3 va vb vc
--         yyy <- unProjectItF log ver'
--         when (isJust yyy) $ do
--             let Vertex3 vvx vvy vvz = fromJust yyy
--             info' $ printf "test verf' %s => vvx %.2f vvy %.2f vvz %.2f" (show ver') vvx vvy vvz
-- 
--         let aaver' = Vertex3 (float2Double va) (float2Double vb) (float2Double vc)
--         aazzz <- unProjectItGLU log modelViewDouble' projectionDouble' aaver'
--         let Vertex3 aavvx aavvy aavvz = aazzz
--         info' $ printf "control verd' %s => aavvx %.2f aavvy %.2f aavvz %.2f" (show aaver') aavvx aavvy aavvz

translateXG n = translate $ vec3 n 0 0
translateYG n = translate $ vec3 0 n 0
translateZG n = translate $ vec3 0 0 n

-- columnmajor, 4*4
newGLmatrix :: [Float] -> IO (GLmatrix GLfloat)
newGLmatrix vals = withNewMatrix ColumnMajor mat where
    mat ptr = mapM_ (map' ptr) [ 0 .. 15]
    map' ptr' n = pokeElemOff ptr' n $ vals !! n

-- meant to work in conjunction with rotationMatrix, to deal with null
-- matrices (rotation of 0 or 180).
flipMatrixBinary theta = flipMatrixBinary' (pi - abs theta) where
    flipMatrixBinary' diff'
      | diff' >= theta = Nothing
      | otherwise = Just flip'
    flip' = newGLmatrix [ inv 1, 0, 0, 0
                        , 0, inv 1, 0, 0
                        , 0, 0, inv 1, 0
                        , 0, 0, 0, 1 ]

-- must be unit vector.
rotationMatrix ux uy uz theta = newGLmatrix $
    [ cos'         + (ux ** 2) * m'
    , ux * uy * m' - uz * sin'
    , ux * uz * m' + uy * sin'
    , 0

    , ux * uy * m' + uz * sin'
    , cos'         + (uy ** 2) * m'
    , uy * uz * m' - ux * sin'
    , 0

    , ux * uz * m' - uy * sin'
    , uy * uz * m' + ux * sin'
    , cos'         + (uz ** 2) * m'
    , 0

    , 0
    , 0
    , 0
    , 1 ] where
        cos' = cos theta
        sin' = sin theta
        m' = 1 - cos'

