prepareTexture log texName width height imagesBase64 frames args = do
    info log "doing callocarray"
    rawPixelData565  <- if copyTo565 then mallocArray $ width * height :: IO (Ptr CUShort)
                                     else mallocArray $ 0              :: IO (Ptr CUShort)
    rawPixelData8888 <- mallocArray                   $ width * height * 4 :: IO (Ptr CUChar)

    info log "doing createImageSurfaceForData"
    cSurf <- C.createImageSurfaceForData rawPixelData8888 C.FormatARGB32 width height (width * 4)

    let glPixelData565 = PixelData GL.RGB GL.UnsignedShort565 rawPixelData565
        -- makes neske turn blue
        -- glPixelData8888 = PixelData GL.BGRA GL.UnsignedByte rawPixelData8888
        glPixelData8888 = PixelData GL.RGBA GL.UnsignedByte rawPixelData8888
        bgSurf' (Just imageBase64) = Just <$> cairoSurfFromData log imageBase64 args
        bgSurf' Nothing = pure Nothing
    info log "doing cairoSurfFromData"
    cBgSurf <- bgSurf' . head $ imagesBase64
    info log "after cairoSurfFromData"
    pure TextureData { textureDataWidth = width
                     , textureDataHeight = height
                     , textureDataFrames = frames
                     -- created by cairo, no manual managing of array
                     , textureDataCBGSurface = cBgSurf
                     -- backed by rawPixelData8888
                     , textureDataCSurface = cSurf
                     , textureDataGLPixelData565 = glPixelData565
                     , textureDataRawPixelData565 = rawPixelData565
                     , textureDataGLPixelData8888 = glPixelData8888
                     , textureDataRawPixelData8888 = rawPixelData8888
                     , textureDataTextureObject = texName }




    -- debug' "doing prepareTexture"
--     let texDim = textureDimension
--         framesNoop = concat . repeat $ [renderNoop]
--     textureData0 <- prepareTexture log texName0 texDim texDim (repeat . Just $ imageBase64_1MOCK) framesNoop args
-- 
--     -- frames1 <- concat . repeat . take 10 <$> SCC.renderFrames False
--     textureData1 <- prepareTexture log texName1 texDim texDim (repeat . Just $ imageBase64_2MOCK) framesNoop args
-- 
--     let bind' tex' = bindAndTransferTexture log tex' 0
--     mapM_ bind' [ textureData0, textureData1 ]


    -- debug' "doing prepareTexture"
--     let texDim = textureDimension
--         framesNoop = concat . repeat $ [renderNoop]
--     textureData0 <- prepareTexture log texName0 texDim texDim (repeat . Just $ imageBase64_1MOCK) framesNoop args
-- 
--     -- frames1 <- concat . repeat . take 10 <$> SCC.renderFrames False
--     textureData1 <- prepareTexture log texName1 texDim texDim (repeat . Just $ imageBase64_2MOCK) framesNoop args
-- 
--     let bind' tex' = bindAndTransferTexture log tex' 0
--     mapM_ bind' [ textureData0, textureData1 ]

quadricStyleSimple = QuadricStyle
        quNormalShadingModel' NoTextureCoordinates
        orient' FillStyle
    where quNormalShadingModel' = Nothing
          orient' = Inside

bindAndTransferTexture log textureData t = do
    bench <- benchStart log $ printf "bindAndTransferTexture %s" (show . textureDataTextureObject $ textureData)
    let texName = textureDataTextureObject textureData
        -- for copying to gl texture, if embedded.
        glPixelData565 = textureDataGLPixelData565 textureData
        rawPixelData565 = textureDataRawPixelData565 textureData
        -- for drawing on with cairo
        glPixelData8888 = textureDataGLPixelData8888 textureData
        rawPixelData8888 = textureDataRawPixelData8888 textureData
        -- backed by rawPixelData8888
        cSurf = textureDataCSurface textureData

        maybeCPatternSurface = textureDataCBGSurface textureData
        width = textureDataWidth textureData
        height = textureDataHeight textureData
        frames = textureDataFrames textureData

        -- mipmap level: always 0.
        level' = 0

        -- 'GL_INVALID_OPERATION is generated if type is GL_UNSIGNED_SHORT_5_6_5
        -- and format is not GL_RGB.'
        -- And the type is read from the PixelData structure.
        internalFormat' = if copyTo565 then GL.RGB' -- GL_RGB
                                       -- else GL.RGBA' -- GL_RGBA
                                       else GL.RGBA8 -- GL_RGBA

    textureBinding Texture2D $= Just texName

    bench1 <- benchStart log "  . renderToCairoSurface"
    -- transfer the bgSurf (pic) to cSurf using a cairo pattern.
    debug log "doing renderWith"
    -- C.renderWith cSurf $ renderToCairoSurface maybeCPatternSurface width height frames
    debug log "after renderWith"
    benchUpdate bench1

    -- transfer pixels from raw8888 to raw565, converting along the way
    let map' n = do
            let n' = n * 4
            -- CUChar
            r <- peekElemOff rawPixelData8888 $ n' + 0
            g <- peekElemOff rawPixelData8888 $ n' + 1
            b <- peekElemOff rawPixelData8888 $ n' + 2

            -- simple conversion: drop 2 or 3 LSBs.
            -- is the and-mask necessary? we are unsigned for sure.
            let rsh = fromIntegral r :: CUShort
                gsh = fromIntegral g :: CUShort
                bsh = fromIntegral b :: CUShort
                rs = (rsh `shiftR` 3) .&. 31
                gs = (gsh `shiftR` 2) .&. 63
                bs = (bsh `shiftR` 3) .&. 31
                rss = (`shiftL` 11) rs
                gss = (`shiftL` 5)  gs
                bss =               bs
                toPoke' = rss .|. gss .|. bss

            pokeElemOff rawPixelData565 n toPoke'

    let texDim = textureDimension
    bench2 <- benchStart log "  . poke"
    when copyTo565 $ do
        debug log "starting poke"
        mapM_ map' [0 .. texDim * texDim - 1]
        debug log "after poke"
    benchUpdate bench2

    let textureSize' = fromIntegral width
    bench3 <- benchStart log "  . texImage2D"
    let texImage2D' = texImage2D Texture2D NoProxy level' internalFormat' (TextureSize2D textureSize' textureSize') 0
    debug log "begin texImage2D"
    if copyTo565 then texImage2D' glPixelData565
                 else texImage2D' glPixelData8888
    debug log "after texImage2D"

    benchUpdate bench3

    checkGLErrors log "after texImage2D"

    benchUpdate bench

-- no longer necessary.
-- convert double matrix to float.
-- also, peeking wasn't necessary anyway (getMatrixComponents would have workd)
modelViewProjectionFloat :: IO (GLmatrix GLdouble, GLmatrix GLdouble)
modelViewProjectionFloat = do
    let with' :: (Ptr GLdouble) -> MatrixOrder -> (Ptr GLfloat) -> IO ()
        with' tgt' order' src' = matrixFloatToDouble' src' tgt'
        matrixFloatToDouble' src' tgt' = mapM_ (map' src' tgt') [0 .. 15]
        map' src' tgt' n = do
            val' <- peekElemOff src' n
            pokeElemOff tgt' n $ float2Double val'
        makeDouble mat' tgtPtr' = withMatrix mat' (with' tgtPtr')

    -- vertex unit = 0
    mvFloat   <- STV.get $ matrix (Just (Modelview 0)) :: IO (GLmatrix GLfloat)
    projFloat <- STV.get $ matrix (Just Projection)    :: IO (GLmatrix GLfloat)

    -- order: trial & error
    (,) <$> withNewMatrix ColumnMajor (makeDouble mvFloat) <*> withNewMatrix ColumnMajor (makeDouble projFloat)

-- get foreign pointer to vector using Vector's unsafeToForeignPtr0
-- then cast it straight to Ptr CUChar (which is Cairo's PixelData)
-- xxx.

cairoSurfFromData :: Log -> ByteString -> [String] -> IO C.Surface
cairoSurfFromData log imageBase64 args = do
    let debug' = debug log
        imageBase64Decoded = B64.decode imageBase64
--         imageBase64Decoded | isEmbedded == True = B64.decode imageBase64MOCK
--                            | otherwise = B64.decode imageBase64

    (when . isLeft $ imageBase64Decoded) $ do
        err' . toLeft $ imageBase64Decoded

    let imageBase64Decoded' = toRight imageBase64Decoded
        dynamicImage = decodePng imageBase64Decoded'

    (when . isLeft $ dynamicImage) $ do
        err'' . toLeft $ dynamicImage

    -- . Better way to do this?
    -- . We use JuicyPixels to get the image data as an RGBA8 vector, then
    -- malloc a new array and poke everything into it.
    -- . Performance still seems ok, even on Android.

    let dynamicImage'                 = toRight dynamicImage
        -- Convert decoded image to RGBA8 by any possible means.
        imgrgba                       = convertRGBA8 dynamicImage'

        imgData :: Image JP.PixelRGBA8 -> DVS.Vector (PixelBaseComponent JP.PixelRGBA8)
        imgData (Image _ _ data')     = data'

        imgWidth (Image width' _ _ )  = width'
        imgHeight (Image _ height' _) = height'

        -- don't always have this function available
        -- stride = C.formatStrideForWidth C.FormatARGB32 800
        stride'                       = 4 * width'

        width'                        = imgWidth imgrgba
        height'                       = imgHeight imgrgba

        theData' :: DVS.Vector (PixelBaseComponent JP.PixelRGBA8)
        theData'                      = imgData imgrgba

        (vecfp', veclen')             = unsafeToForeignPtr0 theData'

    pixelData' <- mallocArray veclen' :: IO (Ptr Word8)

    let poke' n = pokeElemOff pixelData' n $ (theData' DVS.! n)
    mapM_ poke' [0 .. veclen' - 1]

    debug' $ "Stride " <> show stride'

    putStrLn $ printf "vec size: %d" $ DVS.length theData'

    let pixelData'' = castPtr pixelData' :: Ptr CUChar
    C.createImageSurfaceForData pixelData'' C.FormatARGB32 width' height' stride' where
        err' e = die' $ "Couldn't decode base64, " ++ e
        err'' e = die' $ "Couldn't decode png, " ++ e
        die' = die log

-- . calloc an array of 2-byte pixels (Ptr CUShort)
--   it is *not* possible to draw to this array using Cairo.
-- . calloc an array of 4-byte pixels (Ptr CUChar)
--   use this to back a cairo surface which can be drawn on using Cairo.
-- . prepare glPixelData data type with format RGB and type UnsignedShort565:
--   the only combination which GLES allows in the texImage2D call.
-- . make cBgSurf from the image.



matVec4fMultOld :: GLmatrix GLfloat -> Vector4 GLfloat -> IO (Vector4 GLfloat)
matVec4fMultOld mat1 (Vector4 vx vy vz vw) = do
    mc1 <- getMatrixComponents ColumnMajor mat1
    matVec4Mult mc1 [vx, vy, vz, vw]

matVec4Mult m [vx, vy, vz, vw] = do
    let x = mm 0
        y = mm 1
        z = mm 2
        w = mm 3
        col row' n = m !! (4 * row' + n)
        mm row' = vx * col row' 0
                + vy * col row' 1
                + vz * col row' 2
                + vw * col row' 3
        pad' = take 5 . (<>) "    "
        show' = pad' . show
        print' n = putStrLn $ printf "| %s %s %s %s | %s | = %s"
            (show' $ m !! ( n * 4 + 0 ))
            (show' $ m !! ( n * 4 + 1 ))
            (show' $ m !! ( n * 4 + 2 ))
            (show' $ m !! ( n * 4 + 3 ))
            (show' $ vx)
            (show' $ x)
    when False $ do
        mapM_ print' [0 .. 3]
        putStrLn "------"
    pure $ Vector4 x y z w


-- not working
projectIt :: Log -> Vertex3 Float -> IO (Float, Float)
projectIt log (Vertex3 x y z) = do
    let info' = info log
        vec = Vector4 x y z 1.0
        -- vec = Vector4 (inv x) (inv y) (inv z) 1.0
        viewposx = 0
        viewposy = 0
        view0 = frint viewposx
        view1 = frint viewposy
        view2 = frint viewportWidth
        view3 = frint viewportHeight

    mv   <- modelview log
    proj <- projection log

    eye   <- mv `matVec4fMult` vec
    world <- proj `matVec4fMult` eye

    -- confusion with w & depth here xxx
    let v' = world
        shiftX' x' = view0 + view2 * (x' + 1) / 2.0
        shiftY' y' = view1 + view3 * (y' + 1) / 2.0
        shift (Vector4 wx wy _ _) = (shiftX' wx, frint viewportHeight - shiftY' wy)

    info' $ printf "v' %s" (show v')
    pure . shift $ v'

muchomulto :: DMX.Matrix Double -> [Double] -> [Double]
muchomulto  a b = [j, k, l, m] where
    a' = toList a
    j = (a' !! 0) * (b !! 0) + (a' !! 1) * (b !! 1) + (a' !! 2) * (b !! 2) + (a' !! 3) * (b !! 3 )
    k = (a' !! 4) * (b !! 0) + (a' !! 5) * (b !! 1) + (a' !! 6) * (b !! 2) + (a' !! 7) * (b !! 3 )
    l = (a' !! 8) * (b !! 0) + (a' !! 9) * (b !! 1) + (a' !! 10) * (b !! 2) + (a' !! 11) * (b !! 3 )
    m = (a' !! 12) * (b !! 0) + (a' !! 13) * (b !! 1) + (a' !! 14) * (b !! 2) + (a' !! 15) * (b !! 3 )

-- calculate the angles using a different method than lineStroke.
-- small discrepancies in some cases: not sure which is more accurate.
-- external dep.
-- manual matrix multiplication.
-- malloc'd arrays.

lineStrokeManual log shader (c1, c2) (ver1, ver2) thickness = do
    let x (Vertex3 x' _ _ ) = x'
        y (Vertex3 _ y' _ ) = y'
        z (Vertex3 _ _ z' ) = z'
        dx           = verx2 - verx1
        dy           = very2 - very1
        dz           = verz2 - verz1
        dr           = sqrt $ (dx ** 2) + (dy ** 2) + (dz ** 2)
        verx1        = x ver1
        very1        = y ver1
        verz1        = z ver1
        verx2        = x ver2
        very2        = y ver2
        verz2        = z ver2
        basis'       = vec3 1.0 0 0
        vec'         = vec3 dx dy dz
        rotAxis'     = vcross basis' vec'
        rotAxisMag'  = vmag rotAxis'
        -- try to avoid it blowing up ... this number is just a guess.
        rotAxisUnit' | rotAxisMag' < 0.05 = Nothing
                     | otherwise = Just . vdiv rotAxisMag' $ rotAxis'
        rotAngle'    = acos . (/ dr)  $ vdot basis' vec'
        ux           = v3x . fromJust $ rotAxisUnit'
        uy           = v3y . fromJust $ rotAxisUnit'
        uz           = v3z . fromJust $ rotAxisUnit'

        multMatrix' Nothing = pure ()
        multMatrix' (Just mat) = multMatrix =<< mat

        mat' Nothing  = flipMatrixBinary rotAngle'
        mat' (Just _) = Just $ rotationMatrix ux uy uz (inv rotAngle')

    -- putStrLn $ printf "dx %.1f dy %.1f dz %.1f dr %.1f rotAxis' %s, rotAxisUnit' %s, rotAngle' %.1f, has rotation: %s" dx dy dz dr (show rotAxis') (show rotAxisUnit') (toDeg rotAngle') (if isJust (mat' rotAxisUnit') then "yes" else "no" :: String)

    preservingMatrix $ do
        matrixMode $= Modelview 0

        translateX verx1
        translateY very1
        translateZ verz1

        multMatrix' . mat' $ rotAxisUnit'

        translateY . (/ inv 2) $ thickness

        let vs' = ( ver3 0 0 0
                  , ver3 dr 0 0
                  , ver3 dr thickness 0
                  , ver3 0 thickness 0 )
            cs' = (c1, c2, c2, c1)

        rectangle log shader cs' vs'

    pure ()

-- a test to see if Modelview 1 works.
testModelview1 = do
    let v1 = ver3 0 0 lz
        v2 = ver3 0 0 0
        v3 = vec3 0 1 0
        lz = 0.4

    wrapGL log "mm 1" $ matrixMode $= Modelview 1
    wrapGL log "loadid" loadIdentity
    wrapGL log "looky" $ lookAtF v1 v2 v3


manualLookAtF = do
    --    m1 <- lookAtF' v1 v2 v3
    -- m1dm <- toDMX m1
    -- let m2 = translateDMXX . inv $ ex
    --    m3 = translateDMXY . inv $ ey
    --    m4 = translateDMXZ . inv $ ez
    -- toGLMC $ m1dm `multStd` m2 `multStd` m3 `multStd` m4

