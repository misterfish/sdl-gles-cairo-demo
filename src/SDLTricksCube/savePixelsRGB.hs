-- all images must be 512 x 512 -- no boundary checks performed.

module SDLTricksCube.Pixels ( copyJPImageToPixelArray8888
                            , copyJPImageToPixelArray565
                            , copyJPImageToPixelArray8888_Luminance
                            , copyRaw8888To565
                            ) where

import           GHC.Real ( realToFrac )
import           Data.Bits ( (.&.), (.|.)
                           , shiftL, shiftR
                           , unsafeShiftL, unsafeShiftR )

import qualified Data.Vector.Storable as DVS ( (!)
                                             , Vector
                                             , length
                                             , toList
                                             , fromList )
import           Data.Vector.Storable as DVS ( unsafeWith
                                             , unsafeToForeignPtr0 )
import           Text.Printf ( printf )
import           Control.Monad ( (<=<), unless, guard, when, forM_ )

import           Data.Word ( Word8, Word16 )
import           Foreign ( ForeignPtr
                         , Ptr
                         , Int32
                         , castPtr
                         , pokeElemOff
                         , peekElemOff
                         , advancePtr
                         , peekArray
                         , pokeArray
                         , copyArray
                         , mallocForeignPtrArray
                         , withForeignPtr )
import           Foreign.C.Types ( CUChar (CUChar), CUShort (CUShort), CInt )

import           Codec.Picture      as JP ( DynamicImage ( ImageRGBA8, ImageRGB8
                                                         , ImageYA8, ImageY8 )
                                          , Image (Image)
                                          , PixelRGBA8 (PixelRGBA8)
                                          , PixelRGB8 (PixelRGB8)
                                          , PixelYA8 (PixelYA8)
                                          , PixelBaseComponent
                                          , Pixel8
                                          , convertRGBA8
                                          , convertRGB8
                                          , decodePng )

import           SDLTricksCube.Util ( frint
                                    , isMultiple
                                    , isInteger )
import           SDLTricksCube.Config ( doDebug )
import           SDLTricksCube.Types ( Log (Log, info, warn, err) )

foreign import ccall "copy_888_565" c_copy_888_565 :: Int -> Ptr CUChar -> Ptr CUShort -> IO ()
foreign import ccall "convert_888_565" c_convert_888_565 :: CUChar -> CUChar -> CUChar -> IO CUShort

-- DynamicImage is JP's Image type, which is backed by a vector.

copyJPImageToPixelArray8888 = copyJPImageToPixelArray8888_RGBA

copyJPImageToPixelArray8888_RGBA :: DynamicImage -> Ptr CUChar -> IO ()
copyJPImageToPixelArray8888_RGBA img ary = do
    let imgrgba = convertRGB8 img
        -- actually we know that it's 512 * 512 * 4
        -- eh 3
        veclen' = DVS.length theData'
        theData' = dImgData imgrgba
        ary' = castPtr ary :: Ptr Word8
        (fptr, l) = unsafeToForeignPtr0 theData'

    withForeignPtr fptr $ \ptr ->
        copyArray ary' ptr l

copyJPImageToPixelArray565 = copyJPImageToPixelArray565New

copyJPImageToPixelArray565New :: Log -> DynamicImage -> Ptr CUShort -> IO ()
copyJPImageToPixelArray565New log img ary = do
    let imgrgba = convertRGB8 img
        -- actually we know that it's 512 * 512 * 4
        -- 3!
        veclen' = DVS.length theData'
        theData' = dImgData imgrgba
        l' = floor . (/ 3) . frint $ veclen'

    when (not $ isMultiple 3 veclen') . error $ printf "unexpected length: %d" veclen'

    let map' n = rgb8888To565 r g b where
             r = theData' DVS.! (m + 0)
             g = theData' DVS.! (m + 1)
             b = theData' DVS.! (m + 2)
             m = n * 3

    -- pokeArray ary $ map map' [ 0 .. l'-1 ]
    pokeArray ary =<< mapM map' [ 0 .. l'-1 ]

copyRaw8888To565 :: Log -> Ptr CUChar -> Ptr CUShort -> IO ()
copyRaw8888To565 log from to =
    c_copy_888_565 len' from to where
        len' = 512 * 512

copyRaw8888To565' :: (Ptr CUChar, Ptr CUShort) -> Int -> Int -> IO ()
copyRaw8888To565' (from, to) len n
  | n == len = pure ()
  | otherwise = do
      let nxtfrom = advancePtr from 3
          nxtto   = advancePtr to 1
      [r, g, b] <- mapM (peekElemOff from) [0 .. 2]
      pokeElemOff to 0 =<< rgb8888To565 r g b
      copyRaw8888To565' (nxtfrom, nxtto) len (n + 1)

dImgData :: Image JP.PixelRGB8 -> DVS.Vector (PixelBaseComponent JP.PixelRGB8)
dImgData (Image _ _ data')     = data'
dImgWidth (Image width' _ _ )  = width'
dImgHeight (Image _ height' _) = height'

rgb8888To565 r g b = c_convert_888_565 rr gg bb where
    rr = frint r
    gg = frint g
    bb = frint b

-----------------

copyRaw8888To565New :: Log -> Ptr CUChar -> Ptr CUShort -> IO ()
copyRaw8888To565New log from to = do
    copyRaw8888To565' (castPtr from, to) len' 0 where
        len' = 512 * 512

-- simple conversion: drop 2 or 3 LSBs.
-- is the and-mask necessary? we are unsigned for sure.
rgb8888To565Hs r g b = rss .|. gss .|. bss where
    -- making char into short, wasteful.
    rsh = fromIntegral r :: CUShort
    gsh = fromIntegral g :: CUShort
    bsh = fromIntegral b :: CUShort
    -- rs = (rsh `shiftR` 3) .&. 31
    -- gs = (gsh `shiftR` 2) .&. 63
    -- bs = (bsh `shiftR` 3) .&. 31
    rs = rsh `unsafeShiftR` 3
    gs = gsh `unsafeShiftR` 2
    bs = bsh `unsafeShiftR` 3
    rss = (`unsafeShiftL` 11) rs
    gss = (`unsafeShiftL` 5)  gs
    bss =               bs

debug | doDebug == True = info
      | otherwise       = const . const . pure $ ()

-- broken xxx
copyJPImageToPixelArray8888_BGRA :: DynamicImage -> Ptr CUChar -> IO ()
copyJPImageToPixelArray8888_BGRA img ary = do
    -- let imgrgba = convertRGBA8 img
    let imgrgba = convertRGB8 img
        -- actually we know that it's 512 * 512 * 4
        -- eh 3
        veclen' = DVS.length theData'
        theData' = dImgData imgrgba

    -- asymmetry with below (Word8 v CUShort), but avoids an extra
    -- `fromIntegral`.

    let ary' = castPtr ary :: Ptr Word8
        ns = [0, 4 .. veclen' - 1]
    forM_ ns $ \n -> do
        pokeElemOff ary' (n + 2) $ theData' DVS.! (n + 0)
        pokeElemOff ary' (n + 1) $ theData' DVS.! (n + 1)
        pokeElemOff ary' (n + 0) $ theData' DVS.! (n + 2)
        pokeElemOff ary' (n + 3) $ theData' DVS.! (n + 3)

-- probably broken xxx
copyJPImageToPixelArray8888_Luminance :: DynamicImage -> Ptr CUChar -> IO ()
copyJPImageToPixelArray8888_Luminance img ary = do
--     let imgrgba = convertRGBA8 img
    let imgrgba = convertRGB8 img
        -- actually we know that it's 512 * 512 * 4
        -- ehh 3
        veclen' = DVS.length theData'
        theData' = dImgData imgrgba

    -- asymmetry with below (Word8 v CUShort), but avoids an extra
    -- `fromIntegral`.

    let ary' = castPtr ary :: Ptr Word8
        ns = zip [0, 4 .. veclen' - 1] [0 .. ]
    forM_ ns $ \(n, m) -> do
        let r = theData' DVS.! (n + 0)
            g = theData' DVS.! (n + 1)
            b = theData' DVS.! (n + 2)
            r' = frint r
            g' = frint g
            b' = frint b
            l' = r' * 0.3 + g' * 0.6 + b' * 0.1 :: Float
            l = floor l' :: Word8
        pokeElemOff ary' m l

copyJPImageToPixelArray565Old :: Log -> DynamicImage -> Ptr CUShort -> IO ()
copyJPImageToPixelArray565Old log img ary = do
    let imgrgba = convertRGB8 img
        -- actually we know that it's 512 * 512 * 4
        -- ehm 3
        veclen' = DVS.length theData'
        theData' = dImgData imgrgba

        ary' = castPtr ary :: Ptr CUShort
        ns = zip [0, 3 .. veclen' - 1] [0 .. ]

        poke' (n, m) = do
            let r = theData' DVS.! (n + 0)
                g = theData' DVS.! (n + 1)
                b = theData' DVS.! (n + 2)
            val565 <- rgb8888To565 r g b
            pokeElemOff ary' m val565
    forM_ ns poke'

copyJPImageToPixelArray8888_RGBAOld :: DynamicImage -> Ptr CUChar -> IO ()
copyJPImageToPixelArray8888_RGBAOld img ary = do
    let imgrgba = convertRGB8 img
        -- actually we know that it's 512 * 512 * 4
        -- eh 3
        veclen' = DVS.length theData'
        theData' = dImgData imgrgba
        ary' = castPtr ary :: Ptr Word8
        poke' n = pokeElemOff ary' n $ (theData' DVS.! n)

    forM_ [0 .. veclen' - 1] poke'

copyRaw8888To565Old :: Log -> Ptr CUChar -> Ptr CUShort -> IO ()
copyRaw8888To565Old log from to = do
    let info' = info log
    let len = 512 * 512 * 3
        -- ns = zip [0, 4 .. len - 1] [0 .. ]
        ns = zip [0, 3 .. len - 1] [0 .. ]
        poke' (n, m) = do
            r <- peekElemOff from $ (n + 0)
            g <- peekElemOff from $ (n + 1)
            b <- peekElemOff from $ (n + 2)
            val565 <- rgb8888To565 r g b
                -- info' $ printf "len %d n %d m %d val565 %s" len n m (show val565)
            pokeElemOff to m val565
    forM_ ns poke'

