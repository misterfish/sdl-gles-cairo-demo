-- • DynamicImage is JP's Image type, which is backed by a vector of Word8.
-- • all images must be 512 x 512 -- no boundary checks performed.
-- • there would be a noticeable performance benefit to decoding everything as
--   RGB instead of RGBA here -- alpha on the texture images could be useful
--   for some very specific case, but in general it's not necessary. after
--   uploading the image the normal alpha stuff on the CPU (fragment shader,
--   blending) would of course still be available.
-- • however, I wasn't able to draw on such surfaces with Cairo (RGB24) any
--   more, which was complaining about an invalid stride (width * 3). so
--   we're back to Cairo's ARGB32 and stride of width * 4, and decoding
--   images as RGBA.

module SDLTricksCube.Pixels ( copyJPImageToPixelArray8888_RGBA
                            , copyJPImageToPixelArray565
                            , copyRaw8888To565
                            ) where

import           Prelude hiding ( log )

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

foreign import ccall "transfer_8888_565" c_transfer_8888_565 :: Int -> Ptr CUChar -> Ptr CUShort -> IO ()
foreign import ccall "transfer_888_565" c_transfer_888_565 :: Int -> Ptr CUChar -> Ptr CUShort -> IO ()
foreign import ccall "transfer_888_8880" c_transfer_888_8880 :: Int -> Ptr CUChar -> Ptr CUChar -> IO ()
-- too much overhead, don't use.
foreign import ccall "convert_888_565" c_convert_888_565 :: CUChar -> CUChar -> CUChar -> IO CUShort

-- IO, so it can be switched out with the foreign version.
rgb888To565 r g b = do
    let l = rgb888To565Hs r g b
    pure l

copyJPImageToPixelArray8888_RGBA :: DynamicImage -> Ptr CUChar -> IO ()
copyJPImageToPixelArray8888_RGBA img ary = do
    let imgrgba = convertRGBA8 img
        n = 512 * 512 * 4
        theData' = dImgData imgrgba
        (fptr, _) = unsafeToForeignPtr0 theData'
        poke' ptr = copyArray (castPtr ary) ptr n

    withForeignPtr fptr poke'

copyJPImageToPixelArray8880_RGB :: DynamicImage -> Ptr CUChar -> IO ()
copyJPImageToPixelArray8880_RGB img ary = do
    let imgrgba = convertRGB8 img
        n = 512 * 512
        theData' = dImgData imgrgba
        (fptr, _) = unsafeToForeignPtr0 theData'
        poke' ptr = c_transfer_888_8880 n (castPtr ptr) ary
    withForeignPtr fptr poke'

copyJPImageToPixelArray565 = copyJPImageToPixelArray565C

copyJPImageToPixelArray565C :: Log -> DynamicImage -> Ptr CUShort -> IO ()
copyJPImageToPixelArray565C log img ary = do
    -- here we can save some time by skipping alpha right away, since it's
    -- thrown away immediately anyway.
    let imgrgba = convertRGB8 img
        n = 512 * 512
        theData' = dImgData imgrgba
        (fptr, _) = unsafeToForeignPtr0 theData'
        poke' ptr = c_transfer_888_565 n (castPtr ptr) ary
    withForeignPtr fptr poke'

copyRaw8888To565 :: Log -> Ptr CUChar -> Ptr CUShort -> IO ()
copyRaw8888To565 log from to = c_transfer_8888_565 len' from to where
    len' = 512 * 512

dImgData :: Image a -> DVS.Vector (PixelBaseComponent a)
dImgData (Image _ _ data')     = data'
dImgWidth (Image width' _ _ )  = width'
dImgHeight (Image _ height' _) = height'

-----------------

-- don't use: called often so overhead outweighs benefits.
rgb888To565C r g b = c_convert_888_565 rr gg bb where
    rr = frint r
    gg = frint g
    bb = frint b

copyJPImageToPixelArray565HsNew :: Log -> DynamicImage -> Ptr CUShort -> IO ()
copyJPImageToPixelArray565HsNew log img ary = do
    let imgrgba = convertRGBA8 img
        -- actually we know that it's 512 * 512 * x
        veclen' = DVS.length theData'
        theData' = dImgData imgrgba
        x = 4
        l' = floor . (/ frint x) . frint $ veclen'

    when (not $ isMultiple x veclen') . error $ printf "unexpected length: %d" veclen'

    let map' n = rgb888To565 r g b where
             r = theData' DVS.! (m + 0)
             g = theData' DVS.! (m + 1)
             b = theData' DVS.! (m + 2)
             m = n * x

    pokeArray ary =<< mapM map' [ 0 .. l'-1 ]
--     ret <- mapM map' [ 0 .. l'-1 ]
--     pokeArray ary ret

--     let print' n = do
--         putStrLn $ printf "r %d g %d b %d => %d" r g b (frint ret' :: Int) where
--              r = theData' DVS.! (m + 0)
--              g = theData' DVS.! (m + 1)
--              b = theData' DVS.! (m + 2)
--              ret' = (!! n) ret
--              m = n * x
--     mapM_ print' [0 .. 99]

copyRaw8888To565NewHs :: Log -> Ptr CUChar -> Ptr CUShort -> IO ()
copyRaw8888To565NewHs log from to = do
    copyRaw8888To565' (castPtr from, to) len' 0 where
        len' = 512 * 512

-- uses *3*
copyRaw8888To565' :: (Ptr CUChar, Ptr CUShort) -> Int -> Int -> IO ()
copyRaw8888To565' (from, to) len n
  | n == len = pure ()
  | otherwise = do
      let nxtfrom = advancePtr from 3
          nxtto   = advancePtr to 1
      [r, g, b] <- mapM (peekElemOff from) [0 .. 2]
      pokeElemOff to 0 =<< rgb888To565 r g b
      copyRaw8888To565' (nxtfrom, nxtto) len (n + 1)

-- simple conversion: drop 2 or 3 LSBs.
-- is the and-mask necessary? we are unsigned for sure.
rgb888To565Hs r g b = rss .|. gss .|. bss where
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

copyJPImageToPixelArray8888_BGRA :: DynamicImage -> Ptr CUChar -> IO ()
copyJPImageToPixelArray8888_BGRA img ary = do
    let imgrgba = convertRGBA8 img
        -- actually we know that it's 512 * 512 * x
        veclen' = DVS.length theData'
        theData' = dImgData imgrgba

    let ary' = castPtr ary :: Ptr Word8
        ns = [0, 4 .. veclen' - 1]
    forM_ ns $ \n -> do
        pokeElemOff ary' (n + 2) $ theData' DVS.! (n + 0)
        pokeElemOff ary' (n + 1) $ theData' DVS.! (n + 1)
        pokeElemOff ary' (n + 0) $ theData' DVS.! (n + 2)
        pokeElemOff ary' (n + 3) $ theData' DVS.! (n + 3)

copyJPImageToPixelArray565Old :: Log -> DynamicImage -> Ptr CUShort -> IO ()
copyJPImageToPixelArray565Old log img ary = do
    let imgrgba = convertRGBA8 img
        -- actually we know that it's 512 * 512 * x
        veclen' = DVS.length theData'
        theData' = dImgData imgrgba

        ary' = castPtr ary :: Ptr CUShort
        ns = zip [0, 3 .. veclen' - 1] [0 .. ]

        poke' (n, m) = do
            let r = theData' DVS.! (n + 0)
                g = theData' DVS.! (n + 1)
                b = theData' DVS.! (n + 2)
            val565 <- rgb888To565 r g b
            pokeElemOff ary' m val565
    forM_ ns poke'

copyJPImageToPixelArray8888_RGBAOld :: DynamicImage -> Ptr CUChar -> IO ()
copyJPImageToPixelArray8888_RGBAOld img ary = do
    let imgrgba = convertRGBA8 img
        -- actually we know that it's 512 * 512 * x
        veclen' = DVS.length theData'
        theData' = dImgData imgrgba
        ary' = castPtr ary :: Ptr Word8
        poke' n = pokeElemOff ary' n $ (theData' DVS.! n)

    forM_ [0 .. veclen' - 1] poke'

copyRaw8888To565Old :: Log -> Ptr CUChar -> Ptr CUShort -> IO ()
copyRaw8888To565Old log from to = do
    let info' = info log
    let len = 512 * 512 * 3
        ns = zip [0, 4 .. len - 1] [0 .. ]
        poke' (n, m) = do
            r <- peekElemOff from $ (n + 0)
            g <- peekElemOff from $ (n + 1)
            b <- peekElemOff from $ (n + 2)
            val565 <- rgb888To565 r g b
                -- info' $ printf "len %d n %d m %d val565 %s" len n m (show val565)
            pokeElemOff to m val565
    forM_ ns poke'

