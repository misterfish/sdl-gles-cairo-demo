{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Graphics.SGCDemo.Glyph ( initGlyphTexture, loadGlyph ) where

import           Prelude hiding ( log )
-- import           Data.ByteString            as BS   ( writeFile )

import           Control.Monad ( unless, forM )
import           Data.StateVar ( ($=) )
import           Foreign ( Ptr
                         , copyArray
                         , mallocArray
                         , alloca, peek, plusPtr
                         , pokeArray, peekArray, castPtr )
import           Foreign.C ( withCString )
import           Foreign.C.Types ( CChar, CUChar, CUShort )

import           Graphics.Rendering.FTGL ( RenderMode (Front)
                                         , createTextureFont
                                         , setFontFaceSize
                                         , renderFont
                                         )

import           Graphics.Rendering.FreeType.Internal ( ft_Init_FreeType
                                                      , ft_New_Face
                                                      , ft_Set_Pixel_Sizes
                                                      , ft_Get_Char_Index
                                                      , ft_Load_Glyph
                                                      , ft_Render_Glyph )
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes ( FT_Error
                                                                     , FT_Glyph_Format
                                                                     , ft_GLYPH_FORMAT_COMPOSITE
                                                                     , ft_GLYPH_FORMAT_OUTLINE
                                                                     , ft_GLYPH_FORMAT_PLOTTER
                                                                     , ft_GLYPH_FORMAT_BITMAP
                                                                     , ft_RENDER_MODE_NORMAL )
import           Graphics.Rendering.FreeType.Internal.Bitmap ( buffer )
import qualified Graphics.Rendering.FreeType.Internal.Bitmap as FB ( width, rows, pitch, num_grays, pixel_mode, palette_mode)
import           Graphics.Rendering.FreeType.Internal.BitmapSize ( FT_Bitmap_Size )
import           Graphics.Rendering.FreeType.Internal.Face ( glyph
                                                           , num_fixed_sizes
                                                           , num_glyphs
                                                           , available_sizes
                                                           )
import           Graphics.Rendering.FreeType.Internal.FaceType ( FT_Face )
import           Graphics.Rendering.FreeType.Internal.GlyphSlot ( bitmap, bitmap_left, bitmap_top, format, metrics )
import           Graphics.Rendering.FreeType.Internal.Library ( FT_Library )

import           Graphics.Rendering.OpenGL as GL
                 ( TextureTarget2D ( Texture2D )
                 , TextureSize2D ( TextureSize2D )
                 , TextureObject (TextureObject)
                 , TextureUnit (TextureUnit)
                 , DataType ( Double, UnsignedByte, Float, UnsignedShort565 )
                 , Proxy ( NoProxy, Proxy )
                 , PixelData ( PixelData )
                 , PixelFormat ( RGB, Red )
                 , PixelInternalFormat ( RGB'
                                       , R8
                                       , RGBA8 )
                 , PixelStoreDirection ( Unpack, Pack )
                 , TextureFilter ( Nearest, Linear' )
                 , TextureCoordName ( S, T, R, Q )
                 , Clamping ( Repeat, ClampToEdge, Clamp )
                 , Repetition ( Repeated )
                 , Capability ( Enabled, Disabled )
                 , activeTexture
                 , color
                 , texture
                 , genObjectNames
                 , rowAlignment
                 , textureBinding
                 , textureFilter
                 , textureWrapMode
                 , texImage2D )

import           Graphics.SGCDemo.Types ( App (App)
                                        , appLog
                                        , tex8888GLPixelData
                                        )

import           Graphics.SGCDemo.Util ( wrapGL
                                       , color4
                                       , frint )


-- returns a texture name with the glyph loaded.
-- behavior undefined if no new texture can be allocated.

-- xxx -- 565
initGlyphTexture app = do
    let log = appLog app

    -- theNewWay

    [texName] <- wrapGL log "generating glyph texture object" $ genObjectNames 1
    -- wrapGL log "texture enabled" $ texture Texture2D $= Enabled
    let TextureObject id' = texName
    putStrLn $ "id: " ++ (show $ id' - 1)
    pure texName

xloadGlyph app texName ch px t = do
    theNewWay

loadGlyph app texName ch px t = do
    let log = appLog app
        -- width = 256
        -- height = 256
        level' = 0

        format' = GL.Red
        storage' = GL.UnsignedByte
        internalFormat = GL.R8

        -- format' = GL.RGB
        -- storage' = GL.UnsignedShort565
        -- internalFormat = GL.RGB' -- 565

        gl = PixelData format' storage'

        TextureObject texId' = texName

    bmp <- getBmp app ch px

    --let width' = frint . floor . (/2.0) . frint $ t
    --    height' = 128
    --let width'  = frint $ FB.width bmp
     --   height' = frint $ FB.rows bmp
    let width'  = frint $ (FB.width bmp + frint t)
        height' = frint $ FB.rows bmp
        -- height' = frint 256
        width'' = frint width'
        height'' = frint height'
        --p = 4 - width' `mod` 4
        --nw = p + frint width'
        --nw = frint width''

    -- bmp <- peekArray (frint width'' * frint height'') $ buffer bmp
    -- let data' = addPadding p w 0 bmpData
    let buf = buffer bmp

    allen <- mallocArray (128 * 128) :: IO (Ptr CChar)
    copyArray allen buf (128 * 128)

    let stuff = gl buf

    wrapGL log "rowAlignment" $ rowAlignment Unpack $= 1

    -- wrapGL log "set activeTexture" $ activeTexture $= TextureUnit (texId' - 1)
    wrapGL log "set activeTexture" $ activeTexture $= TextureUnit 0
    wrapGL log "textureBinding" $ textureBinding Texture2D $= Just texName
    -- putStrLn "copy texture"
    wrapGL log "texImage2D" $ texImage2D Texture2D NoProxy level' internalFormat (TextureSize2D width' height') 0 stuff

    wrapGL log "textureFilter" $ textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    --wrapGL log "textureFilter" $ textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
    wrapGL log "textureWrapMode" $ textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    wrapGL log "textureWrapMode" $ textureWrapMode Texture2D T $= (Repeated, ClampToEdge)


    pure ()

getBmp app ch px = do
    -- let path = "/home/fritz/.fonts/Raleway-Black.ttf"
    let path = "/usr/share//fonts/truetype/dejavu/DejaVuSansMono.ttf"
    ft <- freeType
    --putStrLn "get ff"
    ff <- fontFace ft path
    --putStrLn "set sizes"
    runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral px) 0
    -- unicode char index.
    --putStrLn "get chNdx"
    chNdx <- ft_Get_Char_Index ff . fromIntegral . fromEnum  $ ch
    --putStrLn $ "got chNdx " ++ show chNdx
    runFreeType $ ft_Load_Glyph ff chNdx 0
    --putStrLn "get slot"
    slot <- peek $ glyph ff

    -- Number of glyphs
    n <- peek $ num_glyphs ff
    --putStrLn $ "glyphs:" ++ show n

    fmt <- peek $ format slot
    --putStrLn $ "glyph format:" ++ glyphFormatString fmt

    --putStr "Sizes:"
    numSizes <- peek $ num_fixed_sizes ff
    --putStrLn $ "num_fixed_sizes: " ++ show numSizes
    sizesPtr <- peek $ available_sizes ff
    sizes <- forM [0 .. numSizes-1] $ \i ->
        peek $ sizesPtr `plusPtr` fromIntegral i :: IO FT_Bitmap_Size
    print sizes

    metrics' <- peek $ metrics slot
    putStrLn $ "metrics: " ++ show metrics'

    l <- peek $ bitmap_left slot
    t <- peek $ bitmap_top slot
    putStrLn $ concat [ "left:" , show l , "\ntop:" , show t ]
    -- putStrLn "render glyph"
    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL
    bmp <- peek $ bitmap slot

    putStrLn $ concat ["width:" , show $ FB.width bmp , " rows:" , show $ FB.rows bmp , " pitch:" , show $ FB.pitch bmp , " num_grays:" , show $ FB.num_grays bmp , " pixel_mode:" , show $ FB.pixel_mode bmp , " palette_mode:" , show $ FB.palette_mode bmp ]

    pure bmp

freeType :: IO FT_Library
freeType = alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p

runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r

fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr

glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString fmt
    | fmt == ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
    | fmt == ft_GLYPH_FORMAT_OUTLINE = "ft_GLYPH_FORMAT_OUTLINE"
    | fmt == ft_GLYPH_FORMAT_PLOTTER = "ft_GLYPH_FORMAT_PLOTTER"
    | fmt == ft_GLYPH_FORMAT_BITMAP = "ft_GLYPH_FORMAT_BITMAP"
    | otherwise = "ft_GLYPH_FORMAT_NONE"


addPadding :: Int -> Int -> a -> [a] -> [a]
addPadding _ _ _ [] = []
addPadding amt w val xs = a ++ b ++ c
    where a = take w xs
          b = replicate amt val
          c = addPadding amt w val (drop w xs)



theNewWay = do
    GL.color $ color4 255 255 0 255
    font <- createTextureFont "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
    setFontFaceSize font 300 300
    renderFont font "Hello world!" Front

