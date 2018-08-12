{-# LANGUAGE PackageImports #-}

module Graphics.SGCDemo.Util ( (<|.>)
                             , checkSDLError
                             , benchStart
                             , benchUpdate
                             , sort
                             , printMatrixDouble
                             , color
                             , col8
                             , color4
                             , color3
                             , allPass
                             , frint
                             , map3
                             , mapM3
                             , stackPush'
                             , stackPop'
                             , stackPop''
                             , stackReplace'
                             , concatTuples2
                             , concatTuples3
                             , concatTuples4
                             , concatVectors3
                             , concatVectors4
                             , concatVertex1
                             , concatVertex2
                             , concatVertex3
                             , concatVertex4
                             , wrapGL
                             , wrapGLStart
                             , wrapGLEnd
                             , checkGLErrors
                             , inv
                             , deg2rad
                             , v3x
                             , v3y
                             , v3z
                             , vcross
                             , vmag
                             , vdot
                             , vdiv
                             , randoms
                             , float
                             , toDeg
                             , up3fst
                             , up3snd
                             , up3thd
                             , replaceModel
                             , replaceView
                             , replaceProj
                             , updateModel
                             , updateView
                             , updateProj
                             , pushModel
                             , pushView
                             , pushProj
                             , popModel
                             , popView
                             , popProj
                             , appReplaceModel
                             , appReplaceView
                             , appReplaceProj
                             , appUpdateModel
                             , appUpdateView
                             , appUpdateProj
                             , appMultiplyModel
                             , appMultiplyView
                             , appMultiplyProj
                             , appMultiplyRightModel
                             , appMultiplyRightView
                             , appMultiplyRightProj
                             , appUpdateMatrix
                             , fst3
                             , snd3
                             , thd3
                             , hsvCycle
                             , isMultiple
                             , isInteger
                             , replicateX
                             , nAtATime
                             , glTrue
                             , glFalse
                             , glTrueF
                             , glFalseF
                             ) where

import           Prelude hiding ( log )

import           Control.Applicative ( (<|>) )
import           Data.List ( unfoldr )
import           Data.Time ( UTCTime, diffUTCTime, getCurrentTime )
import           Data.Function ( (&) )
import qualified Data.StateVar      as STV ( get )
import           Data.Foldable ( find , foldl' )
import           Data.Word ( Word8 )
import           Data.Monoid ( (<>) )
import           System.Info   as I           ( os, arch )
import           Text.Printf                  ( printf )
import           Data.Foldable                ( find )
import           Foreign                      ( peekElemOff )
import           Foreign.C                    ( peekCString )
import           System.Random                ( getStdGen )
import qualified System.Random as SR          ( randoms )
import           Data.Stack ( stackPush, stackPop )
import "matrix"  Data.Matrix        as DMX ( multStd, fromList )

import qualified SDL.Raw.Error as SRE         ( getError
                                              , clearError )
import           Graphics.Rendering.OpenGL as GL
                 ( Vector3 ( Vector3 )
                 , Vector4 ( Vector4 )
                 , Color4 (Color4)
                 , Color3 (Color3)
                 , GLint
                 , GLfloat
                 , Vertex1 ( Vertex1 )
                 , Vertex2 ( Vertex2 )
                 , Vertex3 ( Vertex3 )
                 , Vertex4 ( Vertex4 )
                 , withMatrix
                 , errors )

import           Graphics.SGCDemo.Util2 ( multMatrices )
import           Graphics.SGCDemo.Types ( Log (Log, info, warn, err)
                                        , App
                                        , DMat
                                        , GMat
                                        , GMatD
                                        , appMatrix )
import           Graphics.SGCDemo.Config ( isEmbedded
                                         , doDebug
                                         , doBench )

data Bench = BenchDisabled
           | BenchEnabled { benchLog :: Log
                          , benchTag :: String
                          , benchStartTime :: UTCTime }

checkSDLError :: Log -> String -> IO ()
checkSDLError log tag = do
    check' =<< peekCString =<< SRE.getError
    SRE.clearError where
        check' "" = pure ()
        check' theError = err log . errString $ theError
        errString = (++) $ tag ++ ": "

allPass :: [a -> Bool] -> a -> Bool
allPass fs x = all' fs where
    all'            = check' . find not'
    not' f          = not . f $ x
    check' (Just _) = False
    check' _        = True

wrapGL log tag action = do
    wrapGLStart log tag
    ret <- action
    wrapGLEnd log tag
    pure ret

wrapGLStart log tag = debug log         $ "GL: doing " <> tag
wrapGLEnd log tag   = checkGLErrors log $ "gl: after " <> tag

debug | doDebug == True = info
      | otherwise       = const . const . pure $ ()

checkGLErrors log tag = do
    errs <- STV.get errors
    show' errs where
        show' [] = pure ()
        show' e = err log $ "gl error: " ++ tag ++ ": " ++ show e

float x = x :: Float

toDeg :: Float -> Float
toDeg = (* 180) . (/ pi)

inv x = (-1) * x

deg2rad :: Float -> Float
deg2rad = (/ 180.0) . (* pi)

vcross (Vector3 a b c) (Vector3 x y z) = Vector3 x' y' z' where
    x' = b * z - c * y
    y' = c * x - a * z
    z' = a * y - b * x

vmag (Vector3 a b c) = sqrt $ a ** 2 + b ** 2 + c ** 2

vdot (Vector3 a b c) (Vector3 x y z) = x' + y' + z' where
    x' = a * x
    y' = b * y
    z' = c * z

vdiv n (Vector3 a b c) = Vector3 a' b' c' where
    a' = a / n
    b' = b / n
    c' = c / n

v3x (Vector3 x _ _ ) = x
v3y (Vector3 _ y _ ) = y
v3z (Vector3 _ _ z ) = z

concatTuples2 :: [(a, a)] -> [a]
concatTuples2 = foldr folder' [] where
    folder' (a, b)  acc        = a : b : acc
concatTuples3 :: [(a, a, a)] -> [a]
concatTuples3 = foldr folder' [] where
    folder' (a, b, c) acc      = a : b : c : acc
concatTuples4 :: [(a, a, a, a)] -> [a]
concatTuples4 = foldr folder' [] where
    folder' (a, b, c, d) acc   = a : b : c : d : acc
concatVectors3 :: [Vector3 Float] -> [Float]
concatVectors3 = foldr fold' [] where
    fold' (Vector3 x y z) acc = x : y : z : acc
concatVectors4 :: [Vector4 Float] -> [Float]
concatVectors4 = foldr fold' [] where
    fold' (Vector4 x y z w) acc = x : y : z : w : acc

concatVertex1 :: [Vertex1 Float] -> [Float]
concatVertex1 = foldr folder' [] where
    folder' (Vertex1 a) acc = a : acc
concatVertex2 :: [Vertex2 Float] -> [Float]
concatVertex2 = foldr folder' [] where
    folder' (Vertex2 a b) acc = a : b : acc
concatVertex3 :: [Vertex3 Float] -> [Float]
concatVertex3 = foldr folder' [] where
    folder' (Vertex3 a b c) acc = a : b : c : acc
concatVertex4 :: [Vertex4 Float] -> [Float]
concatVertex4 = foldr folder' [] where
    folder' (Vertex4 a b c d) acc = a : b : c : d : acc

frint :: (Num b, Integral a) => a -> b
frint = fromIntegral

-- | return stream of random doubles from [-1, 1]
-- same stream returned per process, and not sure about edges or even
-- distribution.
randoms :: IO [Double]
randoms = do
    gen <- getStdGen
    return . map divider' . SR.randoms $ gen where
        divider' :: Int -> Double
        divider' n = fromIntegral n / fromIntegral (maxBound :: Int)

map3 f (a, b, c) = (f a, f b, f c)
mapM3 f (a, b, c) = (,,) <$> f a <*> f b <*> f c

stackPush' = flip stackPush
---- these die if the stack is empty.
stackPop'  = snd . maybe (error "stack empty") id . stackPop
stackPop'' = fst . maybe (error "stack empty") id . stackPop
stackReplace' a s = s & stackPop'' & stackPush' a
stackUpdate' f s = s' & stackPush' v' where
    (s', v) = pop'
    v' = f v
    pop' = maybe (error "stack empty") id $ stackPop s

-- good for pushing to vertices, although in the shaders we use floats.
col8 :: Integral a => a -> a -> a -> a -> (Word8, Word8, Word8, Word8)
col8 r g b a = (c r, c g, c b, c a) where
    c = frint

---- constuct their Color3/Color4 (Float) types with integral input.
color4 :: Integral a => a -> a -> a -> a -> Color4 Float
color4 r g b a = Color4 (c r) (c g) (c b) (c a) where
    c = (/ 255) . frint

color3 :: Integral a => a -> a -> a -> Color3 Float
color3 r g b = Color3 (c r) (c g) (c b) where
    c = (/ 255) . frint

color :: Integral a => a -> a -> a -> a -> Vertex4 Float
color r g b a = Vertex4 (x r) (x g) (x b) (x a) where
    x = (/ 255) . frint

up3fst f (a, b, c) = (f a, b, c)
up3snd f (a, b, c) = (a, f b, c)
up3thd f (a, b, c) = (a, b, f c)

replaceModel    = up3fst . stackReplace'
replaceView     = up3snd . stackReplace'
replaceProj     = up3thd . stackReplace'

updateModel     = up3fst . stackUpdate'
updateView      = up3snd . stackUpdate'
updateProj      = up3thd . stackUpdate'

pushModel       = up3fst . stackPush'
pushView        = up3snd . stackPush'
pushProj        = up3thd . stackPush'

popModel        = up3fst . stackPop'
popView         = up3snd . stackPop'
popProj         = up3thd . stackPop'

appReplaceModel = appUpdateMatrix . replaceModel
appReplaceView  = appUpdateMatrix . replaceView
appReplaceProj  = appUpdateMatrix . replaceProj

appUpdateModel  = appUpdateMatrix . updateModel
appUpdateView   = appUpdateMatrix . updateView
appUpdateProj   = appUpdateMatrix . updateProj

appMultiplyModel :: DMat -> App -> App
appMultiplyModel matrix' = appUpdateModel f where
    f = \model -> multMatrices [ matrix', model ]
appMultiplyView  matrix' = appUpdateView  f where
    f = \view  -> multMatrices [ matrix', view  ]
appMultiplyProj  matrix' = appUpdateProj  f where
    f = \proj  -> multMatrices [ matrix', proj  ]

appMultiplyRightModel matrix' = appUpdateModel f where
    f = \model -> multMatrices [ model, matrix' ]
appMultiplyRightView  matrix' = appUpdateView  f where
    f = \view  -> multMatrices [ view, matrix' ]
appMultiplyRightProj  matrix' = appUpdateProj  f where
    f = \proj  -> multMatrices [ proj, matrix' ]

appUpdateMatrix f app = app { appMatrix = appMatrix app & f }

benchStart log tag | doBench == False = pure BenchDisabled
benchStart log tag | doBench == True = do
    t <- getCurrentTime
    pure $ BenchEnabled { benchLog = log
                        , benchTag = tag
                        , benchStartTime = t }

benchUpdate bench | doBench == False = pure ()
benchUpdate bench | doBench == True = do
    t            <- getCurrentTime
    now'         <- getCurrentTime
    let tag'      = benchTag bench
        then'     = benchStartTime bench
        log'      = benchLog bench
        elapsed'  = show $ diffUTCTime now' then'
    info log' $ printf "«%s» elapsed: %s" tag' elapsed'

printMatrixDouble :: String -> GMatD -> IO ()
printMatrixDouble tag matrix = withMatrix matrix peek' where
    peek' order src = mapM_ (map' order src) [0 .. 15]
    map' order src' n = do
        val' <- peekElemOff src' n
        let put' | n == 0     = putStr   $ printf "%s (%s) => %.1f " tag (show order) val'
                 | n == 8     = putStrLn $ printf "%.1f " val'
                 | otherwise  = putStr   $ printf "%.1f " val'
        put'

fst3 (a, b, c) = a
snd3 (a, b, c) = b
thd3 (a, b, c) = c

hsvCycle :: Int -> Int -> (Int, Int, Int)
hsvCycle total tt
    | phase == 0 = (255, asc, 0)
    | phase == 1 = (desc, 255, 0)
    | phase == 2 = (0, 255, asc)
    | phase == 3 = (0, desc, 255)
    | phase == 4 = (asc, 0, 255)
    | phase == 5 = (255, 0, desc) where

    t = tt `mod` total
    asc  = floor $ frint x / frint tp * 255
    desc = 255 - asc
    tp = floor $ frint total / 6
    (phase, x) | t < tp * 1 = (0, t)
               | t < tp * 2 = (1, t `mod` (tp * 1))
               | t < tp * 3 = (2, t `mod` (tp * 2))
               | t < tp * 4 = (3, t `mod` (tp * 3))
               | t < tp * 5 = (4, t `mod` (tp * 4))
               | otherwise  = (5, t `mod` (tp * 5))

isMultiple :: (Integral a, Integral b) => a -> b -> Bool
isMultiple m n = isInteger $ frint n / frint m

isInteger :: RealFrac a => a -> Bool
isInteger a = (== a) . realToFrac  . floor $ a

replicateX n x = unfoldr unfold' (x, 0) where
    unfold' (v, m) | m == n = Nothing
                   | otherwise = Just (v, (v, m + 1))

-- alternate implementation.
-- replicateX n x = concat . replicate n $ [x]

-- point-free version of <|>
(a <|.> b) x = a x <|> b x

glFalse = 0 :: GLint
glTrue  = 1 :: GLint

glFalseF = 0 :: GLfloat
glTrueF  = 1 :: GLfloat


-- if it's not evenly divisible, the last element will be shorter.
nAtATime n xs
  | length right' == 0 = [left']
  | otherwise = left' : tail' where
    tail' = nAtATime n right'
    (left', right') = splitAt n xs

-- | @test
sort :: Ord a => [a] -> [a]
sort = quickSort

-- | @test
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort ss = sorted' where
    l :: Int
    l = length ss
    pivot = ss !! pivotIdx
    pivotIdx = floor $ frint l / 2
    (left, right, _) = foldl' (quickSort' pivotIdx pivot) ([], [], 0) ss
    sorted' = quickSort left <> [pivot] <> quickSort right

quickSort' pivotIdx pivot (l, r, i) x = (ll, rr, ii) where
    ii = i + 1
    (ll, rr)
      | i == pivotIdx  = (l,        r)
      | x <= pivot     = (l <> [x], r)
      | otherwise      = (l,        r <> [x])

