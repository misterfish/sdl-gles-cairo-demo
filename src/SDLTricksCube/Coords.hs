{-# LANGUAGE PackageImports #-}

module SDLTricksCube.Coords ( toMGC
                            , toMGCD
                            , translateX
                            , translateY
                            , translateZ
                            , scaleX
                            , scaleY
                            , scaleZ
                            , rotateX
                            , rotateY
                            , rotateZ
                            , unProjectItF
                            , vec3
                            , vec4
                            , vec3gld
                            , ver3
                            , ver4
                            , verple3
                            , verl3
                            , verl4
                            , vecl4
                            , ver3d
                            , ver3gld
                            , ver4d
                            , invertMatrix
                            , matrixMath
                            , dumpDMX
                            , invertMajor
                            , multMatrices
                            , identityMatrix
                            , frustumF
                            , orthoF
                            , transposeInvert
                            , lookAtF
                            , normalize
                            , invertMajor'
                            , unProjectItGLU
                            ) where

import           Prelude hiding ( log )

import           GHC.Float ( double2Float, float2Double )
import           Data.Foldable ( find , foldl' )
import           Control.Monad ( (<=<), unless, guard, when, forM_, forM )
import           Data.Maybe ( isNothing, isJust, fromJust )
import           Text.Printf ( printf )
import qualified Data.StateVar      as STV ( get )
import           Data.StateVar        as STV ( ($=) )
import           Foreign ( pokeElemOff, pokeArray )

import "matrix"  Data.Matrix        as DMX ( Matrix
                                           , fromList
                                           , transpose
                                           , multStd
                                           , toList )

import           Data.Stack ( Stack, stackPop )

import           Graphics.Rendering.OpenGL as GL
                 ( GLmatrix
                 , GLfloat
                 , GLdouble
                 , Position ( Position )
                 , Size ( Size )
                 , Vector3 ( Vector3 )
                 , Vector4 ( Vector4 )
                 , Vertex3 ( Vertex3 )
                 , Vertex4 ( Vertex4 )
                 , MatrixOrder ( RowMajor, ColumnMajor )
                 , MatrixMode ( Modelview, Projection )
                 , withNewMatrix
                 , preservingMatrix
                 , loadIdentity
                 , matrix
                 , getMatrixComponents
                 , project
                 , unProject
                 , matrixMode
                 )

import           SDLTricksCube.Types ( Log (Log, info, warn, err)
                                     , DMat
                                     , appLog
                                     , appMatrix
                                     )

import           SDLTricksCube.Config ( doDebug )
import           SDLTricksCube.Util ( float
                                    , frint
                                    , deg2rad
                                    , map3
                                    , stackPop'
                                    , wrapGL
                                    , vcross
                                    , vdot
                                    , vmag
                                    , v3x
                                    , v3y
                                    , v3z
                                    )

import           SDLTricksCube.Util2 ( multMatrices
                                     , identityMatrix )

inv x = (-1) * x

vec3 :: Float -> Float -> Float -> Vector3 Float
vec3 = Vector3

vec4 :: Float -> Float -> Float -> Float -> Vector4 Float
vec4 = Vector4

vec3gld :: GLdouble -> GLdouble -> GLdouble -> Vector3 GLdouble
vec3gld = Vector3

ver3 :: Float -> Float -> Float -> Vertex3 Float
ver3 x y z = Vertex3 x y z

ver3d :: Double -> Double -> Double -> Vertex3 Double
ver3d x y z = Vertex3 x y z

ver3gld :: GLdouble -> GLdouble -> GLdouble -> Vertex3 GLdouble
ver3gld x y z = Vertex3 x y z

ver4 :: Float -> Float -> Float -> Float -> Vertex4 Float
ver4 x y z w = Vertex4 x y z w

ver4d :: Double -> Double -> Double -> Double -> Vertex4 Double
ver4d x y z w = Vertex4 x y z w

verple3 :: (Float, Float, Float) -> Vertex3 Float
verple3 (x, y, z) = ver3 x y z

debug | doDebug == True = info
      | otherwise       = const . const . pure $ ()

vecl4 :: [Float] -> Vector4 Float
vecl4 [a, b, c, d] = Vector4 a b c d
vecl4 _ = error "Invalid arg to vecl4"

-- not for GLES.
-- only keeping for the matrixMath function.
modelview log = wrapGL log "getting modelview float"  $
    -- STV.get $ matrix (Just $ Modelview 0) :: IO (GLmatrix GLfloat)
    STV.get $ matrix (Just $ Modelview 0) :: IO (GLmatrix GLdouble)

dumpDMX :: Log -> String -> DMat -> IO ()
dumpDMX log tag x = info log $ printf "%s: %s" tag (show x)

-- ported from mesa.
invertMatrix :: DMat -> Maybe DMat
invertMatrix a = ret where
    inv' = [ inv0, inv1, inv2, inv3, inv4, inv5, inv6, inv7
          , inv8, inv9, inv10, inv11, inv12, inv13, inv14, inv15 ]
    [ m0, m1, m2, m3, m4, m5, m6, m7, m8
      , m9, m10, m11, m12, m13, m14, m15 ] = toList a
    inv0 =   m5*m10*m15 - m5*m11*m14 - m9*m6*m15
             + m9*m7*m14 + m13*m6*m11 - m13*m7*m10
    inv4 =  -m4*m10*m15 + m4*m11*m14 + m8*m6*m15
             - m8*m7*m14 - m12*m6*m11 + m12*m7*m10
    inv8 =   m4*m9*m15 - m4*m11*m13 - m8*m5*m15
             + m8*m7*m13 + m12*m5*m11 - m12*m7*m9
    inv12 = -m4*m9*m14 + m4*m10*m13 + m8*m5*m14
             - m8*m6*m13 - m12*m5*m10 + m12*m6*m9
    inv1 =  -m1*m10*m15 + m1*m11*m14 + m9*m2*m15
             - m9*m3*m14 - m13*m2*m11 + m13*m3*m10
    inv5 =   m0*m10*m15 - m0*m11*m14 - m8*m2*m15
             + m8*m3*m14 + m12*m2*m11 - m12*m3*m10
    inv9 =  -m0*m9*m15 + m0*m11*m13 + m8*m1*m15
             - m8*m3*m13 - m12*m1*m11 + m12*m3*m9
    inv13 =  m0*m9*m14 - m0*m10*m13 - m8*m1*m14
             + m8*m2*m13 + m12*m1*m10 - m12*m2*m9
    inv2 =   m1*m6*m15 - m1*m7*m14 - m5*m2*m15
             + m5*m3*m14 + m13*m2*m7 - m13*m3*m6
    inv6 =  -m0*m6*m15 + m0*m7*m14 + m4*m2*m15
             - m4*m3*m14 - m12*m2*m7 + m12*m3*m6
    inv10 =  m0*m5*m15 - m0*m7*m13 - m4*m1*m15
             + m4*m3*m13 + m12*m1*m7 - m12*m3*m5
    inv14 = -m0*m5*m14 + m0*m6*m13 + m4*m1*m14
             - m4*m2*m13 - m12*m1*m6 + m12*m2*m5
    inv3 =  -m1*m6*m11 + m1*m7*m10 + m5*m2*m11
             - m5*m3*m10 - m9*m2*m7 + m9*m3*m6
    inv7 =   m0*m6*m11 - m0*m7*m10 - m4*m2*m11
             + m4*m3*m10 + m8*m2*m7 - m8*m3*m6
    inv11 = -m0*m5*m11 + m0*m7*m9 + m4*m1*m11
             - m4*m3*m9 - m8*m1*m7 + m8*m3*m5
    inv15 =  m0*m5*m10 - m0*m6*m9 - m4*m1*m10
             + m4*m2*m9 + m8*m1*m6 - m8*m2*m5
    det = m0*inv0 + m1*inv4 + m2*inv8 + m3*inv12
    det' = 1.0 / det
    ret' = fromList 4 4 $ map (* det') inv'
    ret | det == 0 = Nothing
        | otherwise = Just ret'

-- a will be screen x, screen y, depth
-- y = 0 is the *bottom*
-- useful for reference (uses gluProject)
projectItGLU log mv proj vec (viewportWidth, viewportHeight) = do
    let pos' = Position 0 0
        size' = Size (frint viewportWidth) (frint viewportHeight)
    (Vertex3 vert1X vert1YFlip _) <- project vec mv proj (pos', size')
    let vert1Y = (+ (- vert1YFlip)) . frint $ viewportHeight
    pure (vert1X, vert1Y)

unProjectItGLU log mv proj ver (viewportWidth, viewportHeight) = unProject ver' mv proj (pos', size') where
    Vertex3 vx vy vz = ver
    ver' = Vertex3 vx vy' vz

    pos' = Position 0 0
    size' = Size w' h'
    vy' = frint h' - vy
    h' = frint viewportHeight
    w' = frint viewportWidth

matVec4fMult :: GLmatrix GLfloat -> Vector4 GLfloat -> IO (Vector4 GLfloat)
matVec4fMult mat (Vector4 vx vy vz vw) = do
    -- the order is the desired order, and the call will handle the
    -- conversion if necessary.
    mc <- DMX.fromList 4 4 <$> getMatrixComponents RowMajor mat
    let vc = DMX.fromList 4 1 [vx, vy, vz, vw]
    pure . vecl4 . toList $ multStd mc vc

invertMajor mat = DMX.fromList 4 4 n' where
    n' = invertMajor' m'
    m' = toList mat

invertMajor' x = x' where
    x' = [ a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p ]
    a = x !! 0
    e = x !! 1
    i = x !! 2
    m = x !! 3
    b = x !! 4
    f = x !! 5
    j = x !! 6
    n = x !! 7
    c = x !! 8
    g = x !! 9
    k = x !! 10
    o = x !! 11
    d = x !! 12
    h = x !! 13
    l = x !! 14
    p = x !! 15

-- ported from mesa.
unProjectItF app (viewportWidth, viewportHeight) ver
  | isNothing inv' = Nothing
  | otherwise = ret' where

    appmatrix = appMatrix app
    (model, view, proj) = map3 stackPop' appmatrix
    toDMXl = fromList 4 1

    -- seems backwards, but works.
    mat' = multMatrices [ model, view, proj ]
    -- mat' = multMatrices [ proj, view, model ]

    inv' = invertMatrix mat'
    Vertex3 vx vyFlip vz = ver
    vy = frint viewportHeight - vyFlip
    vw' = 1.0
    pos0 = 0
    pos1 = 0
    vx' = (vx - pos0) / (frint viewportWidth) * 2 - 1
    vy' = (vy - pos1) / (frint viewportHeight) * 2 - 1
    vz' = vz * 2 - 1
    vec' = [ vx', vy', vz', vw' ]
    inv'' = fromJust inv'
    inv''' = invertMajor inv''
    [outx', outy', outz', outw'] = DMX.toList . multStd inv''' $ toDMXl vec'
    outx'' = outx' / outw'
    outy'' = outy' / outw'
    outz'' = outz' / outw'
    ret' | outw' == 0 = Nothing
         | otherwise = Just $ Vertex3 outx'' outy'' outz''

verl3 :: [Float] -> Vertex3 Float
verl3 [a, b, c] = Vertex3 a b c

verl4 :: [Float] -> Vertex4 Float
verl4 [a, b, c, d] = Vertex4 a b c d

-- useful for doing matrix multiplication with GL matrices.
-- only for non-GLES.

matrixMath :: Log -> IO () -> IO (GLmatrix GLdouble)
matrixMath log act = do
    let info' = info log
    saveMode <- STV.get matrixMode
    matrixMode $= Modelview 0
    res <- preservingMatrix $ do
        loadIdentity
        act
        modelview log
    matrixMode $= saveMode
    pure res

toMGC :: DMat -> IO (GLmatrix GLfloat)
toMGC m = do
    let map' = (flip pokeArray) ns
        ns = DMX.toList mat
        mat = m
    withNewMatrix ColumnMajor map'

toMGR :: DMat -> IO (GLmatrix GLfloat)
toMGR m = do
    let map' = (flip pokeArray) ns
        ns = DMX.toList mat
        mat = m
    withNewMatrix RowMajor map'

toMGCD :: DMat -> IO (GLmatrix GLdouble)
toMGCD m = do
    let map' = (flip pokeArray) ns'
        ns = DMX.toList mat
        ns' = map float2Double ns
        mat = m
    withNewMatrix ColumnMajor map'

translateX :: Float -> DMat
translateX dx = DMX.fromList 4 4 m where
    m = invertMajor' n
    n = [ 1, 0, 0, dx
        , 0, 1, 0, 0
        , 0, 0, 1, 0
        , 0, 0, 0, 1 ]

translateY :: Float -> DMat
translateY dy = DMX.fromList 4 4 m where
    m = invertMajor' n
    n = [ 1, 0, 0, 0
        , 0, 1, 0, dy
        , 0, 0, 1, 0
        , 0, 0, 0, 1 ]

translateZ :: Float -> DMat
translateZ dz = DMX.fromList 4 4 m where
    m = invertMajor' n
    n = [ 1, 0, 0, 0
        , 0, 1, 0, 0
        , 0, 0, 1, dz
        , 0, 0, 0, 1 ]

rotateX :: Float -> DMat
rotateX deg = DMX.fromList 4 4 m where
    m = invertMajor' n
    n = [ 1     , 0     , 0     , 0
        , 0     , c     , inv s , 0
        , 0     , s     , c     , 0
        , 0     , 0     , 0     , 1 ]
    c = cos theta
    s = sin theta
    theta = deg2rad deg

rotateY :: Float -> DMat
rotateY deg = DMX.fromList 4 4 m where
    m = invertMajor' n
    n = [ c     , 0     , s     , 0
        , 0     , 1     , 0     , 0
        , inv s , 0     , c     , 0
        , 0     , 0     , 0     , 1 ]
    c = cos theta
    s = sin theta
    theta = deg2rad deg

rotateZ :: Float -> DMat
rotateZ deg = DMX.fromList 4 4 m where
    m = invertMajor' n
    n = [ c     , inv s , 0     , 0
        , s     , c     , 0     , 0
        , 0     , 0     , 1     , 0
        , 0     , 0     , 0     , 1 ]
    c = cos theta
    s = sin theta
    theta = deg2rad deg

scaleX :: Float -> DMat
scaleX s = DMX.fromList 4 4 m where
    m = invertMajor' n
    n = [ s, 0, 0, 0
        , 0, 1, 0, 0
        , 0, 0, 1, 0
        , 0, 0, 0, 1 ]

scaleY :: Float -> DMat
scaleY s = DMX.fromList 4 4 m where
    m = invertMajor' n
    n = [ 1, 0, 0, 0
        , 0, s, 0, 0
        , 0, 0, 1, 0
        , 0, 0, 0, 1 ]

scaleZ :: Float -> DMat
scaleZ s = DMX.fromList 4 4 m where
    m = invertMajor' n
    n = [ 1, 0, 0, 0
        , 0, 1, 0, 0
        , 0, 0, s, 0
        , 0, 0, 0, 1 ]

-- new-style: return DMXmatrix which can be converted to Gmatrix and sent to shaders.
-- symmetrical: l = -r & t = -b
frustumF :: Float -> Float -> Float -> Float -> DMX.Matrix GLfloat
frustumF r t n f = DMX.fromList 4 4 $ frustumF' r t n f

frustumF' r t n f = invertMajor' ns where
    ns = [ n/r , 0   , 0     , 0
         , 0   , n/t , 0     , 0
         , 0   , 0   , b     , a
         , 0   , 0   , inv 1 , 0 ]
    a = (inv 2) * f * n / (f - n)
    b = inv $ (f + n) / (f - n)

lookAtF :: Log -> Vertex3 Float -> Vertex3 Float -> Vector3 Float -> DMX.Matrix Float
lookAtF log v1@(Vertex3 ex ey ez) v2 v3 = m where
    m = multMatrices [ m1, m2, m3, m4 ]
    m1' = lookAtFStage1' v1 v2 v3
    m1 = DMX.fromList 4 4 m1'
    m2 = translateX . inv $ ex
    m3 = translateY . inv $ ey
    m4 = translateZ . inv $ ez

lookAtFStage1' (Vertex3 ex ey ez) (Vertex3 cx cy cz) (Vector3 ux uy uz) = ns where
    ns = invertMajor' nsOrig

    nsOrig = [ sx', ux', inv fx', 0
             , sy', uy', inv fy', 0
             , sz', uz', inv fz', 0
             , 0  , 0  , 0      , 1 ]

    fx = cx - ex
    fy = cy - ey
    fz = cz - ez
    f = Vector3 fx fy fz
    f' = normalize f
    u = Vector3 ux uy uz
    s = f' `vcross` u
    s' = normalize s
    u' = s' `vcross` f'
    sx' = v3x s'
    sy' = v3y s'
    sz' = v3z s'
    ux' = v3x u'
    uy' = v3y u'
    uz' = v3z u'
    fx' = v3x f'
    fy' = v3y f'
    fz' = v3z f'

normalize :: Vector3 Float -> Vector3 Float
normalize v@(Vector3 x y z) = Vector3 (x / mag) (y / mag) (z / mag) where
    mag = vmag v

-- symmetrical: l = -r & t = -b
-- @test
orthoF :: Float -> Float -> Float -> Float -> DMX.Matrix GLfloat
orthoF r t n f = DMX.fromList 4 4 $ orthoF' r t n f

orthoF' r t n f = invertMajor' ns where
    ns = [ 1/r, 0,   0, 0
         , 0,   1/t, 0, 0
         , 0,   0,   a, b
         , 0,   0,   0, 1 ]
    a = (inv 2) / (f - n)
    b = inv $ (f + n) / (f - n)

transposeInvert :: DMat -> Maybe DMat
transposeInvert = (DMX.transpose <$>) . invertMatrix
