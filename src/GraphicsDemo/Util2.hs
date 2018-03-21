{-# LANGUAGE PackageImports #-}

module GraphicsDemo.Util2 ( multMatrices
                           , identityMatrix ) where

import           Data.Foldable ( find , foldl' )
import "matrix"  Data.Matrix        as DMX ( multStd, fromList )

import           GraphicsDemo.Types ( DMat )

multMatrices :: [DMat] -> DMat
multMatrices = foldl' multStd identityMatrix

identityMatrix :: DMat
identityMatrix = DMX.fromList 4 4 [ 1, 0, 0, 0
                                  , 0, 1, 0, 0
                                  , 0, 0, 1, 0
                                  , 0, 0, 0, 1 ]

