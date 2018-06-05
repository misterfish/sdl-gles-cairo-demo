{-# LANGUAGE OverloadedStrings #-}

module Graphics.SGCDemo.Scene ( sceneObj
                              , sceneMtl ) where

import           Prelude
import           Prelude            as P  ( readFile )
import           Data.ByteString    as BS ( ByteString, readFile )
import           Data.Text          as DT ( Text, pack )

sceneObj :: IO [Text]
sceneObj = sequence $ [
-- xxx
                      pack <$> P.readFile "/home/fritz/de/branches/graphics/ayotzinapa/ayotzinapa.obj"
                      ]

-- xxx
sceneMtl :: IO ByteString
sceneMtl = BS.readFile "/home/fritz/de/branches/graphics/ayotzinapa/ayotzinapa.mtl"
