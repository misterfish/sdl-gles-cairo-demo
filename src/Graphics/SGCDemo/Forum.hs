{-# LANGUAGE OverloadedStrings #-}

module Graphics.SGCDemo.Forum ( forumObj
                              , forumMtl ) where

import           Data.ByteString    as BS ( ByteString )
import           Data.Text          as DT ( Text, pack )

forumObj :: IO [Text]
forumObj = sequence $ [
-- xxx
                      pack <$> readFile "/home/fritz/de/branches/graphics/ayotzinapa/ayotzinapa.obj"
                      ]

forumMtl :: ByteString
forumMtl = "# Blender MTL File: 'None'\n# Material Count: 10\n\nnewmtl Material\nNs 96.078431\nKa 1.000000 1.000000 1.000000\nKd 0.640000 0.640000 0.640000\nKs 0.500000 0.500000 0.500000\nKe 0.000000 0.000000 0.000000\nNi 1.000000\nd 1.000000\nillum 2\n\nnewmtl None\nNs 0.000000\nKa 0.000000 0.000000 0.000000\nKd 0.800000 0.800000 0.800000\nKs 0.800000 0.800000 0.800000\nKe 0.000000 0.000000 0.000000\nNi 1.000000\nd 1.000000\nillum 2\n\nnewmtl light\nNs 94.117647\nKa 1.000000 1.000000 1.000000\nKd 0.640000 0.640000 0.640000\nKs 0.500000 0.500000 0.500000\nKe 0.000000 0.000000 0.000000\nNi 1.000000\nd 1.000000\nillum 2\n\nnewmtl man\nNs 94.117647\nKa 1.000000 1.000000 1.000000\nKd 0.640000 0.640000 0.640000\nKs 0.500000 0.500000 0.500000\nKe 0.000000 0.000000 0.000000\nNi 1.000000\nd 1.000000\nillum 2\n\nnewmtl patrulla\nNs 94.117647\nKa 1.000000 1.000000 1.000000\nKd 0.640000 0.640000 0.640000\nKs 0.500000 0.500000 0.500000\nKe 0.000000 0.000000 0.000000\nNi 1.000000\nd 1.000000\nillum 2\n\nnewmtl patrulla2\nNs 94.117647\nKa 1.000000 1.000000 1.000000\nKd 0.640000 0.640000 0.640000\nKs 0.500000 0.500000 0.500000\nKe 0.000000 0.000000 0.000000\nNi 1.000000\nd 1.000000\nillum 2\n\nnewmtl red_paint\nNs 94.117647\nKa 1.000000 1.000000 1.000000\nKd 0.640000 0.640000 0.640000\nKs 0.500000 0.500000 0.500000\nKe 0.000000 0.000000 0.000000\nNi 1.000000\nd 1.000000\nillum 2\n\nnewmtl tire\nNs 94.117647\nKa 1.000000 1.000000 1.000000\nKd 0.640000 0.640000 0.640000\nKs 0.500000 0.500000 0.500000\nKe 0.000000 0.000000 0.000000\nNi 1.000000\nd 1.000000\nillum 2\n\nnewmtl van\nNs 94.117647\nKa 1.000000 1.000000 1.000000\nKd 0.640000 0.640000 0.640000\nKs 0.500000 0.500000 0.500000\nKe 0.000000 0.000000 0.000000\nNi 1.000000\nd 1.000000\nillum 2\n\nnewmtl wheel\nNs 94.117647\nKa 1.000000 1.000000 1.000000\nKd 0.640000 0.640000 0.640000\nKs 0.500000 0.500000 0.500000\nKe 0.000000 0.000000 0.000000\nNi 1.000000\nd 1.000000\nillum 2\n"
