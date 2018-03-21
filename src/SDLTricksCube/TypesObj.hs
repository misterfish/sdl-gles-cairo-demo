{-# LANGUAGE PackageImports #-}

-- | ObjI (an internal type) is the root of the tree, built by parsing an
-- .obj file, and adding a mapping between textures and objects. Wavefront
-- .obj doesn't seem to know anything about textures -- you have to figure
-- out the mapping in e.g. blender. Each Object has one or zero textures. It
-- also has materials. Groups of vertices that share a material we call a
-- Burst. Each Burst corresponds to one drawArrays call. Before starting the
-- next Burst, the new material properties need to be sent as attributes
-- and/or uniforms. You can also optionally 'use' a new shader program if
-- desired, but be sure to send the same MVP matrix uniforms as the current
-- program. If the next Burst also starts a new Object, then the active
-- texture needs to be switched (e.g. our `Coords.activeTexture` routine) as
-- well.
--
-- Obj is the public interface.

module SDLTricksCube.TypesObj ( Public ) where

import           Data.Vector        ( Vector )
import           Data.ByteString    ( ByteString )

data Public    = Public [Sequence]

-- A sequence (e.g. walk sequence, run sequence) is a list of .obj files. At
-- some point perhaps it might be good to allow .obj files to be shared if
-- they are common to several sequences.

data Sequence  = Sequence [Obj]

data Obj       = Obj Texture [Burst]
type PngBase64 = ByteString
data Texture   = Texture PngBase64 Int Int
data Burst     = Burst Vertices TexCoords Normals Material

data Vertices  = Vertices (Vector Vertex3)
data TexCoords = TexCoords (Vector Vertex2)
data Normals   = Normals (Vector Vertex3)

data Material  = Material { name :: String
                         , specularExp :: Float
                         , ambientColor :: Vertex3
                         , diffuseColor :: Vertex3
                         , specularColor :: Vertex3 }

data Vertex2   = Vertex2 Float Float
data Vertex3   = Vertex3 Float Float Float
data Vertex4   = Vertex4 Float Float Float Float

makeInfiniteSequence :: Sequence -> Sequence
makeInfiniteSequence (Sequence s) = Sequence $ g s where
    g = concat . repeat

tailSequence :: Sequence -> Sequence
tailSequence (Sequence s) = Sequence $ tail s
