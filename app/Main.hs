{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- Inspired by
-- https://mendo.zone/fun/beginners-haskell-bitmap-images/

-- https://www.scratchapixel.com/index.html

module Main where

import Data.ByteString (ByteString, pack)
import Data.Word (Word8)
import Graphics.Gloss
import Raytracer
import Raytracer qualified
import Vec3 (normalize)

purple :: [Word8]
purple = [128, 0, 128, 255]

redd :: [Word8]
redd = [128, 0, 0, 255]

type Pxl = (Double, Double, Double)

w, h :: Int
w = 600
h = 600

myImage :: [Pxl]
myImage =
    Raytracer.renderScene w h $
        Raytracer.Scene
            { camera =
                Camera
                    { eye = (0, 0, 0)
                    , target = (0, 0, 1)
                    , fieldOfView = pi / 2 :: Double
                    , up = (0, 1, 0)
                    }
            , ambientLight = (0, 0.1, 0.1)
            , lights =
                [ DirectionalLight (normalize (0.5, -0.5, 1)) (1, 0.5, 0.5)
                ]
            , objects =
                [ Sphere (0, 0, 3) 0.6 (1, 0.8, 0)
                , Sphere (-1, 0.3, 3) 1 (0.8, 0, 1)
                , Sphere (1, -0.3, 3) 1 (0, 0.7, 1)
                ]
            }

doubleToWord8 :: Double -> Word8
doubleToWord8 x = round (255 * clamp 0.0 1.0 x)

clamp :: Double -> Double -> Double -> Double
clamp lower upper value = max lower (min upper value)

bitmapData :: [Pxl] -> ByteString
bitmapData pixels = pack $ do
    (r, g, b) <- pixels
    [doubleToWord8 r, doubleToWord8 g, doubleToWord8 b, 255]

ourPicture :: Picture
ourPicture = bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) (bitmapData myImage) True

main :: IO ()
main = display (InWindow "Fultracer" (w, h) (100, 100)) white ourPicture