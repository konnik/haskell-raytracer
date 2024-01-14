{-# LANGUAGE OverloadedRecordDot #-}

module Raytracer (
    renderScene,
    Camera (..),
    Light (..),
    Object (..),
    Scene (..),
    Color,
    Material (..),
) where

import Data.Function (on)
import Data.List (foldl', sortBy)
import Data.Maybe (listToMaybe, mapMaybe)
import Vec3
import Prelude hiding (negate)

type Color = (Double, Double, Double)

data Camera = Camera
    { eye :: Vec3
    , target :: Vec3
    , fieldOfView :: Double
    , up :: Vec3
    }
    deriving (Show)

data Light
    = DirectionalLight Vec3 Color
    deriving (Show)

data Object
    = Sphere Vec3 Double Material
    | Plane Vec3 Vec3 Material
    deriving (Show)

-- https://en.wikipedia.org/wiki/Phong_reflection_model
data Material = Material
    { color :: Color
    , specular :: Double -- ks
    , diffuse :: Double -- kd
    -- , ambient :: Double -- ka
    , shininess :: Double -- alpha
    }
    deriving (Show)

data Scene = Scene
    { camera :: Camera
    , ambientLight :: Color
    , lights :: [Light]
    , objects :: [Object]
    }
    deriving (Show)

data Line = Line Vec3 Vec3 deriving (Show)

renderScene :: Int -> Int -> Scene -> [Color]
renderScene width height scene =
    let
        -- https://en.wikipedia.org/wiki/Ray_tracing_(graphics)
        pE = (0, 0, 0) :: Vec3
        pT = (0, 0, 1) :: Vec3
        fov = pi / 2 :: Double
        v = scene.camera.up

        k :: Int
        k = width

        m :: Int
        m = height

        --
        v, t, b :: Vec3

        t = pT `minus` pE
        b = v `cross` t -- obs! enligt wikipedia är det t x v
        tn, bn, vn :: Vec3
        tn = normalize t
        bn = normalize b
        vn = bn `cross` tn -- obs! enligt wikipedia är det tn x bn
        d :: Double
        d = 1

        gx :: Double
        gx = d * tan (fov / 2)

        gy :: Double
        gy = gx * ((fromIntegral m - 1) / (fromIntegral k - 1))

        qx :: Vec3
        qx = bn `scaledBy` ((2 * gx) / (fromIntegral k - 1))

        qy :: Vec3
        qy = vn `scaledBy` ((2 * gy) / (fromIntegral m - 1))

        p1m :: Vec3
        p1m = ((tn `scaledBy` d) `minus` (bn `scaledBy` gx)) `minus` (vn `scaledBy` gy)

        pij :: Int -> Int -> Vec3
        pij i j = p1m `plus` (qx `scaledBy` (fromIntegral i - 1)) `plus` (qy `scaledBy` (fromIntegral j - 1))
     in
        do
            y <- [1 .. m]
            x <- [1 .. k]
            let ray = Line pE (normalize $ pij x y)
            pure $ renderRay ray scene

renderRay :: Line -> Scene -> Color
renderRay ray scene =
    case closestIntersection ray scene.objects of
        Nothing -> (0, 0, 0)
        Just (s, (_eyeDist, intersectionPoint, surfaceNormal)) ->
            let
                lightContrib :: Light -> Color
                lightContrib = phong ray scene.objects intersectionPoint surfaceNormal (objMaterial s)

                contributons :: [Color]
                contributons = fmap lightContrib scene.lights
             in
                foldl' plus scene.ambientLight contributons
  where
    objMaterial (Sphere _ _ m) = m
    objMaterial (Plane _ _ m) = m

-- (Line _eye rayDir) surfacePoint surfaceNormal material ambientColor light

findIntersection :: Line -> Object -> [(Double, Vec3, Vec3)]
findIntersection line obj =
    case obj of
        Sphere center radius _ -> lineSphereIntersection line center radius
        Plane center normal _ -> linePlaneIntersection line center normal

-- https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
lineSphereIntersection :: Line -> Vec3 -> Double -> [(Double, Vec3, Vec3)]
lineSphereIntersection (Line o un) c r
    | delta < 0 = []
    | delta > 0 = [(d1, i1, n1), (d2, i2, n2)] -- two intersections
    | otherwise = [(d1, i1, n1)] -- one intersection
  where
    a = un `dot` (o `minus` c)
    b = magnitude (o `minus` c)
    delta = a * a - (b * b - r * r)
    d1 = -a - sqrt delta
    i1 = o `plus` (un `scaledBy` d1)
    n1 = normalize $ i1 `minus` c
    d2 = -a + sqrt delta
    i2 = o `plus` (un `scaledBy` d2)
    n2 = normalize $ i2 `minus` c

closestIntersection :: Line -> [Object] -> Maybe (Object, (Double, Vec3, Vec3))
closestIntersection line objects =
    listToMaybe $
        sortBy (compare `on` distance) $
            filter (\x -> distance x > 0) $
                concatMap (\s -> fmap (s,) (findIntersection line s)) objects
  where
    distance (_, (d, _, _)) = d

-- https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection
linePlaneIntersection :: Line -> Vec3 -> Vec3 -> [(Double, Vec3, Vec3)]
linePlaneIntersection (Line l0 l) p0 n
    | ln == 0 = [] -- line and plane are parallell
    | otherwise = [(d, l0 `plus` (l `scaledBy` d), n)]
  where
    ln = l `dot` n
    d = ((p0 `minus` l0) `dot` n) / ln

-- https://en.wikipedia.org/wiki/Phong_reflection_model
phong :: Line -> [Object] -> Vec3 -> Vec3 -> Material -> Light -> Color
phong ray objects surfacePoint surfaceNormal material light =
    case light of
        DirectionalLight dir lightColor ->
            let
                shadow = case closestIntersection (Line (surfacePoint `plus` (surfaceNormal `scaledBy` 0.001)) (negate dir)) objects of
                    Just _ -> True
                    Nothing -> False

                (Line _eye rayDir) = ray
                l = negate dir
                n = surfaceNormal
                v = negate rayDir
                r = (n `scaledBy` (2 * (l `dot` n))) `minus` l

                diffuseDot = (l `dot` n)
                diffuse =
                    if diffuseDot > 0 && not shadow
                        then material.diffuse * diffuseDot
                        else 0

                specularDot = (r `dot` v)
                specular =
                    if specularDot > 0 && diffuseDot > 0 && not shadow
                        then material.specular * (specularDot ** material.shininess)
                        else 0
             in
                hadamard material.color (lightColor `scaledBy` diffuse)
                    `plus` (lightColor `scaledBy` specular)
