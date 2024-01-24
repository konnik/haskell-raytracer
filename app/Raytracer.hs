{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

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
import Data.Maybe (listToMaybe)
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
    | Disc Vec3 Vec3 Double Material
    deriving (Show)

-- https://en.wikipedia.org/wiki/Phong_reflection_model
data Material = Material
    { color :: Color
    , specular :: Double -- ks
    , diffuse :: Double -- kd
    , shininess :: Double -- alpha
    , reflectivity :: Double
    }
    deriving (Show)

data Scene = Scene
    { camera :: Camera
    , background :: Color
    , ambientLight :: Color
    , lights :: [Light]
    , objects :: [Object]
    }
    deriving (Show)

data Ray = Ray Vec3 Vec3 deriving (Show)

data Hit = Hit
    { object :: Object
    , distance :: Double
    , point :: Vec3
    , normal :: Vec3
    }

renderScene :: Int -> Int -> Scene -> [Color]
renderScene width height scene =
    let
        -- https://en.wikipedia.org/wiki/Ray_tracing_(graphics)
        pE = scene.camera.eye
        pT = scene.camera.target
        fov = scene.camera.fieldOfView
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
            let ray = Ray pE (normalize $ pij x y)
            pure $ renderRay ray scene 2

renderRay :: Ray -> Scene -> Int -> Color
renderRay ray scene depth =
    case closestIntersection ray scene.objects of
        Nothing -> scene.background
        Just hit ->
            let
                material = objMaterial hit.object
                lightContrib :: Light -> Color
                lightContrib = phong ray scene.objects hit.point hit.normal material

                contributons :: [Color]
                contributons = fmap lightContrib scene.lights

                color = foldl' plus scene.ambientLight contributons
             in
                if depth <= 0
                    then color
                    else
                        let
                            Ray _ v = ray
                            n = hit.normal
                            r = v `minus` (n `scaledBy` (2 * (v `dot` n)))
                            p' = hit.point `plus` (n `scaledBy` 0.01)
                            ray' = Ray p' r
                            color' = renderRay ray' scene (depth - 1)

                            reflection = material.reflectivity
                         in
                            -- (color `scaledBy` (1 - reflection)) `plus` (color' `scaledBy` reflection)
                            color `plus` (color' `scaledBy` reflection)
  where
    objMaterial (Sphere _ _ m) = m
    objMaterial (Plane _ _ m) = m
    objMaterial (Disc _ _ _ m) = m

-- (Ray _eye rayDir) surfacePoint surfaceNormal material ambientColor light

findIntersection :: Ray -> Object -> [Hit]
findIntersection line obj =
    case obj of
        Sphere center radius _ -> hitObject <$> lineSphereIntersection line center radius
        Plane center normal _ -> hitObject <$> linePlaneIntersection line center normal
        Disc center normal radius _ -> hitObject <$> lineDiscIntersection line center normal radius
  where
    hitObject (d, i, n) = Hit obj d i n

-- https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
lineSphereIntersection :: Ray -> Vec3 -> Double -> [(Double, Vec3, Vec3)]
lineSphereIntersection (Ray o un) c r
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

closestIntersection :: Ray -> [Object] -> Maybe Hit
closestIntersection line objects =
    listToMaybe $
        sortBy (compare `on` distance) $
            concatMap (filter positiveDistance . findIntersection line) objects
  where
    distance hit = hit.distance
    positiveDistance hit = hit.distance > 0

-- https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection
linePlaneIntersection :: Ray -> Vec3 -> Vec3 -> [(Double, Vec3, Vec3)]
linePlaneIntersection (Ray l0 l) p0 n
    | ln == 0 = [] -- line and plane are parallell
    | otherwise = [(d, l0 `plus` (l `scaledBy` d), n)]
  where
    ln = l `dot` n
    d = ((p0 `minus` l0) `dot` n) / ln

lineDiscIntersection :: Ray -> Vec3 -> Vec3 -> Double -> [(Double, Vec3, Vec3)]
lineDiscIntersection l p0 n radius = filter inside $ linePlaneIntersection l p0 n
  where
    inside (_, hitPoint, _) = magnitude (hitPoint `minus` p0) <= radius

-- https://en.wikipedia.org/wiki/Phong_reflection_model
phong :: Ray -> [Object] -> Vec3 -> Vec3 -> Material -> Light -> Color
phong ray objects surfacePoint surfaceNormal material light =
    case light of
        DirectionalLight dir lightColor ->
            let
                shadow = case closestIntersection (Ray (surfacePoint `plus` (surfaceNormal `scaledBy` 0.001)) (negate dir)) objects of
                    Just _ -> True
                    Nothing -> False

                (Ray _eye rayDir) = ray
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
