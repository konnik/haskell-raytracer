module Vec3 where

type Vec3 = (Double, Double, Double)

plus :: Vec3 -> Vec3 -> Vec3
plus (x, y, z) (x', y', z') = (x + x', y + y', z + z')

minus :: Vec3 -> Vec3 -> Vec3
minus (x, y, z) (x', y', z') = (x - x', y - y', z - z')

scaledBy :: Vec3 -> Double -> Vec3
scaledBy (x, y, z) n = (n * x, n * y, n * z)

dividedBy :: Vec3 -> Double -> Vec3
dividedBy (x, y, z) n = (x / n, y / n, z / n)

dot :: Vec3 -> Vec3 -> Double
dot (x, y, z) (x', y', z') = (x * x') + (y * y') + (z * z')

cross :: Vec3 -> Vec3 -> Vec3
cross (x, y, z) (x', y', z') =
    ( y * z' - z * y'
    , z * x' - x * z'
    , x * y' - y * x'
    )

hadamard :: Vec3 -> Vec3 -> Vec3
hadamard (x, y, z) (x', y', z') = (x * x', y * y', z * z')

negate :: Vec3 -> Vec3
negate (x, y, z) = (-x, -y, -z)

magnitude :: Vec3 -> Double
magnitude (x, y, z) = sqrt $ (x * x) + (y * y) + (z * z)

normalize :: Vec3 -> Vec3
normalize v = v `dividedBy` magnitude v