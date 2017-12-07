newtype ModBig = MakeModBig Integer deriving (Eq, Show)

toMB :: Integer -> ModBig
toMB = MakeModBig . (`mod` 10000000000)

fromMB (MakeModBig x) = x
instance Num ModBig where
    fromInteger = toMB
    x + y = toMB $ (fromMB x) + (fromMB y)
    x - y = toMB $ (fromMB x) - (fromMB y)
    x * y = toMB $ (fromMB x) * (fromMB y)
    abs = undefined
    signum = undefined
   

-- (^) :: (Integral a) => ModBig -> a -> ModBig 
-- a ^ 0 = toMB 1
-- a ^ b = let part = a ^ (b `quot` 2)
        -- in part * part * a ^ (b `mod` 2)
