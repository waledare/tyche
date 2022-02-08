division :: Double -> Double -> Maybe Double
division x y | y == 0 = Nothing
             | otherwise = Just (x / y)

