module NumberTheory where

randomInt :: Int -> [Int]
randomInt = drop 4
          . map ((`mod` 16384) . (`div` 65536))
          . iterate ((`mod` (65536*16384)) . (1 +) . (22695477 *))
