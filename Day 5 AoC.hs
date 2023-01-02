import System.IO

convertToInt :: [String] -> (Int, Int, Int)
convertToInt x = (read (x!!1) :: Int, (read (x!!3) :: Int) - 1, (read (x!!5) :: Int) - 1)

movedB x (a, b, c) = reverse (drop a (reverse (x!!b)))
movedC x (a, b, c) = x!!c ++ reverse (take a (reverse (x!!b)))
zipfunny x = zip [0..10] x
movedH (x, y) (a, b, c) n
 | x == b = reverse (drop a (reverse y))
 | x == c = y ++ take a (reverse (n!!b))
 | otherwise = y

moved x (a, b, c) = [movedH (x1, x2) (a, b, c) x | (x1, x2) <- zipfunny x]

final x y = foldl (\x y -> moved x y) x (map convertToInt y)

movedH2 (x, y) (a, b, c) n
 | x == b = reverse (drop a (reverse y))
 | x == c = y ++ reverse(take a (reverse (n!!b)))
 | otherwise = y

moved2 x (a, b, c) = [movedH2 (x1, x2) (a, b, c) x | (x1, x2) <- zipfunny x]

final2 x y = foldl (\x y -> moved2 x y) x (map convertToInt y)

main = do
  x <- readFile "input.txt"
  let x1 = map words (lines x)
  print x1
  print(" ")
  print(moved x1 (2, 3, 5))
  print(zipfunny x1)
  x2 <- readFile "input2.txt"
  let x21 = lines x2
  let x22 = map words x21
  print(head x22)
  print(map last (final x1 x22))
  print(map last (final2 x1 x22))
