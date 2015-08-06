{- Copyright © 2015 Russell Dillin

anagrams is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

anagrams is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with anagrams. If not, see <http://www.gnu.org/licenses/>. -}

import Data.Char (toLower)

count :: Eq a => a -> [a] -> Int
count x []                 = 0
count x (y:ys) | x == y    = succ (count x ys)
               | otherwise = count x ys

countLetters :: String -> [Int]
countLetters xs = [ count x (map toLower xs) | x <- ['a'..'z'] ]

anagrams :: String -> String -> String
anagrams xs ys | countLetters xs == countLetters ys = "YES"
               | otherwise                          = "NO"
