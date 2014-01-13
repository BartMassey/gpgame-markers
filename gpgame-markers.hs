-- Copyright © 2014 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.
-- Generate SVG for laser-cut Glass Plate Game markers

import Data.Ix (index)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- nominal baseline-baseline font height in cm
fontSize :: Double
fontSize = 0.6

usage :: IO ()
usage = do
  hPutStrLn stderr "usage: gpgame-markers [numbers|front|back]"
  exitFailure

data Which = Numbers | Front | Back

argd :: [(String, Which)]
argd = [("numbers", Numbers), ("front", Front), ("back", Back)]

cutCircle :: (Int, Int) -> String
cutCircle (x, y) =
  let cx = fromIntegral x - (0.5 :: Double) in
  let cy = fromIntegral y - (0.5 :: Double) in
  printf "<circle cx=\"%fcm\" cy=\"%fcm\" r=\"0.5cm\" fill=\"none\" stroke=\"black\" stroke-width=\"1px\"/>\n" cx cy 

cutText :: (Int, Int) -> String -> String
cutText (x, y) txt =
  let tx = fromIntegral x - (0.5 :: Double) in
  let ty = fromIntegral y - (0.5 :: Double) + fontSize / 2.4 in
  let tag = printf "<text x=\"%fcm\" y=\"%fcm\" font-size=\"%fcm\" font-family=\"sans-serif\" text-anchor=\"middle\" fill=\"blue\">" tx ty fontSize in
  tag ++ txt ++ "</text>\n"
  

cutAlignmentHole :: (Int, Int) -> String
cutAlignmentHole (x, y) =
  let cx = fromIntegral x :: Double in
  let cy = fromIntegral y :: Double in
  printf "<circle cx=\"%fcm\" cy=\"%fcm\" r=\"0.1cm\" fill=\"black\"/>\n" cx cy 

cutAlignmentHoles :: Int -> String
cutAlignmentHoles n = 
  cutAlignmentHole (1, n - 1) ++ cutAlignmentHole (n - 1, n - 1)

makeOne :: Int -> Which -> (Int, Int) -> String
makeOne n which ix@(x, y) =
  let i = index ((1, 1), (n, n)) (y, x) in
  case which of
    Numbers | i >= n^(2 :: Int) - 1 -> cutCircle ix
    Numbers -> (cutText ix $ show $ i + 1) ++ cutCircle ix
    Front -> cutText ix "O"
    Back -> (cutText ix "N" ++ cutCircle ix)

makeMarkers :: Int -> Which -> String
makeMarkers n which =
  maybeHoles which ++
   concatMap (makeOne n which) [(i, j) | i <- [1..n], j <- [1..n]]
  where
    maybeHoles Front = cutAlignmentHoles n
    maybeHoles _ = ""

renderMarkers :: Which -> IO ()
renderMarkers which = do
  _ <- printf "<svg version=\"1.1\" width=\"%dcm\" height=\"%dcm\">\n" n n
  putStr $ makeMarkers n which
  putStrLn "</svg>"
  where
    n = case which of Numbers -> 5; _ -> 4

main :: IO ()
main = do
  args <- getArgs
  case args of
    [whichStr] ->
      case lookup whichStr argd of
        Just which -> renderMarkers which
        Nothing -> usage
    _ -> usage
