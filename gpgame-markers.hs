{-# LANGUAGE OverloadedStrings #-}
-- Copyright © 2014 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.
-- Generate SVG for laser-cut Glass Plate Game markers

import Data.Ix (index)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Blaze (toValue, toMarkup)
import Text.Blaze.Svg.Renderer.Pretty (renderSvg)
import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

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

dim :: Double -> S.AttributeValue
dim n = toValue $ show n ++ "cm"

cutCircle :: (Int, Int) -> S.Svg
cutCircle (x, y) = do
  let cx = fromIntegral x - 0.5
  let cy = fromIntegral y - 0.5
  S.circle ! A.cx (dim cx) ! A.cy (dim cy) ! A.r (dim 0.5) !
   A.fill "none" ! A.stroke "black" ! A.strokeWidth "1px"

cutText :: (Int, Int) -> String -> S.Svg
cutText (x, y) txt = do
  let tx = fromIntegral x - 0.5
  let ty = fromIntegral y - 0.5 + fontSize / 2.4
  S.text_ ! A.x (dim tx) ! A.y (dim ty) !
   A.fontSize (dim fontSize) ! A.fontFamily "sans-serif" ! 
   A.textAnchor "middle" ! A.fill "blue" $ toMarkup txt

cutAlignmentHole :: (Int, Int) -> S.Svg
cutAlignmentHole (x, y) = do
  let cx = fromIntegral x
  let cy = fromIntegral y
  S.circle ! A.cx (dim cx) ! A.cy (dim cy) ! A.r (dim 0.1) ! A.fill "black"

cutAlignmentHoles :: Int -> S.Svg
cutAlignmentHoles n = do
  cutAlignmentHole (1, n - 1)
  cutAlignmentHole (n - 1, n - 1)

makeOne :: Int -> Which -> (Int, Int) -> S.Svg
makeOne n which ix =
  let i = index ((1, 1), (n, n)) ix in
  case which of
    Numbers | i >= n^(2 :: Int) - 1 -> cutCircle ix
    Numbers -> do cutText ix $ show $ i + 1; cutCircle ix
    Front -> cutText ix "O"
    Back -> cutText ix "N"

makeMarkers :: Int -> Which -> S.Svg
makeMarkers n which = do
  case which of
    Front -> cutAlignmentHoles n
    _ -> return ()
  block (makeOne n which) [(i, j) | i <- [1..n], j <- [1..n]]
  where
    block _ [] = error "overran indices"
    block a [ix] = a ix
    block a (ix : ixs) = do _ <- a ix; block a ixs

renderMarkers :: Which -> IO ()
renderMarkers which =
  putStrLn $ renderSvg $ docHeader $ makeMarkers n which
  where
    n = case which of Numbers -> 5; _ -> 4
    docHeader =
      S.docTypeSvg ! A.version "1.1" !
       A.width (dim $ fromIntegral n) ! A.height (dim $ fromIntegral n)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [whichStr] ->
      case lookup whichStr argd of
        Just which -> renderMarkers which
        Nothing -> usage
    _ -> usage
