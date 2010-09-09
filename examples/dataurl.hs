
import Graphics.Rendering.HSparklines

dp :: [Float]
dp = [24,21,32.3,24,15,34,43,55,57,72,74,75,73,72,55,44]

main :: IO ()
main = do
  im <- make (barSpark {minMarker = False, maxMarker = False}) dp
  gi <- encodePngAsDataUrl im
  writeFile "dataurl.html" ("<p>This sparkline <img src=\"data:image/png;base64,"
                            ++ gi ++ "\" /> is done inline.</p>")
