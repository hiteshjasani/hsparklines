
import Graphics.Rendering.HSparklines

dp :: [Float]
dp = [24,21,32.3,24,15,34,43,55,57,72,74,75,73,72,55,44]

main :: IO ()
main = do
  make (smoothSpark) dp >>= savePngFile "smooth_spark.png"
  make (smoothSpark {step = 5}) dp >>= savePngFile "smooth_spark_s5.png"
  make (smoothSpark {bgColor = rgb 0xee 0xee 0xee}) dp >>=
    savePngFile "smooth_spark_bg.png"
  make (smoothSpark {bgColor = rgb 0xee 0xee 0xee, step = 5, minMarker = False}) dp
    >>= savePngFile "smooth_spark_bg_s5.png"
