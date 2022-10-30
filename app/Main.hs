-- Always imported
import Graphics.Gloss

windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (10, 10)

main :: IO ()
main = display windowDisplay white (Circle 80)

