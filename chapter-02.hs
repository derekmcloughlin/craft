module UsePictures where

import Pictures


-- Exercise 2.1
whiteHorse = invertColour horse
rotateHorse = rotate horse


-- Exercise 2.2
pattern1 = above (beside white black) (beside black white)
pattern2 = beside (above white black) (above black white)

pattern_2x2 = above (beside white black) (beside black white)
pattern_4x4 = above (beside pattern_2x2 pattern_2x2) (beside pattern_2x2 pattern_2x2)  
chessBoard = above (beside pattern_4x4 pattern_4x4) (beside pattern_4x4 pattern_4x4)  

-- Exercise 2.3
blackHorse = horse

first = above (beside blackHorse whiteHorse) (beside whiteHorse blackHorse)

second = above (beside blackHorse whiteHorse) (beside (flipV whiteHorse) (flipV blackHorse))

third = above (beside blackHorse whiteHorse) (beside (rotate whiteHorse) (rotate blackHorse))

-- Exercise 2.4

another = above (beside blackHorse whiteHorse) 
                (beside (rotate (flipV whiteHorse)) (rotate (flipV blackHorse)))
