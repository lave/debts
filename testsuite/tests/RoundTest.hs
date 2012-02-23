module RoundTest
where

import Test.HUnit
import Round


roundToTest = test [
    "round to 1" ~:
        [1, 1, 1, 2, -1, -1, -1, -2] ~=?
        map (roundTo 1) [0.9, 1.0, 1.1, 1.5, -0.9 ,-1.0, -1.1, -1.5]
    ]


smartRoundTest = test [
    "no flips" ~:
        [-10, -20, 30] ~=?
        smartRound 10 [-11, -18, 29],

    "flip side with smallest difference" ~:
        [-10, -20, 30] ~=?
        smartRound 10 [-13, -14, 27],

    "flip side with smallest difference" ~:
        [-10, -10, 20] ~=?
        smartRound 10 [-13, -13, 26],

    "first side with smallest difference flipped first" ~:
        [-20, -10, 30] ~=?
        smartRound 10 [-14, -14, 28],

    "flip several sides" ~:
        [-20, -20, -10, -10, -10, 70] ~=?
        smartRound 10 [-14, -14, -14, -14, -14, 70],

    "payers have priority" ~:
        [10, 10, -20] ~=?
        smartRound 10 [13, 14, -27],

    "several payers both have priority" ~:
        [10, 10, 10, 10, -10, -30] ~=?
        smartRound 10 [14, 14, 14, 14, -18, -38]
    ]


tests = test [roundToTest, smartRoundTest]

