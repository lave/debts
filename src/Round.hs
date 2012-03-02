module Round
where

roundTo :: RealFrac a => a -> a -> a
roundTo base = (* base) . fromIntegral . round . (/ base)

roundListTo :: RealFrac a => a -> [a] -> [a]
roundListTo base = map (roundTo base)


smartRound :: RealFrac a => a -> [a] -> [a]
smartRound base numbers
    | base <= 0 = error "base must be greater than zero"
    -- | sum numbers /= 0 = error ("sum must be 0, not " ++ (show $ sum numbers))
    | otherwise =
        map snd $ foldl flip (zip numbers rounded) [1..flipCount]
    where
        rounded = roundListTo base numbers
        totalDeviation = sum rounded
        step = if totalDeviation > 0 then (-base) else base
        flipCount = round $ abs $ totalDeviation / base

        flip numbers _ = flipped
            where
                (True, flipped) = flip' (abs totalDeviation) numbers

        --  returns pair - whether some number in the list has been flipped,
        --  and the list itself
        flip' _ [] = (False, [])
        flip' minDeviation (x:xs)
            | (isPayer x || step < 0) && deviation < minDeviation =
                case flip' deviation xs of
                    (True, l) -> (True, x : l)
                    (False, l) -> (True, flip x : l)
            | otherwise =
                (isFlipped, (x : l1))
            where
                deviation = getDeviation x
                (isFlipped, l1) = flip' minDeviation xs

                flip (raw, rounded) = (raw, rounded + step)
                getDeviation (raw, rounded) = abs $ rounded + step - raw
                isPayer (raw, _) = raw < 0

                    
