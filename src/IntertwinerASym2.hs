module IntertwinerASym2 where

import Index
import qualified Data.Map.Strict as Map

type AS2Indices     = (Int, Int)
type AS2IndicesUp   = (ISUp, ISUp)
type AS2IndicesDown = (ISDown, ISDown)

as2IUpfromAS2I :: AS2Indices -> AS2IndicesUp
as2IUpfromAS2I (a, b) = (ISUp a, ISUp b)

as2IDownfromAS2I :: AS2Indices -> AS2IndicesDown
as2IDownfromAS2I (a, b) = (ISDown a, ISDown b)

{- Range of spacetime dimensions -}
spacetimeRange :: [Int]
spacetimeRange = [0..3]

spacetimeRangeUp :: [ISUp]
spacetimeRangeUp = map ISUp spacetimeRange

spacetimeRangeDown :: [ISDown]
spacetimeRangeDown = map ISDown spacetimeRange

asym2Range :: [Int]
asym2Range = [0..5]

asym2RangeUp :: [IAS2Up]
asym2RangeUp = map IAS2Up asym2Range

asym2RangeDown :: [IAS2Down]
asym2RangeDown = map IAS2Down asym2Range

{- distribution of metric dof in metric spacetime tensor -}
buildAS2IndicesMap :: Map.Map Int AS2Indices
buildAS2IndicesMap = Map.fromList $ zip 
                   [0..]
                   [ (0, 1),
                     (0, 2),
                     (0, 3),
                     (1, 2),
                     (1, 3),
                     (2, 3) ]

dofIntertwiner :: Fractional a => AS2Indices -> Int -> Int -> a
dofIntertwiner (a, b) m n
                          = let summand1 = if (a == m) && (b == n) then (1 / 2) else (0)
                                summand2 = if (a == n) && (b == m) then ((-1) / 2) else (0)
                            in summand1 + summand2

{- calculate value of spacetime-to-dof projector -}
asym2Projector :: Fractional a => Map.Map Int AS2Indices -> (ISUp, ISUp) -> IAS2Down -> a
asym2Projector indicesMap (ISUp m, ISUp n) (IAS2Down a) = dofIntertwiner (indicesMap Map.! a) m n

buildASym2ProjectorMap :: Fractional a => Map.Map ((ISUp, ISUp), IAS2Down) a
buildASym2ProjectorMap = Map.fromList $ let indicesMap = buildAS2IndicesMap in
                                            do a <- asym2RangeDown
                                               m <- spacetimeRangeUp
                                               n <- spacetimeRangeUp
                                               return (((m, n), a), asym2Projector indicesMap (m, n) a)
