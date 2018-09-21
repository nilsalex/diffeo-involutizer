module PDEProlongation where

import Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.List
import MultiIndex
import PDE

prolongation :: (Num a, Eq a) => Int -> PDE a -> PDE a
prolongation idep pde = addPDEs prolong1 prolong2
                        where prolong1 = idepProlongation idep pde
                              prolong2 = depProlongation idep pde

idepProlongation :: (Num a, Eq a) => Int -> PDE a -> PDE a
idepProlongation idep = pdeFromMap . M.map (coeffDerivative idep) . getPDEMap

depProlongation :: (Num a, Eq a) => Int -> PDE a -> PDE a
depProlongation idep = pdeFromMap . M.mapKeys (addIndex idep) . getPDEMap

prolongations :: (Num a, Eq a) => Int -> PDE a -> S.Seq (PDE a)
prolongations ideps pde = S.fromList $ map (\i -> prolongation i pde) [0..ideps - 1]

systemProlongations :: (Num a, Eq a) => Int -> PDESystem a -> PDESystem a
systemProlongations ideps (PDESys pdesys) = PDESys $ join $ fmap (prolongations ideps) pdesys
