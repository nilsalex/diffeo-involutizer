module PDEProlongation where

import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import MultiIndex
import PDE

prolongation :: (Num a, Eq a) => Int -> PDE a -> PDE a
prolongation idep pde = addPDEs prolong1 prolong2
                        where prolong1 = idepProlongation idep pde
                              prolong2 = depProlongation idep pde

idepProlongation :: (Num a, Eq a) => Int -> PDE a -> PDE a
idepProlongation idep pde = pdeFromMap (getIdeps pde) $ M.map (coeffDerivative idep) $ getPDEMap pde

depProlongation :: (Num a, Eq a) => Int -> PDE a -> PDE a
depProlongation idep pde = pdeFromMap (getIdeps pde) $  M.mapKeys (addIndex idep) $ getPDEMap pde

prolongations :: (Num a, Eq a) => PDE a -> S.Seq (PDE a)
prolongations pde = S.fromList $ map (\i -> prolongation i pde) [0..(getIdeps pde) - 1]

systemProlongations :: (Num a, Eq a) => PDESystem a -> PDESystem a
systemProlongations pdesys = PDESys (getSysIdeps pdesys) $ join $ fmap prolongations $ getPDESequence pdesys
