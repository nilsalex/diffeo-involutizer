module PDEProlongation where

import PDE

termProlongation :: (Integer -> Integer -> Integer) -> Integer -> Term -> [Term]
termProlongation jetMap derivative (Term { coefficient = coeff, dependent = dep })
                   = case coeff of (Constant r)    -> [Term { coefficient = coeff, dependent = jetMap derivative dep }]
                                   (Linear r idep) -> if derivative == idep then [Term { coefficient = Constant r, dependent = dep},
                                                                                  Term { coefficient = coeff, dependent = jetMap derivative dep}]
                                                                            else [Term { coefficient = coeff, dependent = jetMap derivative dep}]

prolongation :: (Integer -> Integer -> Integer) -> Integer -> PDE -> PDE
prolongation jetMap derivative = concat . (map (termProlongation jetMap derivative))
