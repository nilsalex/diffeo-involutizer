module PDEProlongation where

import PDE

termProlongation :: (Integer -> Integer -> Integer) -> Integer -> Term -> [Term]
termProlongation jetMap derivative (Term { coefficient = coeff, independent = idep })
                   = case coeff of (Constant r)   -> [Term { coefficient = coeff, independent = jetMap derivative idep }]
                                   (Linear r dep) -> if derivative == dep then [Term { coefficient = Constant r, independent = idep},
                                                                                Term { coefficient = coeff, independent = jetMap derivative idep}]
                                                                          else [Term { coefficient = coeff, independent = jetMap derivative idep}]

prolongation :: (Integer -> Integer -> Integer) -> Integer -> PDE -> PDE
prolongation jetMap derivative = concat . (map (termProlongation jetMap derivative))
