module Index where

newtype IGUp = IGUp { unIGUp :: Int } deriving (Eq, Show, Read, Ord)
newtype IGDown = IGDown { unIGDown :: Int } deriving (Eq, Show, Read, Ord)

newtype ISUp = ISUp { unISUp :: Int } deriving (Eq, Show, Read, Ord)
newtype ISDown = ISDown { unISDown :: Int } deriving (Eq, Show, Read, Ord)

newtype IS2Up = IS2Up { unIS2Up :: Int } deriving (Eq, Show, Read, Ord)
newtype IS2Down = IS2Down { unIS2Down :: Int } deriving (Eq, Show, Read, Ord)

newtype IS3Up = IS3Up { unIS3Up :: Int } deriving (Eq, Show, Read, Ord)
newtype IS3Down = IS3Down { unIS3Down :: Int } deriving (Eq, Show, Read, Ord)

fromIS2Up :: IS2Up -> IGUp
fromIS2Up = IGUp . unIS2Up

fromIS2Down :: IS2Down -> IGDown
fromIS2Down = IGDown . unIS2Down

fromIGUp :: IGUp -> IS2Up
fromIGUp = IS2Up . unIGUp

fromIGDown :: IGDown -> IS2Down
fromIGDown = IS2Down . unIGDown
