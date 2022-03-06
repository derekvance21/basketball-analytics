module Basketball where

import Text.Parsec

type Team = String
type Number = String

data Player = Player Team Number deriving (Show)

data Location
  = Rim
  | Mid
  | Three
  deriving (Show)

data Result
  = Make (Maybe Foul) (Maybe Rebound)
  | Miss (Maybe Foul) (Maybe Rebound)
  deriving (Show)

data Rebound
  = OReb
  | DReb
  deriving (Show)

-- should a `Foul` have the context of a `Shot`, or should a `Play` be able to be just a `Foul`. I'm going with the former for now
-- Made/Attempted
data Foul = Foul Integer Integer deriving (Show)

data Play
  = TO
  | Shot Location Result
  | Bonus Foul (Maybe Rebound)
  deriving (Show)

-- Might call this GamePlay or GPlay, as in, Play that happens within a game
data PPlay = PPlay Player Play deriving (Show)

type Parser = Parsec String ()

team :: Parser Team
team = many1 letter

number :: Parser Number
number = count 2 digit

player :: Parser Player
player = Player <$> (team <* space) <*> number

location :: Parser Location
location = choice
  [ Rim   <$ string "Rim"
  , Mid   <$ string "Mid"
  , Three <$ string "Three"
  ]

rebound :: Parser Rebound
rebound = OReb <$ string "OR" <|> DReb <$ string "DR"

-- TODO: Right now, if a FT shooter goes 1/2, it's unclear if ended in defensive rebound or make, so would have to explicitly say DR after
--       I don't like this, so need to change it
foul :: Parser Foul
foul = string "FT" *> space *> (Foul <$> (read <$> ((:[]) <$> digit) <* char '/') <*> (read <$> ((:[]) <$> digit)))

missFG :: Parser Result
missFG = Miss Nothing <$> (string "Miss" *> option (Just DReb) (Just <$> (space *> rebound)))

miss :: Parser Result
miss = missFG <|> (Miss <$> (Just <$> foul) <*> optionMaybe (space *> rebound))

make :: Parser Result
make = Make <$> (string "Make" *> optionMaybe foul) <*> optionMaybe (space *> rebound)

result :: Parser Result
result = try make <|> miss

shot :: Parser Play
shot = Shot <$> (location <* space) <*> result

turnover :: Parser Play
turnover = TO <$ string "TO"

bonus :: Parser Play
bonus = Bonus <$> (string "Bonus" *> space *> foul) <*> optionMaybe (space *> rebound)

play :: Parser Play
play = choice
  [ try shot
  , turnover
  , bonus
  ]

pplay :: Parser PPlay
pplay = PPlay <$> player <*> (space *> play) <* eof

comment :: Parser String
comment = char '(' >> manyTill anyChar (try (char ')'))

type Score = Integer

playScore :: Play -> Score
playScore (Shot location result) = 
  case result of 
    Miss mFoul _ -> 0 + mFoulScore mFoul
    Make mFoul _ -> 
      case location of Three -> 3
                       _ -> 2
      + mFoulScore mFoul
  where mFoulScore mFoul = case mFoul of Nothing -> 0
                                         (Just (Foul made _)) -> made 
playScore (Bonus (Foul made _) _) = made
playScore _ = 0

testScore input = do
  case parse pplay "" input of
    Left err -> -1
    Right (PPlay _ play) -> playScore play