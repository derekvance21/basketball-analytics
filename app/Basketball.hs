module Basketball where

import Text.Parsec
import Data.Either (fromRight)

type Team = String
type Number = String

data Player = Player Team Number deriving (Show, Eq)

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

type Game = [PPlay]

type Parser = Parsec String ()

team :: Parser Team
team = many1 letter <?> "Team"

number :: Parser Number
number = count 2 digit <?> "Number"

player :: Parser Player
player = Player <$> (team <* char ' ') <*> number

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
foul = string "FT" *> char ' ' *> (Foul <$> (read <$> ((:[]) <$> digit) <* char '/') <*> (read <$> ((:[]) <$> digit)))

miss :: Parser Result
miss = do
  string "Miss"
  mFoul <- optionMaybe (try $ char ' ' *> foul)
  let defaultReb = case mFoul of Nothing -> Just DReb
                                 Just _  -> Nothing
  mReb <- option defaultReb (Just <$> (char ' ' *> rebound))
  pure (Miss mFoul mReb)

make :: Parser Result
make = Make <$> (try (string "Make") *> optionMaybe (char ' ' *> foul)) <*> optionMaybe (char ' ' *> rebound)

result :: Parser Result
result = make <|> miss

shot :: Parser Play
shot = Shot <$> (location <* char ' ') <*> result

turnover :: Parser Play
turnover = TO <$ string "TO"

bonus :: Parser Play
bonus = Bonus <$> (string "Bonus" *> char ' ' *> foul) <*> optionMaybe (char ' ' *> rebound)

play :: Parser Play
play = choice
  [ try shot
  , turnover
  , bonus
  ]

pplay :: Parser PPlay
pplay = PPlay <$> player <*> (char ' ' *> play)

comment :: Parser String
comment = char '(' >> manyTill anyChar (try (char ')'))

game :: Parser Game
game = pplay `sepEndBy` endOfLine

type Score = Integer

playScore :: Play -> Score
playScore (Shot location result) =
  case result of
    Miss mFoul _ -> mFoulScore mFoul
    Make mFoul _ -> mFoulScore mFoul + case location of Three -> 3
                                                        _ -> 2
  where mFoulScore mFoul = case mFoul of Nothing -> 0
                                         Just (Foul made _) -> made
playScore (Bonus (Foul made _) _) = made
playScore _ = 0

gameFinalScore :: Team -> Team -> Game -> (Score, Score)
gameFinalScore team1 team2 =
  foldl (\(score1, score2) (PPlay (Player team _) play) ->
    let score = playScore play in
      if team == team1 then
        (score1 + score, score2)
      else
        if team == team2 then
          (score1, score2 + score)
        else
          (score1, score2))
    (0, 0)

type Possession = [PPlay]

getPossessions :: Team -> Possession -> Game -> [Possession]
getPossessions currTeam currPoss (pplay@(PPlay (Player pteam _) _):restGame) =
  if pteam == currTeam then getPossessions currTeam (pplay:currPoss) restGame
  else currPoss:getPossessions pteam [pplay] restGame
getPossessions _ currPoss [] = [currPoss]

possessions :: Game -> [Possession]
possessions game@((PPlay (Player team _) _):_) = getPossessions team [] game
possessions [] = []

testGameFile :: (Show a) => String -> (Game -> a) -> IO ()
testGameFile filename f = do
  contents <- readFile filename
  let g = fromRight [] $ parse game "" contents
  print $ f g

filterTeamPossessions :: Team -> [Possession] -> [Possession]
filterTeamPossessions team = filter (\((PPlay (Player t _) _):_) -> t == team)

playerPlays :: Player -> Game -> Game
playerPlays player = filter (\(PPlay p _) -> p == player)

teamPlays :: Team -> Game -> Game
teamPlays team = filter (\(PPlay (Player t _) _) -> t == team)

gameScore :: Game -> Score
gameScore = foldl (\acc (PPlay _ play) -> acc + playScore play) 0
