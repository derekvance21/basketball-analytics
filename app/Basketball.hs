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

-- should a `Foul` have the context of a `Shot`, or should a `Action` be able to be just a `Foul`. I'm going with the former for now
-- Made/Attempted
data Foul = Foul Integer Integer deriving (Show)

data Action
  = TO
  | Shot Location Result
  | Bonus Foul (Maybe Rebound)
  deriving (Show)

data Play = Play Player Action deriving (Show)

type Game = [Play]

type Possession = [Play]

type Score = Integer

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

shot :: Parser Action
shot = Shot <$> (location <* char ' ') <*> result

turnover :: Parser Action
turnover = TO <$ string "TO"

bonus :: Parser Action
bonus = Bonus <$> (string "Bonus" *> char ' ' *> foul) <*> optionMaybe (char ' ' *> rebound)

action :: Parser Action
action = choice
  [ try shot
  , turnover
  , bonus
  ]

play :: Parser Play
play = Play <$> player <*> (char ' ' *> action)

comment :: Parser String
comment = char '(' >> manyTill anyChar (try (char ')'))

game :: Parser Game
game = play `sepEndBy` endOfLine

getAction :: Play -> Action
getAction (Play _ action) = action

getPlayer :: Play -> Player
getPlayer (Play player _) = player

getTeam :: Play -> Team
getTeam (Play (Player team _) _) = team

actionScore :: Action -> Score
actionScore (Shot location result) =
  case result of
    Miss mFoul _ -> mFoulScore mFoul
    Make mFoul _ -> mFoulScore mFoul + case location of Three -> 3
                                                        _ -> 2
  where mFoulScore mFoul = case mFoul of Nothing -> 0
                                         Just (Foul made _) -> made
actionScore (Bonus (Foul made _) _) = made
actionScore _ = 0

playScore :: Play -> Score
playScore = actionScore . getAction

possessions :: Game -> [Possession]
possessions game@(first : _) = auxFunc [] (getTeam first) game
  where
    auxFunc possession team game = case game of -- TODO: make this a stateful computation or something to be more Haskellesque
      [] -> [possession]
      play : rest ->
        if team == getTeam play
          then auxFunc (play : possession) team rest
          else possession : auxFunc [play] (getTeam play) rest
possessions [] = []

testGameFile :: (Show a) => String -> (Game -> a) -> IO ()
testGameFile filename f = do
  contents <- readFile filename
  print . f $ fromRight [] (parse game "" contents)

gameScore :: Game -> Score
gameScore = foldl (\acc play -> acc + playScore play) 0

teamGameScores :: [Team] -> Game -> [Score]
teamGameScores teams = map gameScore . traverse (\team -> gameFilter [\play -> getTeam play == team]) teams

gameFilter :: [Play -> Bool] -> Game -> Game
gameFilter filters = filter (and . sequenceA filters)
