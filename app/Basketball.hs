module Basketball where

import Text.Parsec
import Data.Either (fromRight)

type Team = String

type Number = String

data Player = Player { getTeam :: Team, getNumber :: Number } deriving (Show, Eq)

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
  | Shot { getLocation :: Location, getResult :: Result }
  | Bonus Foul (Maybe Rebound)
  deriving (Show)

data Play = Play { getPlayer :: Player, getAction :: Action } deriving (Show)

data LineP
  = PlayP Play
  | Period
  | Comment
  deriving (Show)

-- TODO: This could use the Data.List.NonEmpty module because Possessions can't be non-empty
type Possession = [Play]

type Game = [Possession]

type Score = Integer

-- TODO: the user state type `u` should be (Maybe Team), and then the quarters start as Nothing
-- TODO: the user state type `u` should be a relevant type. Maybe the file header could have the two teams, and eventually even the list of players?
--       and then every `LineP` parsed after that will check to see if that team is in the game, else it throws a parsing error 
type Parser = Parsec String ()

team :: Parser Team
team = many1 letter <?> "Team"

number :: Parser Number
number = many1 digit <?> "Number"

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
  let defaultReb = case mFoul of
        Nothing -> Just DReb
        Just _ -> Nothing
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

comment :: Parser LineP
comment = Comment <$ between (char '(') (char ')') (many (satisfy (/= ')')))

period :: Parser LineP
period = Period <$ (optional digit *> (string "OT" <|> ((:[]) <$> oneOf "QHP")) *> optional (char ' ' *> comment)) -- "QHP" for Quarter, Half, Period

possessions :: [LineP] -> Game
possessions game@((PlayP first) : _) = auxFunc [] (getTeam . getPlayer $ first) game
  where
    auxFunc possession team game = case game of -- TODO: make this a stateful computation or something to be more Haskellesque
      [] -> [possession]
      (PlayP play) : rest ->
        let t = getTeam . getPlayer $ play in
          if t == team
            then auxFunc (play : possession) team rest
            else possession : auxFunc [play] t rest
      Period : rest@(PlayP play:_) -> possession : auxFunc [] (getTeam . getPlayer $ play) rest
      Period : _ -> [possession]
      Comment : restGame -> auxFunc possession team restGame
possessions _ = []

-- utility, might be used
playsToPossessions :: [Play] -> Game
playsToPossessions = possessions . map PlayP

-- header = count 2 team

-- TODO: use a lexer to ignore spaces before each token. The file `~/haskell/sExpr/app/SExpr.hs` has an example of this

-- TODO: Parse directly into [Possession], and then type Game = [Possession], and there's an easy way to go from Game -> [Play] with concat or something
-- possession = play `chainl1` changeOfPossession -- changeOfPossession uses (:) to aggregate plays into possession. `foldMap`?
game :: Parser Game
game = possessions <$> (PlayP <$> try play <|> period <|> comment) `sepEndBy` endOfLine

-- END OF PARSER CODE --

getPlays :: Game -> [Play]
getPlays = concat

actionScore :: Action -> Score
actionScore (Shot location result) =
  case result of
    Miss mFoul _ -> mFoulScore mFoul
    Make mFoul _ ->
      mFoulScore mFoul + case location of
        Three -> 3
        _ -> 2
  where
    mFoulScore mFoul = case mFoul of
      Nothing -> 0
      Just (Foul made _) -> made
actionScore (Bonus (Foul made _) _) = made
actionScore _ = 0

playScore :: Play -> Score
playScore = actionScore . getAction

playsScore :: [Play] -> Score
playsScore = foldl (\acc play -> acc + playScore play) 0

teamGameScores :: [Team] -> [Play] -> [Score]
teamGameScores = traverse (\team -> playsScore . filter (\play -> (getTeam . getPlayer $ play) == team))

playerGameScores :: [Player] -> [Play] -> [Score]
playerGameScores = traverse (\player -> playsScore . filter ((==player) . getPlayer))

teamPossessions :: Team -> Game -> Game
teamPossessions team = filter ((==team) . getTeam . getPlayer . head)

testGameFile :: (Show a) => String -> (Game -> a) -> IO ()
testGameFile filename f = do
  contents <- readFile filename
  print . f $ fromRight [] (parse game filename contents)

-- You give it a list of plays or possessions. It counts the ones that satisfy a condition. It outputs the num satisfied, the total, and the percentage
aggregator :: (a -> Bool) -> [a] -> (Int, Int, Float)
aggregator pred xs =
  let total = length xs
      numSatisfied = length $ filter pred xs
  in (numSatisfied, total, fromIntegral numSatisfied / fromIntegral total)

oRebRateAgg :: Game -> (Int, Int, Float)
oRebRateAgg = aggregator (isOR . getAction) . filter (isReboundable . getAction) . getPlays

turnoverRateAgg :: Game -> (Int, Int, Float)
turnoverRateAgg = aggregator isTOPossession

pointsPerPossession :: Fractional a => Game -> a
pointsPerPossession game = (fromIntegral . playsScore . getPlays $ game) / (fromIntegral . length $ game)

isFGAttempt :: Action -> Bool
isFGAttempt (Shot _ (Make _ _)) = True
isFGAttempt (Shot _ (Miss Nothing _)) = True
isFGAttempt _ = False

isMadeFG :: Action -> Bool
isMadeFG (Shot _ (Make _ _)) = True
isMadeFG _ = False

isThree :: Action -> Bool
isThree (Shot Three _) = True
isThree _ = False

isTwo :: Action -> Bool
isTwo (Shot Mid _) = True
isTwo (Shot Rim _) = True
isTwo _ = False

isRim :: Action -> Bool
isRim (Shot Rim _) = True
isRim _ = False

isMid :: Action -> Bool
isMid (Shot Mid _) = True
isMid _ = False

isReboundable :: Action -> Bool
isReboundable (Shot _ (Miss Nothing _)) = True
isReboundable _ = False

isOR :: Action -> Bool
isOR (Shot _ (Miss Nothing (Just OReb))) = True
isOR _ = False

isTO :: Action -> Bool
isTO TO = True
isTO _ = False

isTOPossession :: Possession -> Bool
isTOPossession = isTO . getAction . head

oRebRate :: [Play] -> Float
oRebRate plays =
  let reboundables = filter (isReboundable . getAction) plays
  in (fromIntegral . length . filter (isOR . getAction) $ reboundables) / (fromIntegral . length $ reboundables)

getFTA :: Action -> Integer
getFTA (Shot _ (Make (Just (Foul _ a)) _)) = a
getFTA (Shot _ (Miss (Just (Foul _ a)) _)) = a
getFTA (Bonus (Foul _ a) _) = a
getFTA _ = 0

ftRate :: [Play] -> Float
ftRate plays =
  let fga = length . filter (isFGAttempt . getAction) $ plays
      fta = foldl (\acc p -> acc + (getFTA . getAction $ p)) 0 plays
  in fromIntegral fta / fromIntegral fga

statRatio :: (Play -> Bool) -> (Play -> Bool) -> [Play] -> Float
statRatio filterFn1 filterFn2 plays =
  let den = length . filter filterFn1 $ plays
      num = length . filter (and . sequence [filterFn1, filterFn2]) $ plays
  in fromIntegral num / fromIntegral den

turnoverRate :: Game -> Float
turnoverRate g = (fromIntegral . length . filter isTOPossession $ g) / (fromIntegral . length $ g)

eFG :: [Play] -> Float
eFG plays = (fg2 + 1.5 * fg3) / fga
  where fga = fromIntegral . length . filter (isFGAttempt . getAction) $ plays
        fg2 = fromIntegral . length . filter (and . sequenceA [isMadeFG, isThree] . getAction) $ plays
        fg3 = fromIntegral . length . filter (and . sequenceA [isMadeFG, isTwo] . getAction) $ plays

fourFactors :: [Team] -> String -> IO ()
fourFactors teams filename = do
  input <- readFile filename
  let g = fromRight [] (parse game filename input)
      teamGames = traverse (\team -> filter ((==team) . getTeam . getPlayer . head)) teams g
      pp100p = zip teams $ map ((*100) . pointsPerPossession) teamGames
      eFGPercent = zip teams $ map ((*100) . eFG . getPlays) teamGames
      turnoverPercent = zip teams $ map ((*100) . turnoverRate) teamGames
      oRebPercent = zip teams $ map((*100) . oRebRate . getPlays) teamGames
      ftRatePercent = zip teams $ map((*100) . ftRate . getPlays) teamGames
      stats = zip ["PP100P", "eFG%", "TO%", "OReb%", "FTA/FGA"] [pp100p, eFGPercent, turnoverPercent, oRebPercent, ftRatePercent]
  mapM_ print stats
