{-# LANGUAGE BangPatterns #-}

-- | Main module
module Main
(
  -- * Methods
  addWord,
  findWords,
  getSuggestions,
  main,
  startWorker,
  usage,
  worker
) 
where

import System.Environment
import qualified Text.Read as Read
import Data.Maybe
import Debug.Trace
import Data.List.Split


-- | The beginning of everything.
main :: IO() -- ^ Ausgabe
main = do
  args <- getArgs
  if length args < 5 
    then do
      putStrLn "zu wenig Argumente spezifiziert\n"
      usage
    else
      startWorker args

-- | prints how to use the program NGramPredicts
usage :: IO()
usage = putStrLn
      "usage: NGramPredicts <number> <model> <file> <line> <column>\n\
      \where\n\
      \  number: defines the number of suggestions should be made\n\
      \  model:  defines the path to the n-gram-model in ARPA-Format\n\
      \  file:   defines the file which contains the text that should be made a suggestions for\n\
      \  line:   defines the line number in the file in which the suggestion should be made\n\
      \  column: defines the char in the line of the file in which the suggestion should be made"

-- | starts the worker -> collecting all necessary information from args
startWorker 
  :: [String] -- ^ args with at least 5 entries
  -> IO ()
startWorker args = do
  model <- readFile (args !! 1)
  file  <- readFile (args !! 2)
  print $ 
    show $ 
      worker 
        (Read.readMaybe (head args))
        model
        file
        (Read.readMaybe (args !! 3))
        (Read.readMaybe (args !! 4))
        


-- | this one is working after successful parsing arguments
worker 
  :: Maybe Int      -- ^ number of Words to suggest
  -> String   -- ^ model 
  -> String   -- ^ file
  -> Maybe Int      -- ^ line
  -> Maybe Int      -- ^ column
  -> [String] -- ^ return a list of suggested words
worker Nothing _ _ _ _ = []
worker (Just number) model file (Just line) (Just column) 
  = let words = findWords [] "" (getN (lines model)) file line column in let sugs = getSuggestions number [] [] (lines model) words
    in trace ((show $ reverse words) ++ "\n" ++ (show sugs)) $ 
      reverse $ getfirst $ sugs

worker' number model file line column = getSuggestions' number model (findWords [] "" (getN (lines model)) file line column)

-- | calculates k suggestions based on the n-1 words in präfix and the model
getSuggestions
  :: Int                -- ^ how many suggestions
  -> [(String, Float)]  -- ^ actual suggestions should be [] when called from outside 
  -> [Float]            -- ^ backoff .. should be [] when called from outside
  -> [String]           -- ^ lines of the model
  -> [String]           -- ^ n-1 prefix
  -> [(String, Float)]  -- ^ returns suggestions
getSuggestions _ s _ [] _ = s
getSuggestions k s backoff (m : odel) prefix = let calc = calcLine m backoff prefix s k in getSuggestions k (snd calc) (fst calc) odel prefix

-- | takes a line and does some calculations with this line
calcLine
  :: String                       -- ^ one line from the model
  -> [Float]                      -- ^ backoff
  -> [String]                     -- ^ prefix
  -> [(String, Float)]            -- ^ actual suggestions
  -> Int                          -- ^ how many suggestions
  -> ([Float], [(String, Float)]) -- ^ returns an updated backoff and suggestion list
calcLine line backoff prefix actSug num
  = let splitted = filter (\s -> not (s == "")) (splitOn " " line) -- split line
    in 
      if length splitted < 3 -- not a line with ngram data
      then (backoff, actSug)
      else -- there is a line to compare
        if (Read.readMaybe (splitted !! (length splitted - 1)) :: Maybe Float) == Nothing -- last is not the backoff - else, last is backoff
        then -- at least in layer n of ngram
          knowingNGramData (10 ** (read (splitted !! 0))) (tail splitted) undefined actSug backoff prefix num
        else -- previous layer
          knowingNGramData (10 ** (read (splitted !! 0))) (tail (init splitted)) (10 ** (read (splitted !! (length splitted - 1)))) actSug backoff prefix num


knowingNGramData :: Float -> [String] -> Float -> [(String, Float)] -> [Float] -> [String] -> Int -> ([Float], [(String, Float)])
knowingNGramData prob words backoff sugs boffs prefix num =
  if words == (reverse $ take (length words) prefix)
  then (boffs ++ [backoff], updateSuggestions sugs backoff)
  else knowingNotABackoff prob words backoff sugs boffs prefix num

knowingNotABackoff prob words backoff sugs boffs prefix num = 
  if init words == (reverse $ take (length words - 1) prefix)
  then matchingWordsPrefixAndPrefix prob words backoff sugs boffs prefix num
  else (boffs, sugs)

matchingWordsPrefixAndPrefix prob words backoff sugs boffs prefix num = 
  if length boffs > length (init words)
  then (boffs, returnKMostPropabalWords num ((words !! (length words - 1)), (prob * (boffs !! (length words - 1)))) sugs)
  else (boffs, returnKMostPropabalWords num ((words !! (length words - 1)), prob) sugs)

-- | updates suggestions with a new backoff
updateSuggestions
  :: [(String, Float)]      -- ^ suggestions
  -> Float                  -- ^ new backoff
  -> [(String, Float)]      -- ^ returns updated suggestions
updateSuggestions [] _            = []
updateSuggestions ((s, f) : xs) m = ((s, f * m) : updateSuggestions xs m)

-- | adds a suggestion to the suggestion set
-- update propability if suggestion already in the set
-- hold at maximum k values
addSuggestion 
  :: String             -- ^ suggestion to be added
  -> Float              -- ^ the propability of the suggestion
  -> [(String, Float)]  -- ^ set of sugggestions and their propabilities
  -> Int                -- ^ k - maximum number of values
  -> [(String, Float)]  -- ^ returns a list with suggestions
addSuggestion _ _ _ 0             = []
addSuggestion sug prob [] _       = [(sug, prob)]
addSuggestion sug prob (x : xs) k 
  = if fst x == sug
    then addSuggestion sug prob xs k
    else 
      if prob <= snd x 
      then
        if length ((sug, prob) : x : xs) <= k
        then trace (show (sug, prob)) ((sug, prob) : x : xs)
        else (x : xs)
      else
        let l = addSuggestion sug prob xs k
        in 
          if length l < k
          then x : l
          else l


returnKMostPropabalWords
  :: Int
  -> (String, Float)
  -> [(String, Float)]
  -> [(String, Float)]
returnKMostPropabalWords 0 _ _ = []
returnKMostPropabalWords _ t [] = [t]
returnKMostPropabalWords k (s, p) ((sx, px) : xs)
  | s == sx                       = returnKMostPropabalWords k (s, p + px) xs
  | k > length xs + 1  && p <= px = (s, p) : (sx, px) : xs
  | k <= length xs + 1 && p <= px = (sx, px) : xs
  | otherwise                     = 
    let kmpw = returnKMostPropabalWords k (s, p) xs in
    if length kmpw < k 
    then (sx, px) : kmpw
    else kmpw



getSuggestions' :: Int -> String -> [String] -> String
getSuggestions' n s l = "n: " ++ (show n) ++ ", s: model" ++ ", l: " ++ (show l)








-- | takes a file and walks to the given position and returns the last n words
findWords 
  :: [String] -- ^ initial list of found words -> should be [] when called from outside
  -> String   -- ^ currentWord that is put together -> should be "" when calles from outside
  -> Int      -- ^ n in n-gramm -> searched for n-1 words
  -> String   -- ^ the file in which it should be searched
  -> Int      -- ^ the line in which the end position is
  -> Int      -- ^ the column of the end position
  -> [String] -- ^ returns a list with the last n-1 full words before the possition
findWords !wordList _ _ _ 1 1 = wordList
findWords _ _ _ [] _ _ = trace "97" []
findWords _ _ _ [f] _ _ = trace [f] []
findWords !wordList cw n (f1 : f2 : file) 1 c 
  | f1 == ' ' && f2 == ' '  = findWords wordList "" n (f2 : file) 1 (c-1)
  | f1 == '\n' && f2 == ' ' = findWords wordList "" n (f2 : file) 1 (c-1)
  | f2 == '\n'              = trace "102" []
  | f2 == ' ' && (f1 == '.' || f1 == '!' || f1 == '?')
                            = findWords [] "" n (f2 : file) 1 (c-1)
  | f2 == ' '               = findWords (addWord (cw ++ [f1]) (n-1) wordList) "" n (f2 : file) 1 (c-1)
  | f1 == ' ' || f1 == '\n' = findWords wordList "" n (f2 : file) 1 (c-1)
  | f2 == '.' || f2 == '!' || f2 == '?' 
                            = findWords [] "" n (f2 : file) 1 (c-1)
  | otherwise               = findWords wordList (cw ++ [f1]) n (f2 : file) 1 (c-1)
findWords !wordList cw n (f1 : f2 : file) l c
  | f1 == ' ' && f2 == ' '   = findWords wordList "" n (f2 : file) l c
  | f1 == '\n' && f2 == '\n' = findWords wordList "" n (f2 : file) (l-1) c
  | f1 == ' ' && f2 == '\n'  = findWords wordList "" n (f2 : file) (l-1) c
  | f1 == '\n' && f2 == ' '  = findWords wordList "" n (f2 : file) l c
  | f2 == ' ' && (f1 == '.' || f1 == '!' || f1 == '?')               
                             = findWords [] "" n (f2 : file) l c
  | f2 == '\n' && (f1 == '.' || f1 == '!' || f1 == '?')
                             = findWords [] "" n (f2 : file) (l-1) c
  | f2 == ' '                = findWords (addWord (cw ++ [f1]) (n-1) wordList) "" n (f2 : file) l c
  | f2 == '\n'               = findWords (addWord (cw ++ [f1]) (n-1) wordList) "" n (f2 : file) (l-1) c
  | f1 == ' ' || f1 == '\n'  = findWords wordList "" n (f2 : file) l c
  | f2 == '.' || f2 == '!' || f2 == '?' 
                             = findWords [] "" n (f2 : file) l c
  | otherwise                = findWords wordList (cw ++ [f1]) n (f2 : file) l c

-- | adds a word to the current State
addWord 
  :: String     -- ^ word to add to the State
  -> Int        -- ^ how many words should be there at most (n-1)
  -> [String]   -- ^ current List of last words
  -> [String]   -- ^ result list of words
addWord s i xs
  = if length xs >= i 
    then s : init xs
    else s : xs


-- | getting the number of n in ngram
getN 
  :: [String]   -- ^ lines of the model file
  -> Int        -- ^ returns the highest number of accepted ngram
getN (m : odel)
  = if m == "\\data\\"
    then getN odel
    else 
      if length m > 2 -- check that it is not a \n
      then max (read (splitOn "=" (splitOn " " m !! 1) !! 0)) (getN odel)
      else 0

getfirst :: [(a,b)] -> [a]
getfirst [] = []
getfirst ((a,b) : xs) = a : getfirst xs

-- 1. Fehlerhafte Eingaben:
--   - zu wenig Argumente
--   - zu viele Argumente: Meldung und mit den ersten richtigen Argumenten fortfahren
--   - Argumente vom falschen Typ
--   - jeweils Ausgabe der richtigen Benutzung
-- 2. Argumente parsen
--   - number to int
--   - model to ?????
--   - file öffnen und in String packen
--   - line to int
--   - column to int
-- 3. starte einen Arbeiter, der alle Eingaben im richtigen Format entgegen nimmt
-- 4. n-1 Wörter vor der besagten Stelle herausfinden
--   - mitschleppen einer Liste mit den n-1 letzten Wörtern
--   - jedes neu gelesene Wort wird hinten angehängt und vorn eins gelöscht, wenn es mehr als n-1 sind
-- 5. an der Stelle angekommen: prüfen ob man mitten in einem Wort ist
--   - wenn ja: Warnung und den Teil der gelesen wurde vergessen
-- 6. n-Gramm-Model aufschlagen und nach den ersten n-1 Wörtern suchen
--   - wenn gefunden: number Wörter mit der größten Warscheinlichkeit auswählen
--   - wenn nicht: n-2 Wörter im n-1-Gramm suchen 