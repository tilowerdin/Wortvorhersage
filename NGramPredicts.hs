{-# LANGUAGE ScopedTypeVariables #-}

module Main
(
    findWords,
    getNumberOfLines,
    getSuggestions,
    getWordsFromLine,
    main,
    splitModel,
    toEntry,
    toEntryWithBackoff,
    usage
)
where

import System.Environment
import Data.Char
import Debug.Trace
import Control.Exception as E
import System.Exit
import System.IO()

data Entry = Entry {
    pre :: [String],
    word :: String,
    propability :: Float,
    back :: Float
} deriving Show

-- | main method
main :: IO ()
main = do 
  args <- getArgs
  case length args of
    5 -> catch (testArgs args) $ 
      \(_ :: E.SomeException) -> do{
            putStrLn "Unfortunately one or more of the arguments could not be read or didn't lead to an existing file\n\n";
            usage;
            exitSuccess;}
    _ -> do putStrLn "You did not pass 5 arguments\n\n"
            usage
            exitSuccess

  --model <- readFile (args !! 1) `catch` usageExc
  --file <- readFile (args !! 2) `catch` usageExc
  --start (maybeRead $ head args) model file (maybeRead $ args !! 3) (maybeRead $ args !! 4)
  
testArgs :: [String] -> IO ()
testArgs args = do
    model <- readFile $ args !! 1
    file <- readFile $ args !! 2
    start (read $ head args) model file (read $ args !! 3) (read $ args !! 4)

start :: Int -> String -> String ->Int ->Int -> IO()
start k model file line column
  = k `seq` model `seq` file `seq` line `seq` column `seq` let ws = findWords (length $ getNumberOfLines $ lines model) file line column in print $ map fst $ 
  getSuggestions k model $ trace (show ws) ws

usage :: IO ()
usage = putStrLn
      "usage: NGramPredicts <number> <model> <file> <line> <column>\n\
      \where\n\
      \  number: defines the number of suggestions should be made\n\
      \  model:  defines the path to the n-gram-model in ARPA-Format\n\
      \  file:   defines the file which contains the text that should be made a suggestions for\n\
      \  line:   defines the line number in the file in which the suggestion should be made\n\
      \  column: defines the char in the line of the file in which the suggestion should be made"

-- | Diese Methode liefert den Präfix der gegebenen Stelle
findWords 
  :: Int        -- ^ Länge des zu erstellenden Präfixes
  -> String     -- ^ String der Datei, in der der Präfix gefunden werden soll 
  -> Int        -- ^ Zeile, der betroffenen Stelle
  -> Int        -- ^ Stelle, in der Zeile, an der die Vorhersage erfolgen soll
  -> [String]   -- ^ Liste von k Wörter vor der Stelle 
findWords k str line column
  = let strLines = lines str 
    in 
      let wordList = words ( unlines ( take (line - 2) strLines)) ++ getWordsFromLine (strLines !! (line - 1)) (column-1)
      in drop (length wordList - k) wordList

-- | liefert alle Wörter einer Zeile, bis zur Stelle column
getWordsFromLine 
  :: String     -- ^ die Zeile aus der gelesen werden soll
  -> Int        -- ^ die Stelle bis zu der gelesen werden soll
  -> [String]   -- ^ Liste aller Wörter, die bis zu dieser Stelle abgeschlossen sind
getWordsFromLine line column
  = if isLetter (line !! column) 
    then init $ words $ take column line
    else words $ take column line

-- | liefert die Wortvorschläge inklusive deren berechneter Wahrscheinlichkeiten
getSuggestions 
  :: Int                -- ^ Anzahl an Vorschlägen
  -> String             -- ^ String der Datei des Models
  -> [String]           -- ^ der Präfix der zu berechnenden Stelle
  -> [(String, Float)]  -- ^ Wortvorschläge inklusive deren Wahrscheinlichkeiten
getSuggestions k model
  = search (reverse $ splitModel model)
  where
    -- | sucht innerhalb des ngrams nach Ergebnissen 
    search 
      :: [[Entry]]          -- ^ index 0 sollte die ngrame des ngram models darstellen und keine backoffs beinhalten
      -> [String]           -- ^ der Präfix der zu berechnenden Stelle
      -> [(String, Float)]  -- ^ liefert Wortvorschläge inklusive deren Wahrscheinlichkeiten
    search [] _            = undefined
    search (m : ms) prefix 
        = let best = calculateBest [] 0 m prefix in 
          if length best < k 
          then searchWithBackoff ms prefix 0 best
          else best

    -- | sucht innerhalt der (n-1)gram .. 1gram nach Ergebnissen
    searchWithBackoff 
      :: [[Entry]]          -- ^ index 0 wird untersucht, der Rest der Liste rekursiv abgearbeitet
      -> [String]           -- ^ der zu untersuchende Präfix
      -> Float              -- ^ der aktuell berechnete Backoff
      -> [(String, Float)]  -- ^ die aktuell k besten Vorschläge
      -> [(String, Float)]  -- ^ liefert die k besten Wortvorschläge
    searchWithBackoff [] _ _ dic = dic
    searchWithBackoff (m : ms) prefix backoff dic
      = let b = calculateBackoff backoff m prefix in go b
      where
        go b 
          = let best = calculateBest dic b m prefix in
            if length best < k
            then searchWithBackoff ms prefix b best
            else best

    -- | erstellt eine Liste mit den k besten Einträgen
    calculateBest 
      :: [(String, Float)]  -- ^ bisherige gefundene Vorschläge
      -> Float              -- ^ der aktuell berechnete Backoff
      -> [Entry]            -- ^ eine sektion von igram im ngram modell
      -> [String]           -- ^ der zu untersuchende Präfix
      -> [(String, Float)]  -- ^ liefert eine Liste mit den k besten Wortvorschlägen inklusive Wahrscheinlichkeiten
    calculateBest dic _ [] _ = dic
    calculateBest dic backoff (entry : xs) prefix 
      = if drop (length prefix - length (pre entry)) prefix == pre entry && not (inside (word entry) dic)
        then 
          calculateBest  
            (insert (word entry, backoff + propability entry) dic) 
            backoff 
            xs 
            prefix
        else 
          calculateBest
            dic
            backoff
            xs
            prefix

    -- | berechnet den neuen backoff anhand einer igram liste
    calculateBackoff 
      :: Float      -- ^ der aktuelle backoff
      -> [Entry]    -- ^ eine igram liste aus dem ngram modell
      -> [String]   -- ^ der zu untersuchende Präfix
      -> Float      -- ^ liefert den neuen Backoff
    calculateBackoff backoff [] _ = backoff
    calculateBackoff backoff (entry : xs) prefix
      = let d = drop (length prefix - length (pre entry) - 1) prefix in --trace ("prefix: " ++ show d ++ " entry: " ++ show ( pre entry ++ [word entry])) $
        if d == pre entry ++ [word entry]
        then backoff + back entry
        else calculateBackoff backoff xs prefix

    -- | fügt ein neues Wort mit seiner Wahrscheinlichkeit in die Liste der Vorschläge ein
    insert 
      :: (String, Float)    -- ^ der neue Eintrag
      -> [(String, Float)]  -- ^ die bisherigen Vorschläge
      -> [(String, Float)]  -- ^ die aktualisierte Liste der Vorschläge
    insert (wo, pr) [] = [(wo, pr)]
    insert (wo, pr) ((w, p) : xs)
      | p > pr =
          if length xs + 1 < k
          then (wo, pr) : (w, p) : xs
          else (w, p) : xs
      | length xs + 1 < k = 
          (w, p) : insert (wo, pr) xs
      | otherwise = insert (wo, pr) xs

    -- | testet, ob ein Wort bereits in der  Vorschlagliste einhalten ist
    inside 
      :: String             -- ^ da Wort
      -> [(String, Float)]  -- ^ die Liste der bisherigen Vorschläge
      -> Bool               -- ^ true wenn es bereits vorhanden ist
    inside _ [] = False
    inside w ((wo, _) : xs)
      | w == wo = True
      | otherwise = inside w xs

-- | teilt das Model in seine Bestandteile auf
splitModel 
  :: String     -- ^ der String des ngram-Model 
  -> [[Entry]]  -- ^ Zeilen werden in Entries umgewandelt, Entries in Listen zusammengefasst, je nach dem zu welchen igram sie gehören
splitModel s = go [] (lines s)
  where 
    go :: [Int] -> [String] -> [[Entry]]
    go [] xs       = let numLines = getNumberOfLines xs in go numLines (drop (length numLines + 3) xs)
    go [i] xs      = [map toEntry (take i xs)]
    go (i : is) xs = let (front, end) = splitAt i xs in map toEntryWithBackoff front : go is (drop 2 end)

-- | liefert die Anzahl an Zeilen zu jedem igram model im ngram model
getNumberOfLines 
  :: [String]   -- ^ das ngram model als Liste von Zeilen 
  -> [Int]      -- ^ eine Liste von Zeilen, die zu dem jeweiligen igram gehören (index 0: 1gram, index n-1: ngram)
getNumberOfLines [] = []
getNumberOfLines (x:xs) 
  | isNumberOfLines x = parseNumberOfLines x ++ getNumberOfLines xs
  | otherwise = []
  where
    -- | entscheidet ob diese Zeile noch dazu gehört, oder nicht
    isNumberOfLines 
      :: String -- ^ die Zeile
      -> Bool   -- ^ true wenn die Zeile noch dazu gehört
    isNumberOfLines s
      | s == "\\data\\" = True
      | take 5 s == "ngram" = True
      | otherwise = False

    -- | gibt die Zahl zurück, die in der Zeile verborgen ist
    parseNumberOfLines 
      :: String -- ^ die Zeile
      -> [Int]  -- ^ die Zahl als Atomare Liste
    parseNumberOfLines s
      | s == "\\data\\" = []
      | otherwise = [read (getNumber s)]
      where
        getNumber [] = "ERROR"
        getNumber (y : ys)
          | y == '=' = ys
          | otherwise = getNumber ys

-- | macht aus einer Zeile einen (n-1)gram .. 1gram einen Entry
toEntryWithBackoff 
  :: String -- ^ eine Zeile mit Backoff
  -> Entry  -- ^ die Zeile umgewandelt
toEntryWithBackoff s = let lineSplitted = words s in 
  Entry {
    pre=init (init (tail lineSplitted ) ), 
    word=lineSplitted !! (length lineSplitted - 2), 
    propability=read (head lineSplitted),
    back=read (lineSplitted !! (length lineSplitted - 1))
  }

-- | macht aus einer Zeile des ngrams einen Entry
toEntry 
  :: String -- ^ eine Zeile ohne Backoff
  -> Entry  -- ^ die Zeile umgewandelt in einen Entry
toEntry s = let lineSplitted = words s in
  Entry {
    pre=init (tail lineSplitted),
    word=lineSplitted !! (length lineSplitted - 1),
    propability=read (head lineSplitted),
    back=0
  }