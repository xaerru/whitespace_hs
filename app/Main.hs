{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Text.Read

type ErrorMsg = String

type Result = Either ErrorMsg String

data Prog = Prog
  { _code :: String,
    _cursor :: Int,
    _counter :: [Int]
  }
  deriving (Show)

data LangState = LangState
  { _prog :: Prog,
    _input :: String,
    _output :: String,
    _stack :: [Int],
    _labels :: Map String Int,
    _heap :: Map Int Int,
    _lerror :: String
  }
  deriving (Show)

makeLenses ''Prog
makeLenses ''LangState

makeLangState :: String -> String -> LangState
makeLangState c i =
  LangState
    { _prog = Prog {_code = filter (\x -> x == ' ' || x == '\t' || x == '\n') c, _cursor = 0, _counter = []},
      _input = i,
      _output = "",
      _stack = [],
      _labels = Map.empty,
      _heap = Map.empty,
      _lerror = ""
    }

cursorAfterNextTerminal :: LangState -> LangState
cursorAfterNextTerminal s = np
  where
    ocur = view (prog . cursor) s
    co = view (prog . code) s
    ncur = '\n' `elemIndex` drop ocur co
    np = case ncur of
      Just n -> set (prog . cursor) (n + ocur + 1) s
      Nothing -> set lerror "Error" s

dropBeforeCursor :: LangState -> String
dropBeforeCursor s = drop (s ^. prog . cursor) $ s ^. (prog . code)

moveCur :: LangState -> (Int -> Int) -> LangState
moveCur s f = (prog . cursor) %~ f $ s

decFromBin :: String -> Maybe Int
decFromBin (c : cs)
  | c == ' ' = decFromBin cs
  | c == '\t' = Just (+ (2 ^ fromMaybe 0 ('\n' `elemIndex` cs))) <*> decFromBin cs
decFromBin ('\n' : _) = Just 0
decFromBin (_ : cs) = decFromBin cs
decFromBin [] = Nothing

parseNumber :: LangState -> (Int, LangState)
parseNumber s
  | x == '\t' = (-1 * fst e, snd e)
  | x == ' ' = e
  where
    co = dropBeforeCursor s
    x = head co
    xs = tail co
    r = decFromBin xs
    ns = cursorAfterNextTerminal s
    e = case r of
      Just n -> (n, ns)
      Nothing -> (0, set lerror "N1umParseError" ns)
parseNumber (LangState (Prog (_ : _) _ _) _ _ _ _ _ _) = (0, set lerror "N2umParseError" (makeLangState "" ""))
parseNumber (LangState (Prog [] _ _) _ _ _ _ _ _) = (0, set lerror "N3umParseError" (makeLangState "" ""))

parseNumberMoveCur :: LangState -> (Int -> Int) -> (Int, LangState)
parseNumberMoveCur s f = parseNumber $ moveCur s f

parseLabel :: String -> Maybe String
parseLabel (c : cs)
  | c == '\t' || c == ' ' = Just (c :) <*> parseLabel cs
parseLabel ('\n' : _) = Just []
parseLabel (_ : cs) = parseLabel cs
parseLabel [] = Nothing

errState :: String -> LangState
errState msg = lerror %~ const msg $ makeLangState "" ""

impSpace :: LangState -> LangState
impSpace s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : _ -> stack %~ (++ [n]) $ s1
        where
          (n, s1) = parseNumberMoveCur s (+ 1)
      '\t' : ' ' : _ -> if check then cs else errState "Index out of range"
        where
          (n, s1) = parseNumberMoveCur s (+ 2)
          sl = length (s1 ^. stack)
          idx = sl - n - 1
          check = idx <= sl - 1 && idx >= 0
          cs = stack %~ (\x -> x ++ [x !! (length x - n - 1)]) $ s1
      '\t' : '\n' : _ ->
        if n >= length (view stack s1) || n < 0
          then if null (s1 ^. stack) then errState "Empty Stack" else cs
          else os
        where
          (n, s1) = parseNumberMoveCur s (+ 2)
          cs = stack %~ (\x -> [last x]) $ s1
          os = stack %~ (\x -> take (length x - n - 1) x ++ [last x]) $ s1
      '\n' : ' ' : _ -> if null (s ^. stack) then errState "Empty Stack" else cs
        where
          s1 = moveCur s (+ 2)
          cs = stack %~ (\x -> x ++ [last x]) $ s1
      '\n' : '\t' : _ -> if length (s ^. stack) >= 2 then cs else errState "Less than 2 values on the stack"
        where
          s1 = moveCur s (+ 2)
          cs = stack %~ (\x -> take (length x - 2) x ++ [last x, last $ init x]) $ s1
      '\n' : '\n' : _ -> if null (s ^. stack) then errState "Empty Stack" else cs
        where
          s1 = moveCur s (+ 2)
          cs = stack %~ init $ s1
      _ : _ -> moveCur s (+ 1)
      [] -> s

stackBinOp :: (Int -> Int -> Int) -> LangState -> LangState
stackBinOp binop s = if length (s ^. stack) >= 2 then cs else errState "Less than 2 elements in stack"
  where
    cs = stack %~ (\x -> take (length x - 2) x ++ [binop (x !! (length x - 2)) (x !! (length x - 1))]) $ s

impTabSpace :: LangState -> LangState
impTabSpace s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : ' ' : _ -> stackBinOp (+) $ moveCur s (+ 2)
      ' ' : '\t' : _ -> stackBinOp (-) $ moveCur s (+ 2)
      ' ' : '\n' : _ -> stackBinOp (*) $ moveCur s (+ 2)
      '\t' : ' ' : _ -> if check then cs else errState "Divide By Zero Error"
        where
          check = null (s ^. stack) || last (s ^. stack) /= 0
          cs = stackBinOp div $ moveCur s (+ 2)
      '\t' : '\t' : _ -> if check then cs else errState "Divide By Zero Error"
        where
          check = null (s ^. stack) || last (s ^. stack) /= 0
          cs = stackBinOp mod $ moveCur s (+ 2)
      _ : _ -> moveCur s (+ 1)
      [] -> s

impTabTab :: LangState -> LangState
impTabTab s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : _ -> if length (s ^. stack) >= 2 then cs else errState "Less than 2 elements in stack"
        where
          s1 = moveCur s (+ 1)
          addToMap x = heap %~ Map.insert (x !! (length x - 2)) (x !! (length x - 1)) $ s1
          cs = stack %~ (\x -> take (length x - 2) x) $ addToMap (s1 ^. stack)
      '\t' : _ -> if null (s ^. stack) then errState "Empty Stack" else cs
        where
          hval = Map.lookup (last (s ^. stack)) (s ^. heap)
          pushStack s1 val = stack %~ (\x -> init x ++ [val]) $ s1
          cs = case hval of
            Just val -> pushStack (moveCur s (+ 1)) val
            Nothing -> errState "Heap Address not found"
      _ : _ -> moveCur s (+ 1)
      [] -> s

parseNumberFromInput :: LangState -> (Maybe Int, LangState)
parseNumberFromInput s = acs
  where
    c1 = '\n' `elem` (s ^. input)
    a = readMaybe (takeWhile (/= '\n') (s ^. input)) :: Maybe Int
    acs = case a of
      Just a' -> (Just a', input %~ tail . dropWhile (/= '\n') $ s)
      Nothing ->
        if c1
          then (Nothing, errState "Couldn't Parse number from input")
          else (Nothing, errState "Invalid Number")

impTabLineFeed :: LangState -> LangState
impTabLineFeed s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : ' ' : _ -> if null (s ^. stack) then errState "Empty Stack" else cs
        where
          cs = stack %~ init $ output %~ (++ [chr $ last (s ^. stack)]) $ moveCur s (+ 2)
      ' ' : '\t' : _ -> if null (s ^. stack) then errState "Empty Stack" else cs
        where
          cs = stack %~ init $ output %~ (++ (show $ last (s ^. stack))) $ moveCur s (+ 2)
      '\t' : ' ' : _ -> cs
        where
          c1 = not $ null $ s ^. input
          a = if c1 then Just (ord $ head (s ^. input)) else Nothing
          s1 = input %~ tail $ moveCur s (+ 2)
          c2 = not $ null $ s ^. stack
          b = if c2 then Just (last (s ^. stack)) else Nothing
          cs = case a of
            Just a' -> case b of
              Just b' -> heap %~ Map.insert b' a' $ stack %~ init $ s1
              Nothing -> errState "Empty Stack"
            Nothing -> errState "End of input"
      '\t' : '\t' : _ -> cs
        where
          (a, s1) = parseNumberFromInput $ moveCur s (+ 2)
          c1 = not $ null (s ^. stack)
          b = if c1 then Just (last (s ^. stack)) else Nothing
          cs = case a of
            Just a' -> case b of
              Just b' -> heap %~ Map.insert b' a' $ stack %~ init $ s1
              Nothing -> errState "Empty Stack"
            Nothing -> s1
      _ : _ -> moveCur s (+ 1)
      [] -> s

impLineFeed :: LangState -> LangState
impLineFeed s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : ' ' : _ -> cursorAfterNextTerminal $ moveCur s (+ 2)
      ' ' : '\t' : _ -> nss
        where
          s1 = moveCur s (+ 2)
          l = parseLabel $ dropBeforeCursor s1
          s2 = cursorAfterNextTerminal s1
          s3 = prog . counter %~ (++[s2 ^. prog . cursor]) $ s2
          nss = case l of
            Just n -> prog . cursor %~ const (Map.findWithDefault 0 n (s ^. labels)) $ s3
            Nothing -> lerror %~ const "LabelParseError" $ s3
      ' ' : '\n' : _ -> nss
        where
          s1 = moveCur s (+ 2)
          l = parseLabel $ dropBeforeCursor s1
          s2 = cursorAfterNextTerminal s1
          nss = case l of
            Just n -> case Map.lookup n (s ^. labels) of
              Just cur -> prog . cursor %~ const cur $ s2
              Nothing -> errState "Couldn't find label"
            Nothing -> errState "Couldn't Parse label"
      '\t' : ' ' : _ -> nss
        where
          s1 = moveCur s (+ 2)
          l = parseLabel $ dropBeforeCursor s1
          s2 = cursorAfterNextTerminal s1
          c1 = not $ null (s2 ^. stack)
          ms3 = if c1 then Just (stack %~ init $ s2) else Nothing
          nss = case l of
            Just n -> case Map.lookup n (s ^. labels) of
              Just cur -> case ms3 of
                Just s3 -> prog . cursor %~ (\ocur -> if last (s2 ^. stack) == 0 then cur else ocur) $ s3
                Nothing -> errState "Empty Stack"
              Nothing -> errState "Couldn't find label"
            Nothing -> errState "Couldn't Parse label"
      '\t' : '\t' : _ -> nss
        where
          s1 = moveCur s (+ 2)
          l = parseLabel $ dropBeforeCursor s1
          s2 = cursorAfterNextTerminal s1
          c1 = not $ null (s2 ^. stack)
          ms3 = if c1 then Just (stack %~ init $ s2) else Nothing
          nss = case l of
            Just n -> case Map.lookup n (s ^. labels) of
              Just cur -> case ms3 of
                Just s3 -> prog . cursor %~ (\ocur -> if last (s2 ^. stack) < 0 then cur else ocur) $ s3
                Nothing -> errState "Empty Stack"
              Nothing -> errState "Couldn't find label"
            Nothing -> errState "Couldn't Parse label"
      '\t' : '\n' : _ -> prog . counter %~ init $ prog . cursor %~ (\_ -> last (s ^. prog . counter)) $ s
      '\n' : '\n' : _ -> moveCur s (const (length (s ^. prog . code)))
      _ : _ -> moveCur s (+ 1)
      [] -> s

-- TODO: Figure out a way of processing all the labels before computation
processLabels :: LangState -> LangState
processLabels s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : _ -> case dropBeforeCursor (moveCur s (+ 1)) of
        ' ' : _ -> processLabels $ snd $ parseNumberMoveCur s (+ 2)
        '\t' : ' ' : _ -> processLabels $ snd $ parseNumberMoveCur s (+ 3)
        '\t' : '\n' : _ -> processLabels $ snd $ parseNumberMoveCur s (+ 3)
        '\n' : ' ' : _ -> processLabels $ moveCur s (+ 3)
        '\n' : '\t' : _ -> processLabels $ moveCur s (+ 3)
        '\n' : '\n' : _ -> processLabels $ moveCur s (+ 3)
        _ : _ -> processLabels $ moveCur s (+ 2)
        [] -> prog . cursor %~ const 0 $ s
      '\t' : ' ' : _ -> case dropBeforeCursor (moveCur s (+ 2)) of
        ' ' : ' ' : _ -> processLabels $ moveCur s (+ 4)
        ' ' : '\t' : _ -> processLabels $ moveCur s (+ 4)
        ' ' : '\n' : _ -> processLabels $ moveCur s (+ 4)
        '\t' : ' ' : _ -> processLabels $ moveCur s (+ 4)
        '\t' : '\t' : _ -> processLabels $ moveCur s (+ 4)
        _ : _ -> processLabels $ moveCur s (+ 3)
        [] -> prog . cursor %~ const 0 $ s
      '\t' : '\t' : _ -> case dropBeforeCursor (moveCur s (+ 2)) of
        ' ' : _ -> processLabels $ moveCur s (+ 3)
        '\t' : _ -> processLabels $ moveCur s (+ 3)
        _ : _ -> processLabels $ moveCur s (+ 3)
        [] -> prog . cursor %~ const 0 $ s
      '\t' : '\n' : _ -> case dropBeforeCursor (moveCur s (+ 2)) of
        ' ' : ' ' : _ -> processLabels $ moveCur s (+ 4)
        ' ' : '\t' : _ -> processLabels $ moveCur s (+ 4)
        '\t' : ' ' : _ -> processLabels $ moveCur s (+ 4)
        '\t' : '\t' : _ -> processLabels $ moveCur s (+ 4)
        _ : _ -> processLabels $ moveCur s (+ 3)
        [] -> prog . cursor %~ const 0 $ s
      '\n' : _ -> case dropBeforeCursor (moveCur s (+ 1)) of
        ' ' : ' ' : _ -> nss
          where
            s1 = moveCur s (+ 3)
            l = parseLabel $ dropBeforeCursor s1
            s2 = cursorAfterNextTerminal s1
            nss = case l of
              Just n -> processLabels $ labels %~ Map.insert n (s2 ^. prog . cursor) $ s2
              Nothing -> errState "Couldn't parse label"
        ' ' : '\t' : _ -> processLabels $ cursorAfterNextTerminal $ moveCur s (+ 3)
        ' ' : '\n' : _ -> processLabels $ cursorAfterNextTerminal $ moveCur s (+ 3)
        '\t' : ' ' : _ -> processLabels $ cursorAfterNextTerminal $ moveCur s (+ 3)
        '\t' : '\t' : _ -> processLabels $ cursorAfterNextTerminal $ moveCur s (+ 3)
        '\t' : '\n' : _ -> processLabels $ moveCur s (+ 3)
        '\n' : '\n' : _ -> processLabels $ moveCur s (+ 3)
        _ : _ -> processLabels $ moveCur s (+ 2)
        [] -> prog . cursor %~ const 0 $ s
      _ : _ -> processLabels $ moveCur s (+ 1)
      [] -> prog . cursor %~ const 0 $ s

process :: LangState -> LangState
process s = ns
  where
    ns = case dropBeforeCursor s of
      ' ' : _ -> process $ impSpace (moveCur s (+ 1))
      '\n' : _ -> process $ impLineFeed (moveCur s (+ 1))
      '\t' : ' ' : _ -> process $ impTabSpace (moveCur s (+ 2))
      '\t' : '\t' : _ -> process $ impTabTab (moveCur s (+ 2))
      '\t' : '\n' : _ -> process $ impTabLineFeed (moveCur s (+ 2))
      _ : _ -> process $ moveCur s (+ 1)
      [] -> s

whitespace :: String -> String -> Result
whitespace "" "" = Left "error"
whitespace c i = r
  where
    s = process $ processLabels $ makeLangState c i
    r = case s ^. lerror of
      "" -> Right (s ^. output)
      _ -> Left (s ^. lerror)

main :: IO ()
main = do
  let i = makeLangState "   \t\n   \t \n\t\t    \t\n\t\t\t" ""
  print $ process i
