{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Stepik2_2_5 where

import           Control.Applicative

newtype PrsEP a =
  PrsEP
    { runPrsEP :: Int -> String -> (Int, Either String (a, String))
    }

instance Functor PrsEP where
  fmap f p = PrsEP $ \n s -> fmap (\(a, str) -> (f a, str)) <$> runPrsEP p n s

instance Applicative PrsEP where
  pure x = PrsEP (\n str -> (n, Right (x, str)))
  p1 <*> p2 =
    PrsEP $ \n str ->
      case runPrsEP p1 n str of
        (n', Right (f, str2)) -> runPrsEP (f <$> p2) n' str2
        (n', Left x)          -> (n', Left x)

instance Alternative PrsEP where
  empty = PrsEP (\n str -> (n, Left $ "pos " ++ show n ++ ": empty alternative"))
  --  сообщение об ошибке возвращается из той альтернативы,
  --  которой удалось распарсить входную строку глубже.
  p1 <|> p2 =
    PrsEP $ \n str ->
      case runPrsEP p1 n str of
        (n1, Left err1) ->
          case runPrsEP p2 n str of
            (n2, Left err2)
              | n2 > n1 -> (n2, Left err2)
              | otherwise -> (n1, Left err1)
            x -> x
        x -> x

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP predicate = PrsEP f
  where
    f n "" = (succ n, Left $ "pos " ++ show (succ n) ++ ": " ++ "unexpected end of input")
    f n (x:xs)
      | predicate x = (succ n, Right (x, xs))
      | otherwise = (succ n, Left $ "pos " ++ show (succ n) ++ ": " ++ "unexpected " ++ [x])

charEP c = satisfyEP (== c)

prsEP1 = runPrsEP (charEP 'A') 0 "ABC"
  --(1, Right ('A', "BC"))

prsEP2 = runPrsEP (charEP 'A') 41 "BCD"
  --(42, Left "pos 42: unexpected B")

prsEP3 = runPrsEP (charEP 'A') 41 ""
  --(42, Left "pos 42: unexpected end of input")

anyEP = satisfyEP (const True)

tripleP [a, b, c] = (\x y z -> [x, y, z]) <$> charEP a <*> charEP b <*> charEP c

testP = (,) <$> anyEP <* charEP 'B' <*> anyEP

functorPrsEP1 = runPrsEP (pure 42) 0 "ABCDEFG" --(0,Right (42,"ABCDEFG"))

functorPrsEP2 = runPrsEP testP 0 "ABCDE" --(3,Right (('A','C'),"DE"))

functorPrsEP3 = parseEP testP "BCDE" --Left "pos 2: unexpected C"

functorPrsEP4 = parseEP testP "" --Left "pos 1: unexpected end of input"

functorPrsEP5 = parseEP testP "B" --Left "pos 2: unexpected end of input"

altParserEP1 :: (Int, Either String (Int, String))
altParserEP1 = runPrsEP empty 0 "ABCDEFG" -- (0,Left "pos 0: empty alternative")

altParserEP2 :: Either String (String, String)
altParserEP2 = parseEP (tripleP "ABC" <|> tripleP "ADC") "ABE" -- Left "pos 3: unexpected E"

altParserEP3 :: Either String (String, String)
altParserEP3 = parseEP (tripleP "ABC" <|> tripleP "ADC") "ADE" -- Left "pos 3: unexpected E"

altParserEP4 :: Either String (String, String)
altParserEP4 = parseEP (tripleP "ABC" <|> tripleP "ADC") "AEF" -- Left "pos 2: unexpected E"
