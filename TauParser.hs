module TauParser(
  ARGS(..), MATCH(..), CLAUSE(..), AST(..), parse
) where

import Data.List

data Token = ID_T String
    | LEFT_BRACKET
    | RIGHT_BRACKET
    | List [Token] deriving (Show, Eq)

data ARGS = ARGS [String] deriving (Show, Eq)
data MATCH = MATCH String deriving (Show, Eq)
data CLAUSE = CLAUSE MATCH ARGS AST deriving (Show, Eq)

data AST = ID String
    | FDEF ARGS MATCH [CLAUSE]
    | FCOMP ARGS AST
    | APPLY AST [AST] deriving (Show, Eq)

parse = makeAst . makeLists . splitToTokens

splitToTokens src = reverse (splitToTokens' src [] [])
    where splitToTokens' [] [] tokens = tokens
          splitToTokens' [] id tokens = (ID_T (reverse id)):tokens

          splitToTokens' (x:xs) [] tokens
              | x `elem` "\t\n\r "   = splitToTokens' xs     []     tokens
              | x == '('             = splitToTokens' xs     []     (LEFT_BRACKET:tokens)
              | x == ')'             = splitToTokens' xs     []     (RIGHT_BRACKET:tokens)
              | otherwise            = splitToTokens' xs     (x:[]) tokens

          splitToTokens' (x:xs) id tokens
              | x `elem` "\t\n\r ()" = splitToTokens' (x:xs) []     ((ID_T (reverse id)):tokens)
              | otherwise            = splitToTokens'  xs    (x:id) tokens


makeLists tokens = makeLists' tokens (List []) []
    where makeLists' [] (List list) [] = List (reverse list)
          makeLists' [] (List list) [(List [])] = List list
          makeLists' (x:xs) localFrame@(List tokens) stack
              | x == LEFT_BRACKET  = makeLists' xs (List []) (localFrame:stack)
              | x == RIGHT_BRACKET = levelUp xs (closeScope localFrame stack)
              | otherwise          = makeLists' xs (List (x:tokens)) stack
              where levelUp src (localFrame:stack) = makeLists' src localFrame stack
                    closeScope localFrame [] = [(reverseFrame localFrame)]
                    closeScope localFrame ((List frame):ls) = (List ((reverseFrame localFrame):frame)):ls

reverseFrame (List localFrame) = List (reverse localFrame)

getIdName (ID_T name) = name
makeArgs tokens = ARGS (map getIdName tokens)

makeAst (ID_T name) = ID name
makeAst (List tokens) = makeAst' tokens

makeAst' (token:[]) = makeAst token
makeAst' list
    | isFunction list = parseFunction list
    | otherwise       = parseApply list

-- elemIndex можно упростить
isFunction ids = isFunction' ids 0
isFunction' [] _ = False
isFunction' ((ID_T "=>"):xs) level = level > 0
isFunction' ((ID_T id):xs) level = isFunction' xs (level + 1)
isFunction' _ _ = False

parseFunction list = parseFunction' list []
parseFunction' ((ID_T "=>"):xs) args
    | isDefinition xs  = parseDefinition (reverse args) (tail xs)
    | otherwise        = parseComposition (reverse args) xs
parseFunction' (x:xs) args = parseFunction' xs (x:args)

isDefinition (x:xs) = x == (ID_T "match")

parseDefinition args ((ID_T name):xs) = FDEF (makeArgs args) (MATCH name) (getClauses xs)

getClauses tokens = getClauses' tokens []
getClauses' [] clauses = reverse clauses
getClauses' ((ID_T "|"):xs) clauses = parseClause xs clauses

parseClause tokens clauses = parseClauseArgs tokens clauses []

parseClauseArgs (x:xs) clauses ids
    | x == (ID_T "=>") = parseClauseBody xs clauses (reverse ids)
    | otherwise        = parseClauseArgs xs clauses (x:ids)

makeClause args body = CLAUSE (MATCH (getIdName (head args))) (makeArgs (tail args)) (makeAst' (reverse body))

parseClauseBody tokens clauses ids = parseClauseBody' tokens clauses ids []
parseClauseBody' [] clauses ids body = reverse ((makeClause ids body):clauses)
parseClauseBody' ((ID_T "|"):xs) clauses ids body = parseClause xs ((makeClause ids body):clauses)
parseClauseBody' (x:xs) clauses ids body = parseClauseBody' xs clauses ids (x:body)

parseComposition args tokens = (FCOMP (makeArgs args) (makeAst (List tokens)))

parseApply (x:xs) = (APPLY (makeAst x) (map makeAst xs))
