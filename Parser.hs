module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST

------------------------------------------------------------
-- Wrapper total (igual que LIS base)
------------------------------------------------------------

totParser :: Parser a -> Parser a
totParser p = do
    whiteSpace lis
    t <- p
    eof
    return t

------------------------------------------------------------
-- Lexer
------------------------------------------------------------

lis :: TokenParser u
lis = makeTokenParser (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , identStart      = letter <|> char '_'
    , identLetter     = alphaNum <|> oneOf "_'"
    , reservedNames   =
        [ "true","false"
        , "skip","if","then","else","end"
        , "repeat","until"
        , "not"
        , "step"
        , "len","toInt","toStr"
        , "upper","lower","reverse","trim"
        , "print","sleep"
        ]
    , reservedOpNames =
        [ ":=","::=",";","|>","++"
        , "+","-","*","/"
        , "=","<",">"
        , "&&","||"
        ]
    })

------------------------------------------------------------
-- Parser de expresiones enteras
------------------------------------------------------------

intexp :: Parser IntExp
intexp = chainl1 term addopp

term :: Parser IntExp
term = chainl1 factor multopp

factor :: Parser IntExp
factor =
        try (parens lis intexp)
    <|> try (do reservedOp lis "-"
                f <- factor
                return (UMinus f))
    <|> try (do reserved lis "len"
                e <- parens lis strexp
                return (Len e))
    <|> try (do reserved lis "toInt"
                e <- parens lis strexp
                return (ToInt e))
    <|> try (do n <- integer lis
                return (Const (fromInteger n)))
    <|> do v <- identifier lis
           return (Var v)

multopp :: Parser (IntExp -> IntExp -> IntExp)
multopp =
        (do reservedOp lis "*"; return Times)
    <|> (do reservedOp lis "/"; return Div)

addopp :: Parser (IntExp -> IntExp -> IntExp)
addopp =
        (do reservedOp lis "+"; return Plus)
    <|> (do reservedOp lis "-"; return Minus)

------------------------------------------------------------
-- Parser de expresiones string (extensión)
------------------------------------------------------------

strexp :: Parser StrExp
strexp = do
    base <- strexpConcat
    filters <- many (try (do reservedOp lis "|>"
                             f <- strfilter
                             return f))
    return (foldl PipeStr base filters)

strexpConcat :: Parser StrExp
strexpConcat = chainl1 strexpValue concatop

concatop :: Parser (StrExp -> StrExp -> StrExp)
concatop = do
    reservedOp lis "++"
    return Concat

strexpValue :: Parser StrExp
strexpValue =
        try (do s <- stringLiteral lis
                return (StrLit s))
    <|> try (do reserved lis "toStr"
                e <- parens lis intexp
                return (ToStr e))
    <|> try (parens lis strexp)
    <|> do v <- identifier lis
           return (SVar v)

strfilter :: Parser StrFilter
strfilter =
        (do reserved lis "upper"; return Upper)
    <|> (do reserved lis "lower"; return Lower)
    <|> (do reserved lis "reverse"; return Reverse)
    <|> (do reserved lis "trim"; return Trim)

------------------------------------------------------------
-- Parser de expresiones booleanas
------------------------------------------------------------

boolexp :: Parser BoolExp
boolexp = chainl1 boolexp2 orop

orop :: Parser (BoolExp -> BoolExp -> BoolExp)
orop = do
    reservedOp lis "||"
    return Or

boolexp2 :: Parser BoolExp
boolexp2 = chainl1 boolexp3 andop

andop :: Parser (BoolExp -> BoolExp -> BoolExp)
andop = do
    reservedOp lis "&&"
    return And

boolexp3 :: Parser BoolExp
boolexp3 =
        try (parens lis boolexp)
    <|> try (do reserved lis "not"
                b <- boolexp3
                return (Not b))
    <|> try intcomp
    <|> boolvalue

intcomp :: Parser BoolExp
intcomp = do
    i <- intexp
    c <- compopp
    j <- intexp
    return (c i j)

compopp :: Parser (IntExp -> IntExp -> BoolExp)
compopp =
        (do reservedOp lis "="; return Eq)
    <|> (do reservedOp lis "<"; return Lt)
    <|> (do reservedOp lis ">"; return Gt)

boolvalue :: Parser BoolExp
boolvalue =
        (do reserved lis "true"; return BTrue)
    <|> (do reserved lis "false"; return BFalse)

------------------------------------------------------------
-- Parser de comandos
------------------------------------------------------------

comm :: Parser Comm
comm = do
    c <- chainl1 comm2 seqop
    actions <- many (try (do reservedOp lis "|>"
                             a <- pipeaction
                             return a))
    return (foldl Pipe c actions)

seqop :: Parser (Comm -> Comm -> Comm)
seqop = do
    reservedOp lis ";"
    return Seq

comm2 :: Parser Comm
comm2 =
        (do reserved lis "skip"
            return Skip)
    <|> try ifcomm
    <|> try repeatcomm
    <|> try letstr
    <|> try letint
    <|> try stepcomm
    <|> parens lis comm

letint :: Parser Comm
letint = do
    v <- identifier lis
    reservedOp lis ":="
    e <- intexp
    return (LetInt v e)

letstr :: Parser Comm
letstr = do
    v <- identifier lis
    reservedOp lis "::="
    e <- strexp
    return (LetStr v e)

stepcomm :: Parser Comm
stepcomm = do
    reserved lis "step"
    e <- parens lis strexp
    return (Step e)

ifcomm :: Parser Comm
ifcomm = do
    reserved lis "if"
    b <- boolexp
    reserved lis "then"
    t <- comm
    reserved lis "else"
    f <- comm
    reserved lis "end"
    return (Cond b t f)

repeatcomm :: Parser Comm
repeatcomm = do
    reserved lis "repeat"
    c <- comm
    reserved lis "until"
    b <- boolexp
    reserved lis "end"
    return (Repeat c b)

pipeaction :: Parser PipeAction
pipeaction =
        (do f <- strfilter
            return (ActFilter f))
    <|> (do reserved lis "print"
            return ActPrint)
    <|> (do reserved lis "sleep"
            e <- parens lis intexp
            return (ActSleep e))

------------------------------------------------------------
-- Funcion de parseo (igual que LIS base)
------------------------------------------------------------

parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
