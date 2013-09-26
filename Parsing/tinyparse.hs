{--
Lucas Nunno
CS 491: Advanced Functional Programming
Tiny Basic Parser
Grammar at: http://en.wikipedia.org/wiki/Tiny_BASIC
Used Real World Haskell's Parsec Tutorial heavily: http://book.realworldhaskell.org/read/using-parsec.html
Parsec Documentation: http://legacy.cs.uu.nl/daan/download/parsec/parsec.pdf
--}
import Text.Parsec.Combinator 
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

spaceSep :: Parser String        
spaceSep = many (char ' ')
        
-- var ::= A | B | C .... | Y | Z
var :: Parser String
var = do
    varName <- upper
    return [varName]

-- var-list ::= var (, var)*
varList :: Parser String
varList = do
    vars <- sepBy var (char ',') 
    let v2 = reverse $ drop 1 $ reverse $ concat [x++"," |  x <- vars]
    return v2    
    
cr :: Parser Char    
cr = char '\n'    

-- relop ::= < (>|=|e) | > (<|=|e) | =
-- relop :: Parser String
relop :: Parser String
relop = 
    do
        char '<' 
        c <- oneOf ">="
        return ("<"++[c])
    <|> 
    do 
        char '>'
        c <- oneOf "<="
        return (">"++[c])
    <|> string "="

-- Standard string definition.
str :: Parser String
str = do
    char '"'
    strBody <- many (noneOf "\"")
    char '"'
    return strBody
    
-- number ::= digit digit*    
number :: Parser String
number = 
    do
        ds <- many1 digit
        return ds
    <?> "number"    

-- 0 or more statements ended by a BREAK.    
basicFile = endBy (many line) (string "BREAK")
          
parseBasic input =  parse basicFile "(unknown)" input

{--
Execute the tinybasic parser.
Similar to CSV parser main in RWH Tutorial.
--}
main =
    do c <- getContents
       case parse basicFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r
            
{--
Run a specific parser p on string input.
From Parsec Docs.
--}            
run :: Show a => Parser a -> String -> IO ()
run p input
    = case (parse p "" input) of
        Left err -> do{ putStr "parse error at "
        ; print err
        }
        Right x -> print x 
    
line =
    do
        spaceSep
        n <- number
        spaceSep
        s <- statement
        cr
        return (n++" "++s++"\n")
    <|>
    do
        s <- statement
        cr
        return (s++"\n")

{--
   statement ::= PRINT expr-list
                 IF expression relop expression THEN statement
                 GOTO expression
                 INPUT var-list
                 LET var = expression
                 GOSUB expression
                 RETURN
                 CLEAR
                 LIST
                 RUN
                 END
NOTE: Try(s) are needed so that the string calls do not consume input.
--}        
statement = 
    do        
        try (string "PRINT")
        spaceSep
        exprs <- exprList 
        return ("PRINT " ++ exprs)
    <|>
    do
        try (string "IF")
        spaceSep
        e <- expr
        spaceSep
        r <- relop
        spaceSep
        e2 <- expr
        spaceSep
        string "THEN"
        spaceSep
        s <- statement
        return ("IF " ++ (show e) ++ r ++ (show e2) ++ " THEN " ++ s)
    <|>
    do
        try(string "GOTO")
        spaceSep
        e <- expr
        return ("GOTO " ++ (show e))
    <|>
    do
        try(string "INPUT")
        spaceSep
        vs <- varList 
        return ("INPUT " ++ vs)
    <|>
    do
        try(string "LET")
        spaceSep
        v <- var
        spaceSep
        char '='
        spaceSep
        e <- expr
        return ("LET " ++ v ++ " = " ++ (show e))
    <|>
    do
        try(string "GOSUB")
        spaceSep
        e <- expr
        return ("GOSUB " ++ (show e))
    <|>
    do 
        try(string "RETURN")
        return "RETURN"
    <|>
    do
        try(string "CLEAR")
        return "CLEAR"
    <|>
    do
        try(string "LIST")
        return "LIST"
    <|>
    do
        try(string "RUN")
        return "RUN"
    <|>
    do
        try(string "END")
        return "END"
        
expr :: Parser String        
expr    =  
    do
        es <- term `chainl1` addop
        return es

-- expr-list ::= (string|expression) (, (string|expression) )*
-- exprList :: Parser [String]
exprList = 
    do
        exprs <- sepBy (str <|> expr) (char ',') 
        let e2 = reverse $ drop 1 $ reverse $ concat [x++"," |  x <- exprs]
        return e2    
        
term :: Parser String        
term    =  
    do 
        f <- factor 
        fs <- many tHelp
        return $ f ++ (concat fs)
        
tHelp = 
    do
        op <- oneOf "*/"
        f <- factor
        return ([op]++f)
      
factor :: Parser String      
factor  =  var <|> number <|> 
    do 
        char '('
        e <- expr      
        char ')'
        return ( ('(':e)++")")
    
-- addop :: GenParser Char st (String -> String -> String)
addop = 
    do
        char '+'
        return (\x y -> (x++"+"++y) )
    <|>        
    do
        char '-'
        return (\x y -> (x++"-"++y) )

-- Wouldn't compile, although addop did and they're identical..Don't know why. GHC magic.        
-- mulop :: GenParser Char st (String -> String -> String)
-- mulop =      
    -- do
        -- char '*'
        -- return (\x y -> (x ++"*"++y) )
    -- <|>
    -- do
        -- char '/'
        -- return (\x y -> (x ++"/"++y) )  

        