{- Bloop, Copyright (C) 2005, Jaap Weel -- This program is free
software; you can redistribute it and/or modify it under the terms of
the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any
later version.  This program is distributed in the hope that it will
be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.  You should have received a
copy of the GNU General Public License along with this program; if
not, write to the Free Software Foundation, Inc., 59 Temple Place,
Suite 330, Boston, MA 02111-1307 USA -}

module ParseBloop ( parseBloop ) 
where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.Regex
import AstBloop

-- Drive the parser --------------------------------------------------
parseBloop filename program_text = parse program filename program_text

-- Syntax ------------------------------------------------------------
program = do whiteSpace
             ps <- sepBy (procedure <|> top_expr) semi
             symbol "."
             return (Program ps)                           <?> "FLooP program"

procedure = do reserved "DEFINE" 
               reserved "PROCEDURE" 
               symbol "\""
               name <- atom
               symbol "\""
               args <- brackets (commaSep atom)            <?> "argument list"
               symbol ":"                       
               body <- block
               return (Procedure (s name) (Var name) (map Var args) body) 
            <?> "procedure"
  where s (Symbol n) = case matchRegex (mkRegex ".*\\?$") n of
                Just _  -> Boolean False
                Nothing -> Number 0

top_expr = do e <- expr
              return (TopExp e)                            <?> "expression"

-- Blocks  -----------------------------------------------------------
block = do reserved "BLOCK"
           b1 <- fixnum
           symbol ":" 
           reserved "BEGIN"
           statements <- endBy statement semi
           reserved "BLOCK"
           b2 <- fixnum
           symbol ":" 
           reserved "END" 
           if (b1 /= b2)
              then fail $ "Block mismatch "++show b1++" "++show b2
              else return (Block b1 statements)
        <?> "block"

-- Statements  -------------------------------------------------------
statement = try block <|> quit <|> abort <|> loop <|> muloop <|>
            ifthen <|> assignment

quit = do reserved "QUIT" 
          reserved "BLOCK"
          n <- fixnum
          return (Quit n)                      <?> "quit statement"

abort = do reserved "ABORT" 
           reserved "LOOP"
           n <- fixnum
           return (Abort n)                    <?> "abort statement"

muloop = do reserved "MU-LOOP" 
            symbol ":"
            b@(Block n _) <- block
            return (Muloop n b)                <?> "mu loop statement"

-- Note that "AT MOST" is not required on p.410 of GEB...

loop = do reserved "LOOP" 
          option () (do reserved "AT"; reserved "MOST")
          e <- expr
          whiteSpace
          reserved "TIMES"
          symbol ":"
          b@(Block n _) <- block
          return (Loop e n b)                  <?> "bounded loop statement"

ifthen = do reserved "IF"
            e <- expr
            symbol "," 
            reserved "THEN" 
            symbol ":"
            s <- statement
            return (Ifthen e s)                <?> "if-then statement"

assignment = do left <- lvalue
                symbol "<="
                right <- expr
                return (Assign left right)     <?> "assignment"

-- Expressions  ------------------------------------------------------
expr        = infix_expr <|> simple_expr
simple_expr = braces expr <|> cell <|> output <|> number <|> boolean
              <|> try funcall <|> var 
infix_expr  = buildExpressionParser operators simple_expr

operators = [[ op "*" AssocLeft ], [ op "+"  AssocLeft ],
             [ op "=" AssocNone, op "<" AssocNone, op ">" AssocNone ],
             [ op "AND" AssocRight ], [ op "OR" AssocRight ]]
  where op name = Infix (reservedOp name >> return (\x y -> Binop name x y)) 

funcall = do a <- atom 
             argl <- brackets (commaSep expr)
             return (Funcall a argl)         <?> "function call"

output = do reserved "OUTPUT"
            return (Output)                  <?> "output identifier"

boolean = (do reserved "YES"; return (Boolean True)) <|>
          (do reserved "NO";  return (Boolean False))
                                             <?> "boolean constant"

number = do n <- fixnum 
            return (Number n)                <?> "decimal number"

var = do a <- atom
         return (Var a)                      <?> "variable name"

cell = do reserved "CELL"
          n <- parens fixnum
          return (Cell $ CellNumber n)       <?> "cell identifier"

lvalue = output <|> cell <|> var             <?> "valid lvalue"

-- Lexical structure  ------------------------------------------------
ident_chars = oneOf "?-" <|> upper

lexer  = P.makeTokenParser $
         emptyDef   { P.commentStart   = "/*",
                      P.commentEnd     = "*/",
                      P.commentLine    = "==",
                      P.identStart     = ident_chars,
                      P.identLetter    = ident_chars,
                      P.reservedNames  =
                        ["DEFINE", "PROCEDURE", "CELL", "OUTPUT",
                         "BLOCK", "BEGIN", "END", "QUIT", 
                         "IF", "THEN", "MU-LOOP", "YES", "NO",
                         "LOOP", "AT", "MOST", "TIMES", "ABORT", "LOOP"],
                      P.reservedOpNames= 
                       ["<=","=","<",">","\"",":",";",",",".","*","+",
                        "AND","OR"] }

whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
symbol     = P.symbol lexer
reservedOp = P.reservedOp lexer
decimal    = P.decimal lexer
reserved   = P.reserved lexer
semi       = P.semi lexer
commaSep   = P.commaSep lexer
semiSep    = P.semiSep lexer
braces     = P.braces lexer
parens     = P.parens lexer
brackets   = P.brackets lexer

-- Some tokens wrapped in a little sugar -----------------------------
fixnum = do n <- decimal 
            whiteSpace
            return n           <?> "decimal number"

atom = do i <- identifier
          return (Symbol i)    <?> "symbol"

