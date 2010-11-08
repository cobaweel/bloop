\documentclass{scrartcl}
%include lhs2TeX.fmt
%include lhs2TeX.sty
\title{The syntax of Blaise}
\author{Jaap Weel}
\setlength{\parindent}{0pt}
\setlength{\parskip}{1ex plus 0.5ex minus 0.2ex} 
\begin{document}
\maketitle

Blaise, Copyright (C) 2005, Jaap Weel -- This program is free
software; you can redistribute it and/or modify it under the terms of
the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any
later version.  This program is distributed in the hope that it will
be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.  You should have received a
copy of the GNU General Public License along with this program; if
not, write to the Free Software Foundation, Inc., 59 Temple Place,
Suite 330, Boston, MA 02111-1307 USA

\begin{code}
module BlaiseParse where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.Regex
import Data.Char ( digitToInt )
import AstBlaise
\end{code}

\section{Introduction} 

The following is a parser for the programming language Blaise,
inspired by the programming language Oberon as described in the Oberon
report v.1.10.90 by Niklaus Wirth. There exist some minor differences
between Blaise and Oberon:

\begin{itemize}

\item Blaise reads literals (numbers, strings, etc.) in Haskell syntax.

\item Blaise does not include type assertions, since its implementor
does not know how to parse those.

\item Blaise does not support extensible types.

\end{itemize}

There are a few points in the grammar where the $try$ combinator is
used; an effort has been made to restrict these to cases where
backtracking is unlikely to be unwieldy.

The symbols \verb.<|>. and \verb.<?>. in Parsec are written as
$\sqcup$ and $\triangleright$ respectively.

%format <|> = "\sqcup"
%format <?> = "\triangleright"

This is the main entry point into this module.

\begin{code}

parse_blaise filename program_text = parse program filename program_text
\end{code}

\section{Syntax}
The syntax for Blaise is described using Monadic Parser combinators,
Leijen style.

These are some convenient additional combinators on top of the ones
defined in Parsec:

\begin{code}
block x y         = between (key x) (key y)
after f x y       = f x >> y
before f x y      = do r <- y; f x; return r
around x y z      = do r <- x; y; s <- z; return $ (r, s)
kons k x          = x >>= return . k 
perhaps x         = option Nothing (x >>= return . Just)
\end{code}

\section{Vocabulary and representation}

We use lowercase identifiers, unlike the original.
\begin{code}
lexer =  P.makeTokenParser $ emptyDef   
         {  P.commentStart   = "(*",
            P.commentEnd     = "*)",
            P.identStart     = letter,
            P.identLetter    = letter <|> digit,
            P.reservedNames  =
              ["array", "begin", "case", "const", "div", "do", "else",
               "elsif", "end", "exit", "if", "import", "is", "loop",
               "mod", "module", "nil", "of", "or", "pointer", "to",
               "procedure", "record", "repeat", "return", "then",
               "type", "until", "var", "while", "with"],
            P.reservedOpNames = 
              [ "+", "-", "*", "/", "~", "&", ".", ",", ";", "|", "(",
               "[", "{", ":", "=", "^", "=", "#", "<", ">", "<=",
               ">=", "..", ":", ")", "]", "}", ":=" ] } 
\end{code}

The representation of numbers is one area in which we shall diverge
from the Oberon syntax, and instead stick to the standard
C/Java/Haskell syntax. String and character literals, similarly,
follow the Haskell conventions.

\begin{code}
charLiteral     = P.charLiteral lexer
stringLiteral   = P.stringLiteral lexer
whiteSpace      = P.whiteSpace lexer
identifier      = kons Id (P.identifier lexer)
naturalOrFloat  = P.naturalOrFloat lexer
integer         = P.integer lexer
key             = P.reserved lexer
op              = P.reservedOp lexer
semi            = P.semi lexer
dot             = P.dot lexer
semiSep         = P.semiSep lexer
semiTerm x      = many (before op ";" x)
semiSep1        = P.semiSep1 lexer
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
parens          = P.parens lexer
brackets        = P.brackets lexer
\end{code}


\section{Declarations and scope rules}

The scope of all identifiers is lexical, and all identifiers must be
declared. The only exceptions are field designators for record types,
which follow the scope of the identifier that names the record type.

Identifiers can be optionally qualified (like foo.bar). When they are
defined, they can have a * after them to mark that they should be
exported from the current module. 

The following kinds of identifiers exist: constants, types, variables,
and procedures.

\begin{code}
qualident = do  qual       <- perhaps $ try (do  (Id id) <- identifier
                                                 after op "." (return id))
                (Id name)  <- identifier
                return $ QId qual name
                <?> "qualified identifier"

globident = do  (Id name)  <- identifier
                global_p   <- option Local (after op "*" (return Global))
                return $ GId global_p name
                <?> "identifier, with an optional *"
\end{code}

\section{Constant declarations}
\begin{code}
constantDeclaration = do  (name, val) <- around globident (op "=") expr
                          return [ConstantDec name val]
                          <?> "constant declaration"
\end{code}

\section {Type declarations}
We do not use Oberon's "extensible" records at all.

\begin{code}
typeDeclaration = do  (name, t) <- around globident (op "=") ty
                      return [TypeDec name t]
                      <?> "type declaration"

ty =  arrayType <|> recordType <|>  pointerType <|>
      procedureType <|> idType

arrayType      = do  dimensions <- block "array" "of" (commaSep1 expr)
                     kons (TyArray dimensions) ty

recordType     = block "record" "end" $ kons TyRecord (commaSep1 globident)
pointerType    = after key "pointer" $ after key "to" $ kons TyPointer ty
procedureType  = after key "procedure" $ kons TyProc (perhaps formalParameters)
idType         = kons TyId qualident  <?> "simple type"
\end{code}

\section {Variable declarations}

Any one varDeclaration in the code can be an abbreviation for a number
of varDeclarations. We resolve that right here in the parser.

\begin{code}
varDeclaration = do  (vars,t) <- around (commaSep1 globident) (op ":") ty
                     return $ map (\n -> VarDec n t) vars
                     <?> "variable declaration"
\end{code}

\section{Expressions} 

Expressions represent functions from the current environment to a
value. Designators are like lvalues; they are used mainly in
assignment statements.

\begin{code}
designator = do  id <- qualident
                 further (DesVariable id)
                 <?> "designator"

-- For type assertions:
-- do ty <- parens qualident; further (DesAssertTy  base ty)

further base =  (do  field <- after op "." identifier
                     further (DesProject base field) 
                     <?> "field selection (dot)") <|>
                (do  index <- brackets (commaSep1 expr)
                     further (DesSubscript base index)
                     <?> "subscript [brackets]") <|>
                (do  op "^"
                     further (DesReference base)
                     <?> "reference operator ^") <|>  
                (do  return base)
\end{code}

Other expressions. All operators associate left. There are 4 levels of
precedence.

\begin{code}
expr           = buildExpressionParser operators simpleExpr 
                  <|> simpleExpr

simpleExpr     =  literalChar <|> literalString <|> literalNIL <|> 
                  literalNumber <|> parens expr <|> try functionCall <|> term

term           =  kons DesignatorExpr designator

functionCall   =  do  fun <- designator
                      params <- argl
                      return $ FunctionCall fun params
                      <?> "function call"

literalNumber  =  (do  i <- naturalOrFloat
                       return $ case i of
                                     Left n -> LiteralInt n
                                     Right n -> LiteralReal n)
                  <|> 
                  kons LiteralInt integer
                  <?> "integer or real number"

literalChar    =  kons LiteralChar charLiteral <?> "character literal"

literalString  =  kons LiteralString stringLiteral <?> "string literal"

literalNIL     =  key "nil" >> return LiteralNil <?> "the symbol \"nil\""

operators      =  [[ un "~"],
                   [ bin "*",  bin "/",  bir "div", bir "mod", bir "&" ],
                   [ bin "+", bin "-", bir "or", un "+", un "-" ],
                   [ bin "=", bin "#", bin "<=", bin "<", 
                     bin ">=", bin ">", bir "is"]]
  where  bin name  = Infix  (op name >> return (Binop name)) AssocLeft
         bir name  = Infix  (key   name >> return (Binop name)) AssocLeft
         un name   = Prefix (op name >> return (Unop  name)) 
\end{code}

\section{Statements}

Statements represent functions from the current environment to a new
environment.

If statements are tranformed right here in the parser into nested if
statements.

\begin{code}
sqn = semiSep statement

statement =  try assignment  <|> procedureCall    <|> ifStatement      <|> 
             caseStatement   <|> whileStatement   <|> repeatStatement  <|> 
             loopStatement   <|> exitStatement    <|> returnStatement

procedureCall    =  do  proc   <- designator <?> "procedure name"
                        params <- argl
                        return $ ProcCall proc params

argl             =  option [] (parens (commaSep1 expr)) <?> "argument list"

repeatStatement  =  do  actions <- after key "repeat" sqn
                        kons (Repeat actions) (after key "until" expr)

whileStatement   =  block "while" "end" $ 
                    do  (exp, action) <- around expr (key "do") sqn
                        return $ While exp action

loopStatement    =  block "loop" "end" $ kons Loop sqn

exitStatement    =  after key "exit" $ return Exit

returnStatement  =  after key "return" $ kons Return expr

assignment       =  do  (left, right) <- around designator (op ":=") expr
                        return $ Assign left right
                        <?> "assignment"

ifStatement      =  block "if" "end" ifBody
ifBody           =  do  pred        <- expr
                        consequent  <- after key "then" sqn
                        alternate   <- option [] (after key "elsif" elif <|>
                                                  after key "else"  sqn)
                        return $ If pred consequent alternate

elif             =  do  alternate <- ifBody
                        return [alternate]

caseStatement    =  block "case" "end" $
                    do  examinee   <- expr
                        cases      <- after key "of" $ 
                                      sepBy1 onecase (op "|")
                        case_else  <- option [] (after key "else" sqn)
                        return $ Case examinee cases case_else
 
onecase          =  do  (lbl,action) <- around (commaSep1 caselabel) (op ":") sqn
                        return (lbl, action)
                        <?> "a case"

caselabel        =  do  begin  <- expr
                        end    <- perhaps (after op ".." expr)
                        return (begin,end)
\end{code}

\section{Procedure declarations}

\begin{code}
procedureDeclaration  =   do  perhaps (op "*") -- means: do not inline
                              name     <- globident
                              params   <- formalParameters
                              semi
                              decs     <- declarationSequence
                              actions  <- option [] (after key "begin" sqn)
                              key "end"
                              return $ ProcDec name params decs actions
                              <?> "procedure declaration"

forwardDeclaration    =  do  name <- after op "^" globident
                             params <- formalParameters
                             return $ ForwardDec name params
                             <?> "forward procedure declaration"

formalParameters  =  do  params <- option [] (parens (semiSep1 fpsection))
                         returnty <- perhaps (after op ":" qualident)
                         return $ Sig (concat params) returnty

fpsection = do  passing    <- option ByValue (key "var" >> return ByReference)
                (names,t)  <- around (commaSep1 identifier) (op ":") formalType
                return $ map (\i -> (i,passing,t)) names

formalType   =  (after key "procedure" $ kons FTyProc (perhaps formalParameters)) <|>
                formalType'
formalType'  =  (after key "array" $ after key "of" $ kons FTyArray formalType') <|> 
                (kons FTyId qualident)
\end{code}

A declarationSequence is a sequence of declarations that most commonly
happens inside a procedureDeclaration, although it also occurs inside
a module declaration.

\begin{code}
declarationSequence = 
  do  decs <-   many $
                (after key "const"  $ semiTerm constantDeclaration) <|>
                (after key "type"   $ semiTerm typeDeclaration) <|>
                (after key "var"    $ semiTerm varDeclaration)
      procs <-  semiTerm
                (after key "procedure" 
                 (forwardDeclaration <|> procedureDeclaration))
      return ((concat $ concat decs) ++ procs)
\end{code}

\section{Module system}
\begin{code}
moduleDeclaration  =   do  key "module"
                           name     <- identifier
                           semi
                           imports  <- option [] $
                                       block "import" ";" $
                                       commaSep1 importclause
                           decls    <- declarationSequence
                           code     <- option [] sqn
                           name'    <- after key "end" identifier --ignored
                           dot
                           return $ Module name imports decls code

importclause      =  do  localname <- identifier
                         remotename <- option localname $
                                       after op ":=" identifier
                         return $ Import localname remotename
                     <?> "module import clause"
\end{code}

\section{A program is sequence of modules}
\begin{code}
program = do  whiteSpace
              modules <- many moduleDeclaration
              return (Program modules)
\end{code}

\end{document}
