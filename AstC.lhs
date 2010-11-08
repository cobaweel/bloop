\documentclass{scrartcl}
\usepackage{amssymb}
%include lhs2TeX.fmt
%include lhs2TeX.sty
\title{The AST of ``C'' (sort of)}
\author{Jaap Weel}
\setlength{\parindent}{0pt}
\setlength{\parskip}{1ex plus 0.5ex minus 0.2ex} 
\begin{document}
\maketitle

Originally snarfed from the FrontC code, then translated from ML using
Emacs macros. Originally Copyright Hugues Cassé, released under
GPL. I've thrown out all K$\&$R specific stuff, but kept the ANSI and GNU
stuff. I've redone all of the declaration stuff. 

\begin{verbatim}
"cabs -- abstract syntax for FrontC, 
Project: frontc, 
Version: 2.1, 
Date: 4.7.99,
Author: Hugues Cass'e."
\end{verbatim}

Bloop, Copyright (C) 2005, Jaap Weel -- This program is free
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
module AstC where
import PPrint
import Data.Maybe ( fromMaybe )
import Data.Generics
\end{code}

The symbols \verb.<>., \verb.<$>. and \verb.<+>. in PPrint are written
as $\Diamond$, $\asymp$ and $\Join$ respectively.

%format <> = "\Diamond{}"
%format <$> = "\asymp{}"
%format <+> = "\Join{}"


\section{Declarations}
 The C declaration syntax is one REALLY big ball of hair. It is
 virtually impossible to come up with an AST that captures all legal C
 declarations, but nothing else, and is still reasonably
 concise. 

 Instead, I will provide a *subset* of the C language, which does not
 include some gratuitous abbreviations. A parser could expand those
 abbreviations on the fly, and anything that targets C automatically
 probably doesn't use them. It also does not include many "K$\&$R"
 misfeatures absent or deprecated in ANSI.

 Examples of abbreviations not supported are:

\begin{verbatim}
 int x, y;    => int x; int y;
 int x, *x;   => int x; int* x;
\end{verbatim}

 This greatly simplifies the abstract grammar. It does make it
 impossible to parse a C file and pretty-print the exact same thing
 back out; rather, a small bit of preprocessing becomes irreversible.
\begin{code}
data File  = File [Declaration] 
             deriving (Typeable, Data)

data Body  = Body [Declaration] [Statement] 
             deriving (Typeable, Data)

data Declaration  = Declare Type Declarator
                  | Initialize Type Declarator Initializer
                  | ProtoFun Type String [Declaration]
                  | DeclareFun Type String [Declaration] Body
                  | Typedef Type String 
                    deriving (Typeable, Data)

data Initializer  = SimpleInit Expression | CompoundInit [Initializer] 
                    deriving (Typeable, Data)

data Declarator  = Array Declarator [Expression] 
                 | Name String 
                   deriving (Typeable, Data)

data Signature   = Signature Type [Declaration] 
                   deriving (Typeable, Data)

data Type  =  TypeName  String | Char | Int | Float | Double | Void   
           |  Auto Type | Extern Type | Register Type | Static Type   
           |  Short Type | Long Type | Signed Type | Unsigned Type    
           |  PointerTo Type                                          
           |  FunctionPointer  Type [Type]                            
           |  VFunctionPointer Type [Type]                            
           |  Struct (Maybe String) [Declaration]                     
           |  Union  (Maybe String) [Declaration]                     
           |  Enum   (Maybe String) [Declaration] 
              deriving (Typeable, Data)
\end{code}

\section{Statements}
\begin{code}
-- Statements  -------------------------------------------------------
data Statement =
        -- A simple Expression, usually an assignment. 
          Computation Expression
        -- A block between braces 
        | Block Body
        -- "if" Statement with or without else-part. 
        | If Expression Statement (Maybe Statement)
        -- "while" Statement. 
        | While Expression Statement
        -- "do ... while" Statement 
        | Dowhile Expression Statement
        -- "for" Statement. 
        | For Expression Expression Expression Statement
        -- "break" Statement. 
        | Break
        -- "continue" Statement. 
        | Continue
        -- "return" Statement with an Expression or with Nothing. 
        | Return (Maybe Expression)
        -- "switch" Statement. Cases are put in the sub-Statement as labels. 
        | Switch Expression Statement
        -- "case" Statement as a label. 
        | Case Expression 
        -- "default" Statement as a label. 
        | Default 
        -- "label" Statement whose sub-Statement follows colon ":". 
        | Label LabelName 
        -- "goto" Statement. 
        | Goto GotoTarget
          deriving (Typeable, Data)

newtype LabelName = LabelName String deriving(Typeable, Data)
newtype GotoTarget = GotoTarget String deriving(Typeable, Data)
\end{code}

\section{Expressions}
\begin{code}
data Expression =
        -- Unary operator use. 
          Unary Unary_operator Expression
        -- Binary operator use. 
        | Binary Binary_operator Expression Expression
        -- "condition ? then-Expression : else-Expression" operator. 
        | Question Expression Expression Expression
        -- "(type)expresson" type casting. 
        | Cast Type Expression
        -- Function call. 
        | Call Expression [Expression]
        -- Constant value. 
        | Literal Literal
        -- Access to an identifier. 
        | Variable String
        -- "sizeof" with Expression. 
        | Expr_sizeof Expression
        -- "sizeof" with type. 
        | Type_sizeof Type
        -- Access to an array item; 
        | Index Expression Expression
        -- Indirection through ".". 
        | Memberof Expression String
        -- Pointer indirection through "->". 
        | Memberofptr Expression String
        -- GNU braces inside an Expression. 
        | Gnu_body Body 
          deriving (Typeable, Data)
\end{code}
\subsection{Literals}
\begin{code}
data Literal  = Lit_int String  
              | Lit_float String
              | Lit_char String 
              | Lit_string String 
               deriving (Typeable, Data)
\end{code}
\subsection{Operators}
\begin{code}
data Binary_operator  = 
       Add                      -- \verb@+@ operator. 
    |  Sub                      -- \verb@-@ operator. 
    |  Mul                      -- \verb@*@ operator. 
    |  Div                      -- \verb@/@ operator. 
    |  Mod                      -- \verb@%@ operator. 
    |  And                      -- \verb@&&@ operator. 
    |  Or                       -- \verb@||@ operator. 
    |  Band                     -- \verb@&@ operator. 
    |  Bor                      -- \verb@|@ operator. 
    |  Xor                      -- \verb@^@ operator. 
    |  Shl                      -- \verb@<<@ operator. 
    |  Shr                      -- \verb@>>@ operator. 
    |  Eq                       -- \verb@==@ operator. 
    |  Ne                       -- \verb@!=@ operator. 
    |  Lt                       -- \verb@<@ operator. 
    |  Gt                       -- \verb@>@ operator. 
    |  Le                       -- \verb@<=@ operator. 
    |  Ge                       -- \verb@>=@ operator. 
    |  Assign                   -- \verb@=@ operator. 
    |  Add_assign               -- \verb@+=@ operator. 
    |  Sub_assign               -- \verb@-=@ operator. 
    |  Mul_assign               -- \verb@*=@ operator. 
    |  Div_assign               -- \verb@/=@ operator. 
    |  Mod_assign               -- \verb@%=@ operator. 
    |  Band_assign              -- \verb@&=@ operator. 
    |  Bor_assign               -- \verb@|=@ operator. 
    |  Xor_assign               -- \verb@\^=@ operator. 
    |  Shl_assign               -- \verb@<<=@ operator. 
    |  Shr_assign               -- \verb@>>=@ operator. 
    |  Comma                    -- \verb@,@ operator.
       deriving (Typeable, Data)

data Unary_operator =
          Minus                 -- \verb@-@ operator. 
        | Plus                  -- \verb@+@ operator. 
        | Not                   -- \verb@!@ operator. 
        | Bnot                  -- \verb@~@ operator. 
        | Memof                 -- \verb@*@ operator. 
        | Addrof                -- \verb@\&@ operator. 
        | Preincr               -- \verb@++@ pre-incrementation. 
        | Predecr               -- \verb@--@ pre-decrementation. 
        | Posincr               -- \verb@++@ post-incrementation. 
        | Posdecr               -- \verb@--@ post-decrementation. 
          deriving (Typeable, Data)
\end{code}

\section{Pretty printing}          
\subsection{Utility functions}          
\begin{code}
notempty :: [a] -> Doc -> Doc
notempty [] _                      = empty
notempty _  y                      = y

notnothing :: Maybe a -> Doc -> Doc
notnothing Nothing _               = empty
notnothing _       y               = y

commasep :: (Pretty a) => [a] -> Doc
commasep x                         = hsep $ punctuate comma (map pretty x)

spacesep :: (Pretty a) => [a] -> Doc
spacesep x                         = hsep $ (map pretty x)

tab :: Doc -> Doc
tab x                              = indent 4 x

prettyShow s = (displayS (renderPretty 1.0 80 (pretty s))) ""
\end{code}

\section{The pretty-printer}
\begin{code}
instance Pretty File where
  pretty (File definitionL)        = (vsep $ map semiize definitionL) <$>
                                     text "\n\n\n"

instance Pretty Body where
  pretty (Body defl statl)         = text "{" <$>
                                     (tab $ vsep $
                                      map semiize defl ++
                                      map pretty statl) <$>
                                     text "}"

semiize d@(DeclareFun _ _ _ _)     = pretty d
semiize d                          = pretty d <> semi
\end{code}
\begin{code}
instance Pretty Declaration where
  pretty (Declare ty declarator) = 
    pretty ty <+> pretty declarator
  pretty (Initialize ty declarator ini) = 
    pretty ty <+> pretty declarator <+> text "=" <+> pretty ini
  pretty (ProtoFun ty name argl) = 
    pretty ty <+> text name <> commasep argl
  pretty (DeclareFun ty name argl body) = 
    pretty ty <+> text name <> parens (commasep argl) <$> pretty body
  pretty (Typedef ty s) =
    pretty ty <+> text s
\end{code}
\begin{code}
instance Pretty Type where
  pretty (Char)                 = text "char"
  pretty (Int)                  = text "int"
  pretty (Float)                = text "float"
  pretty (Double)               = text "double"
  pretty (Void)                 = text "void"
  pretty (TypeName str)         = text str
  pretty (Auto t)               = text "auto" <+> pretty t
  pretty (Extern t)             = text "extern" <+> pretty t
  pretty (Register t)           = text "register" <+> pretty t
  pretty (Static t)             = text "static" <+> pretty t
  pretty (Short t)              = text "short" <+> pretty t
  pretty (Long t)               = text "long" <+> pretty t
  pretty (Signed t)             = text "signed" <+> pretty t
  pretty (Unsigned t)           = text "unsigned" <+> pretty t
  pretty (PointerTo t)          = parens (pretty t) <+> text "*"

  pretty (Struct name declL)    =  text "struct" <>
                                   (case name of Just s -> space <> text s
                                                 Nothing -> empty) <$>
                                   tab (pretty declL)
  pretty (Union name declL)     =  text "union" <>
                                   (case name of Just s -> space <> text s
                                                 Nothing -> empty) <$>
                                   tab (pretty declL)
  pretty (Enum name declL)      =  text "enum" <>
                                   (case name of Just s -> space <> text s
                                                 Nothing -> empty) <+>
                                   tab (pretty declL)

  -- ***TODO*** Verify that these are correct. This is hairy.
  pretty (FunctionPointer _ _)   = error "unimplemented"
  pretty (VFunctionPointer _ _)  = error "unimplemented"
\end{code}
\begin{code}
instance Pretty Initializer where
  pretty (SimpleInit expression)      = pretty expression
  pretty (CompoundInit initializerL)  = braces (commasep initializerL)
\end{code}
\begin{code}
instance Pretty Declarator where
  pretty (Array name expL)      =  pretty name <+>
                                           hsep (map (brackets . pretty) expL)
  pretty (Name name)            =  pretty name
\end{code}
\begin{code}
kr_block :: Statement -> Doc
kr_block s@(Block body)         =  pretty body
kr_block s                      =  line <> tab (pretty s)

instance Pretty Statement where
  pretty (Computation expr)     =  pretty expr <> semi
  pretty (Block body)           =  pretty body
  pretty (If expr a b)          =  text "if" <+> parens (pretty expr) <+> 
                                   kr_block a <>
                                   (notnothing b $
                                    line <> text "else" <+>
                                    (fromMaybe empty (b >>= Just . kr_block)))
  pretty (While expr stat)      =  text "while" <+> parens (pretty expr) <+> 
                                   kr_block stat
  pretty (Dowhile expr stat)    =  text "do" <$> tab (pretty stat) <$>
                                   text "while" <+> parens (pretty expr) <> semi
  pretty (For x y z stat)       =  text "for" <+>
                                   parens (pretty x <> semi <+>
                                           pretty y <> semi <+>
                                           pretty z) <+> kr_block stat
  pretty (Break)                =  text "break;" 
  pretty (Continue)             =  text "continue;" 
  pretty (Return expr)          =  text "return" <+> 
                                   notnothing expr (pretty expr) <> semi
  pretty (Switch expr stat)     =  text "switch" <+> parens (pretty expr) <$>
                                   tab (pretty stat)
  pretty (Case expr)            =  text "case" <+> pretty expr <+> colon
  pretty (Default)              =  text "default:" 
  pretty (Label str)            =  pretty str <> colon
  pretty (Goto str)             =  text "goto" <+> pretty str <> semi

instance Pretty LabelName where pretty (LabelName s) = text s
instance Pretty GotoTarget where pretty (GotoTarget s) = text s

\end{code}
\begin{code}
instance Pretty Binary_operator where
  pretty (Add)                  =  text "+" 
  pretty (Sub)                  =  text "-" 
  pretty (Mul)                  =  text "*" 
  pretty (Div)                  =  text "/" 
  pretty (Mod)                  =  text "%" 
  pretty (And)                  =  text "&&" 
  pretty (Or)                   =  text "||" 
  pretty (Band)                 =  text "&" 
  pretty (Bor)                  =  text "|" 
  pretty (Xor)                  =  text "^" 
  pretty (Shl)                  =  text "<<" 
  pretty (Shr)                  =  text ">>" 
  pretty (Eq)                   =  text "==" 
  pretty (Ne)                   =  text "!=" 
  pretty (Lt)                   =  text "<" 
  pretty (Gt)                   =  text ">" 
  pretty (Le)                   =  text "<=" 
  pretty (Ge)                   =  text ">=" 
  pretty (Assign)               =  text "=" 
  pretty (Add_assign)           =  text "+=" 
  pretty (Sub_assign)           =  text "-=" 
  pretty (Mul_assign)           =  text "*=" 
  pretty (Div_assign)           =  text "/=" 
  pretty (Mod_assign)           =  text "%=" 
  pretty (Band_assign)          =  text "&=" 
  pretty (Bor_assign)           =  text "|=" 
  pretty (Xor_assign)           =  text "^=" 
  pretty (Shl_assign)           =  text "<<=" 
  pretty (Shr_assign)           =  text ">>=" 
  pretty (Comma)                =  text ","

instance Pretty Unary_operator where
  pretty (Minus)                =  text "-" 
  pretty (Plus)                 =  text "+" 
  pretty (Not)                  =  text "!" 
  pretty (Bnot)                 =  text "~" 
  pretty (Memof)                =  text "*" 
  pretty (Addrof)               =  text "&" 
  pretty (Preincr)              =  text "++" 
  pretty (Predecr)              =  text "--" 
  pretty (Posincr)              =  text "++" 
  pretty (Posdecr)              =  text "--" 
\end{code}

The pretty-printing of expressions is somewhat conservative. It tries
to omit the most superfluous parentheses, but still inserts more than
are strictly necessary. Trying to emulate a human C programmer in this
regard would be tricky; most programmer's rely on some, but not all,
of the precedence rules.

\begin{code}
instance Pretty Expression where
  pretty (Unary op@Posincr expr)  =  parenize expr <> pretty op
  pretty (Unary op@Posdecr expr)  =  parenize expr <> pretty op
  pretty (Unary op expr)          =  pretty op <> parenize expr
  pretty (Binary op x y)          =  parenize x <+> pretty op <+> 
                                     parenize y
  pretty (Question x y z)         =  parenize x <+> text "?" <+> 
                                     parenize y <+> text ":" <+> 
                                     parenize z
  pretty (Cast btype expr)        =  (parens $ pretty btype) <+> 
                                     parenize expr
  pretty (Call expr expL)         =  parenize expr <> 
                                     parens (commasep expL)
  pretty (Literal constant)       =  pretty constant
  pretty (Variable str)           =  text str
  pretty (Expr_sizeof expr)       =  text "sizeof" <> parenize expr
  pretty (Type_sizeof btype)      =  text "sizeof" <> (parens $ pretty btype)
  pretty (Index a i)              =  parenize a <> brackets (pretty i)
  pretty (Memberof expr str)      =  parenize expr <> text "." <> text str
  pretty (Memberofptr expr str)   =  parenize expr <> text "->" <> text str
  pretty (Gnu_body body)          =  parens $ pretty body

parenize :: Expression -> Doc
parenize e@(Literal _)            =  pretty e
parenize e@(Variable _)           =  pretty e
parenize e@(Gnu_body _)           =  pretty e
parenize e                        =  parens $ pretty e

instance Pretty Literal where
  pretty (Lit_int str)          =  text str
  pretty (Lit_float str)        =  text str
  pretty (Lit_char str)         =  text str
  pretty (Lit_string str)       =  text $ show str

\end{code}

\end{document}

