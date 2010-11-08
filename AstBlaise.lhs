\documentclass{scrartcl}
\usepackage{amssymb}
%include lhs2TeX.fmt
%include lhs2TeX.sty
\title{The AST of Blaise}
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
module AstBlaise where
import PPrint
\end{code}

\section{Introduction} 

This is the AST for the programming language Blaise.

The symbols \verb.<>., \verb.<$>. and \verb.<+>. in PPrint are written
as $\Diamond$, $\asymp$ and $\Join$ respectively.

%format <> = "\Diamond{}"
%format <$> = "\asymp{}"
%format <+> = "\Join{}"

\section{AST}
\begin{code}
data Program = Program [Module]

data Module  = Module Id [Import] [Dec] [Stat]
               deriving(Show)

data Import  = Import Id Id
               deriving(Show)

-- Declarations
data Dec  = ConstantDec GId Exp
          | TypeDec GId Type
          | VarDec GId Type
          | ProcDec GId Sig [Dec] [Stat]
          | ForwardDec GId Sig
            deriving(Show)

data Sig  = Sig [(Id,Passing,FormalType)] (Maybe QId)
            deriving(Show)

data Passing  = ByValue | ByReference
                deriving(Show)

-- Statements
data Stat  = ProcCall Designator [Exp]
           | If       Exp [Stat] [Stat]
           | Case     Exp [ ([(Exp,Maybe Exp)] , [Stat]) ] [Stat]
           | While    Exp [Stat]
           | Repeat   [Stat] Exp
           | Loop     [Stat]
           | Exit
           | Return   Exp
           | Assign   Designator Exp
             deriving(Show)

-- Expressions
data Exp  = LiteralInt Integer
          | LiteralReal Double
          | LiteralChar Char
          | LiteralString String
          | LiteralNil
          | DesignatorExpr Designator
          | FunctionCall Designator [Exp]
          | Unop String Exp
          | Binop String Exp Exp
            deriving(Show)

-- "Designators" (a.k.a. lvalues)
data Designator  = DesProject   Designator Id
                 | DesSubscript Designator [Exp]
                 | DesAssertTy  Designator QId
                 | DesReference Designator
                 | DesVariable  QId
                   deriving(Show)

-- Types
data Type  = TyId      QId
           | TyArray   [Exp] Type
           | TyRecord  [GId]
           | TyPointer Type
           | TyProc    (Maybe Sig)
             deriving(Show)

data FormalType  = FTyArray FormalType
                 | FTyId    QId
                 | FTyProc  (Maybe Sig)
                   deriving(Show)

-- Identifiers
data Id   = Id  String deriving(Show)
data QId  = QId (Maybe String) String deriving(Show)
data GId  = GId Ex String deriving(Show)
data Ex   = Global | Local deriving(Show)

\end{code}

\section{Pretty printing}

Some extra utility functions for pretty printing.

\begin{code}
fold f []       = empty
fold f ds       = foldr1 f ds
tab             = indent 4
enum sep p x    = sep  $ punctuate p (map pretty x)
notempty [] y   = empty
notempty _  y   = y
\end{code}

Pretty printing specification for the Blaise AST.

\begin{code}
instance Pretty Program where
  pretty (Program modules) = vsep $ map pretty modules

instance Pretty Module where
  pretty (Module name imports declarations code) = 
    text "module" <+> pretty name <> semi <$>
    notempty imports (text "import" <$> 
                      tab (enum vsep comma imports) <> semi) <$>
    notempty declarations  (enum vsep semi declarations <> semi) <$>
    notempty code          (enum vsep semi code) <$>
    text "end" <+> pretty name <> dot

instance Pretty Import where
  pretty (Import local global) = pretty local <+> text ":=" <+> pretty global

instance Pretty Dec where
  pretty (ConstantDec g e)  =  text "const" <+> 
                               pretty g <+> text "=" <+> pretty e
  pretty (TypeDec g t)      =  text "type" <+>
                               pretty g <+> text "=" <+> pretty t
  pretty (VarDec g t)       =  text "var" <+>
                               pretty g <+> text ":" <+> pretty t
  pretty (ProcDec g s d c)  =  text "procedure" <+> pretty g <>
                               pretty s <> semi <$>
                               notempty d (tab (enum vsep semi d)<>line) <>
                               notempty c (text "begin" <$>
                                           tab (enum vsep semi c)) <$>
                               text "end"
  pretty (ForwardDec g s)   =  text "procedure^" <+> pretty g <> pretty s

instance Pretty Sig where
  pretty (Sig args ret)  =  (notempty args $ parens $ hsep $
                             (punctuate semi (map pretty' args))) <>
                            case ret of 
                                     Just x -> space <> colon <+> pretty x
                                     Nothing -> empty
    where pretty' (id,passing,ty) =  (case passing of
                                        ByValue -> empty
                                        ByReference -> text "var" <> space) <>
                                     pretty id <+> colon <+> pretty ty

instance Pretty Stat where
  pretty (ProcCall d e)  =  pretty d <> notempty e (parens (enum vsep comma  e))
  pretty (If e x y)      =  text "if" <+> pretty e <+> 
                            text "then" <$> tab (enum vsep semi x) <$> 
                            text "else" <$> tab (enum vsep semi y) <$>
                            text "end"
  pretty (Case e c f)    =  text "case" <+> pretty e <+> text "of" <$>
                            tab (fold barsep (map pretty' c)) <$>
                            notempty f  (text "else" <$> 
                                         tab (enum vsep semi f) <> line) <>
                            text "end"
    where pretty' (conditions, action) = 
            (hsep $ 
             punctuate comma (map pretty'' conditions)) <> 
            colon <$> tab (enum vsep semi action)
          pretty'' (x, Just y)   = 
            pretty x <> text ".." <> pretty y
          pretty'' (x, Nothing)  = 
            pretty x
          x `barsep` y = 
            x <$> text "|" <> y

  pretty (While e s)     =  text "while" <+> pretty e <+> text "do" <$>
                            tab (enum vsep semi s) <$> text "end"
  pretty (Repeat s e)    =  text "repeat" <$> tab (enum vsep semi s) <$>
                            text "until" <+> pretty e
  pretty (Loop s)        =  text "loop" <$> tab (enum vsep semi s) <$> text "end"
  pretty (Exit)          =  text "exit"
  pretty (Return e)      =  text "return" <+> pretty e
  pretty (Assign d e)    =  pretty d <+> text ":=" <+> pretty e


instance Pretty Exp where
  pretty (LiteralInt i)        = integer i
  pretty (LiteralReal d)       = double d
  pretty (LiteralChar c)       = char c
  pretty (LiteralString s)     = string s
  pretty (LiteralNil)          = text "nil"
  pretty (DesignatorExpr d)    = pretty d
  pretty (FunctionCall d e)    = pretty (ProcCall d e)
  pretty (Unop op x)           = text op <> pretty x
  pretty (Binop op x y)        = parens (pretty x <+> text op <+> pretty y)

instance Pretty Designator where
  pretty (DesProject d i)    = pretty d <> dot <> pretty i
  pretty (DesSubscript d e)  = pretty d <> brackets (enum vsep comma  e)
  pretty (DesAssertTy d q)   = pretty d <> parens (pretty q)
  pretty (DesReference d)    = pretty d <> text "^"
  pretty (DesVariable q)     = pretty q


instance Pretty Type where
  pretty (TyId q)       =  pretty q
  pretty (TyArray d t)  =  text "array" <+> enum vsep comma d <+>
                           text "of" <+> pretty t
  pretty (TyRecord g)   =  text "record" <$> tab (enum vsep semi g) <$> 
                           text "end"
  pretty (TyPointer t)  =  text "pointer to" <+> pretty t
  pretty (TyProc (Just s))  =  text "procedure" <> pretty s
  pretty (TyProc (Nothing)) =  text "procedure"

instance Pretty FormalType where
  pretty (FTyArray t)  = text "array of" <+> pretty t
  pretty (FTyId q)     = pretty (TyId q)
  pretty (FTyProc s)   = pretty (TyProc s)

instance Pretty Id where    pretty (Id s) = text s
instance Pretty GId where   pretty (GId e s) = text s <> pretty e
instance Pretty QId where   pretty (QId (Just x) y) = text x <> dot <> text y
                            pretty (QId (Nothing) y) = text y
instance Pretty Ex where    pretty Global = text "*"
                            pretty Local = text ""


prettyShow p = (displayS (renderPretty 1.0 80 (pretty p))) ""
\end{code}

\end{document}