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

module AstBloop
  ( Program(..)
  , TopLevel(..)
  , Statement(..)
  , Exp(..)
  , CellNumber(..)
  , Symbol(..)
  , prettyShow
  )
where
import PPrint
import Data.Generics

-- AST structure of Floop --------------------------------------------
data Program = Program [TopLevel] deriving(Typeable, Data)

data TopLevel = Procedure Exp Exp [Exp] Statement
              | TopExp Exp
              deriving(Typeable, Data)
-- Procedure name default-value arguments body

data Statement = Quit Integer
               | Abort Integer
               | Block Integer [Statement]
               | Muloop Integer Statement
               | Loop Exp Integer Statement
               | Ifthen Exp Statement
               | Assign Exp Exp
               deriving(Typeable, Data)

data Exp = Number Integer
         | Var Symbol
         | Boolean Bool
         | Cell CellNumber
         | Output
         | Binop String Exp Exp
         | Funcall Symbol [Exp]
         deriving(Typeable, Data)

newtype CellNumber = CellNumber Integer
                    deriving(Typeable, Data, Pretty, Eq)

data Symbol = Symbol String deriving(Read, Typeable, Data)

-- The deriving code for Show is too smart...
instance Show Symbol where show (Symbol s) = s
instance Show CellNumber where show (CellNumber i) = show i

-- Pretty printing ---------------------------------------------------
prettyShow s = (displayS (renderPretty 1.0 80 (pretty s))) ""

instance Pretty Program where
  pretty (Program ps) = vcat (punctuate (semi <> linebreak) (map pretty ps))
                        <> dot

instance Pretty TopLevel where
  pretty (Procedure _ name argl body) =
    text "DEFINE PROCEDURE" <+> (dquotes $ pretty name) <+>
    list (map pretty argl) <> colon <$> pretty body
  pretty (TopExp e) = pretty e

instance Pretty Statement where
  pretty (Quit i) = text "QUIT BLOCK" <+> pretty i
  pretty (Abort i) = text "ABORT LOOP" <+> pretty i
  pretty (Block i s) = text "BLOCK" <+> pretty i <> text ": BEGIN" <$>
                       indent 6 (vsep (map ((<> semi) . pretty) s)) <$>
                       text "BLOCK" <+> pretty i <> text ": END" 
  pretty (Muloop _ s) = text "MU-LOOP:" <$> pretty s
  pretty (Loop e _ s) = text "LOOP AT MOST" <+> pretty e <+> text "TIMES"
                        <> colon <$> pretty s
  pretty (Ifthen e s) = text "IF" <+> pretty e <> text ", THEN:" <$> pretty s
  pretty (Assign e f) = pretty e <+> text "<=" <+> pretty f

instance Pretty Exp where
  pretty (Number i) = pretty i
  pretty (Var s) = pretty s
  pretty (Boolean True) = text "YES"
  pretty (Boolean False) = text "NO"
  pretty (Cell i) = text "CELL" <> parens (pretty i)
  pretty (Output) = text "OUTPUT"
  pretty (Binop s e f) = braces (pretty e <+> text s <+> pretty f)
  pretty (Funcall s es) = pretty s <> list (map pretty es)

instance Pretty Symbol where
  pretty (Symbol s) = text s

