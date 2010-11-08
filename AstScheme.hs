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

module AstScheme where
import PPrint

-- AST structure of Scheme (simplistically) --------------------------
data S = Atom String | List [S] deriving(Eq)
data Scheme = Scheme [S]

-- Pretty printing ---------------------------------------------------
instance Pretty S where
  pretty (Atom s) = text s
  pretty (List l) = prettyCons l
    where prettyCons = PPrint.parens . align. cat .
                       (punctuate space) .
                       (map pretty)

instance Pretty Scheme where
  pretty (Scheme l) = vsep (map pretty l)

prettyShow s = (displayS (renderPretty 1.0 80 (pretty s))) ""
