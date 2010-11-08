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

module AnalyzeBloop ( scells, Cell (..)  ) where
import AstBloop
import Data.Generics ( everything, mkQ, Typeable, Data )

data Cell = C Integer deriving(Eq)

scells :: Statement -> [Cell]
scells = everything (++) (mkQ [] gather_cell)

gather_cell :: CellNumber -> [Cell]
gather_cell (CellNumber i) = [C i]

{-
Another chunk of code mercilessly killed by Scrap Your Boilerplate.

-- Determine what "CELL()" references exist in a piece of BLOOP code -
scells (Block _ ss)        = concat (map scells ss)
scells (Muloop _ s)        = scells s
scells (Loop e _ s)        = ecells e ++ scells s
scells (Ifthen e s)        = ecells e ++ scells s
scells (Assign e f)        = ecells e ++ ecells f
scells (_)                 = []
ecells (Cell i)            = [ C i ]
ecells (Binop _ e f)       = ecells e ++ ecells f
ecells (Funcall _ es)      = concat (map ecells es)
ecells (_)                 = []
-}