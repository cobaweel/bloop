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

module AnalyzeC ( gotos ) where
import AstC
import Data.Generics ( everything, mkQ )

gather :: GotoTarget -> [String]
gather (GotoTarget t) = [t]

gotos :: Declaration -> [String]
gotos = everything (++) (mkQ [] gather)

{- I shall preserve the following code for posterity as a testament
to the great usefulness of generic programming. It is the initial
implementation I wrote of the 'gotos' function, before deciding this
was maybe the time for me to read up on Scrap Your Boilerplate...

class Gotos a where
  gotos :: a -> [String]
  gotomaybe :: Gotos a => Maybe a -> [String]

gotomaybe = (concatMap gotos) . maybeToList

instance Gotos Body where
  gotos (Body decls stats) = concatMap gotos stats

instance Gotos Declaration where
  gotos (DeclareFun _ _ _ body) = gotos body

instance Gotos Statement where
  gotos (Computation e) = gotos e
  gotos (Block b) = gotos b
  gotos (If e b c) = gotos e ++ gotos b ++ gotomaybe c
  gotos (While e s) = gotos e ++ gotos s
  gotos (Dowhile e s) = gotos e ++ gotos s
  gotos (For e f g s) = gotos e ++ gotos f ++ gotos g ++ gotos s
  gotos (Return e) = gotomaybe e
  gotos (Switch e s) = gotos e ++ gotos s
  gotos (Goto s) = [s]
  gotos _ = []

instance Gotos Expression where
  gotos (Unary _ e) = gotos e
  gotos (Binary _ e f) = gotos e ++ gotos f
  gotos (Question e f g) = gotos e ++ gotos f ++ gotos g
  gotos (Cast _ e) = gotos e
  gotos (Call e es) = gotos e ++ concatMap gotos es
  gotos (Expr_sizeof e) = gotos e
  gotos (Index e f) = gotos e ++ gotos f
  gotos (Memberof e _) = gotos e
  gotos (Memberofptr e _) = gotos e
  gotos (Gnu_body b) = gotos b
  gotos _ = []
  
-}