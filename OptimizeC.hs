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

module OptimizeC ( optimize ) where 
import Control.Monad
import Data.Maybe
import Data.Generics
import AstC
import AnalyzeC

-- Why this is not in Control.Monad completely escapes me...
frobnicate :: (MonadPlus m) => (a -> m a) -> [a] -> m a
frobnicate f xs = msum (map f xs)

optimize :: File -> File
optimize fn = blocksGone $ labelsGone fn

----------------------------------------------------------------------
-- Blocks 
blocksGone :: File -> File
blocksGone (File ds) = File (map od ds)

od :: Declaration -> Declaration
od (DeclareFun ty name argl body)  = DeclareFun ty name argl (ob body)
od d                               = d

ob :: Body -> Body
ob (Body decls ss)                 = Body decls (concat $ map oss ss)

obs :: Body -> [Statement]
obs (Body [] ss)                   = concat $ map oss ss
obs b                              = [Block $ ob b]

oss :: Statement -> [Statement]
oss (Block body)                   = obs body
oss s                              = [os s]

os :: Statement -> Statement
os (Block (Body [] [s]))           = os s
os (Block b)                       = Block (ob b)
os (If e s1 s2)                    = If (oe e) (os s1) ((liftM os) s2)
os (While e s)                     = While (oe e) (os s)
os (Dowhile e s)                   = Dowhile (oe e) (os s)
os (For e1 e2 e3 s)                = For (oe e1) (oe e2) (oe e3) (os s)
os (Return e)                      = Return ((liftM oe) e)
os s                               = s

oe :: Expression -> Expression
oe (Gnu_body b)                    = Gnu_body (ob b)
oe e                               = e

----------------------------------------------------------------------
-- Labels

-- It remains to be seen how to Scrap *this* Boilerplate...

labelsGone (File ds)                   = File $
                                         map decl_labels ds
decl_labels f@(DeclareFun a b c body)  = DeclareFun a b c $
                                         body_labels (gotos f) body
decl_labels other                      = other
body_labels g (Body a s)               = Body a $ 
                                         concatMap (maybeToList . labels g) s

labels                    :: [String] -> Statement -> Maybe Statement
labels g (Block b)        = return $
                            Block $ body_labels g b
labels g (If e b c)       = return $
                            If e (ee $ labels g b) (c >>= labels g)
labels g (While e s)      = return $
                            While e $ (ee $ labels g s)
labels g (Dowhile e s)    = return $
                            Dowhile e $ (ee $ labels g s)
labels g (For a b c s)    = return $
                            For  a b c $ (ee $ labels g s)
labels g (Switch e s)     = return $
                            Switch e $ (ee $ labels g s)
labels g l@(Label (LabelName str)) = case str `elem` g of
                                      True -> return l
                                      False -> fail ""
labels _ other            = return $ other

ee = fromMaybe (Block $ Body [] [])

