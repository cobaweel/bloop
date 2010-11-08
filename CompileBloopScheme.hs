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

module CompileBloopScheme ( compile ) where 
import Data.List ( nub )
import AstBloop
import AnalyzeBloop ( scells, Cell(..) )
import qualified AstScheme as S

-- Bloop/Floop to Scheme "Compiler" ----------------------------------

-- Scheme macros and constants ---------------------------------------
_when      = S.Atom "_when"             
_defproc   = S.Atom "_defproc"          
_block     = S.Atom "_block"            
_muloop    = S.Atom "_muloop"           
_bloop     = S.Atom "_bloop"            
_show      = S.Atom "_show"             
_set       = S.Atom "set!"              
_false     = S.Atom "#f"                
_true      = S.Atom "#t"                
_zero      = S.Atom "0"                 
_string s  = S.Atom ("\"" ++ s ++ "\"") 
_output    = S.Atom "_output"           
_cell i    = S.Atom ("_cell" ++ show i) 
_quit i    = S.Atom ("_quit" ++ show i) 
_abort i   = S.Atom ("_abort" ++ show i)
_scells c  = map (\(C i) -> _cell i) (scells c)

-- Compile Bloop expressions -----------------------------------------
ce (Number i)        = S.Atom (show i)
ce (Var (Symbol s))  = S.Atom s
ce (Boolean True)    = _true
ce (Boolean False)   = _false
ce (Cell i)          = _cell i
ce (Output)          = _output
ce (Binop o e1 e2)   = S.List [ S.Atom o, ce e1, ce e2]
ce (Funcall (Symbol s) es) = S.List $ (S.Atom s):(map ce es)

-- Compile Bloop Statements ------------------------------------------
-- This translation is fairly straightforward because the prelude
-- defines some macros that correspond to Bloop statements.
cs (Quit i)      = S.List [ _quit i ]
cs (Abort i)     = S.List [ _abort i ]

cs (Block i ss)  = S.List $ [ _block, _quit i] ++ map cs ss ++ [ _output ]
cs (Muloop i s)  = S.List $ [ _muloop, _abort i, cs s]
cs (Loop e i s)  = S.List $ [ _bloop, _abort i, ce e, cs s ]
cs (Ifthen e s)  = S.List $ [ _when, ce e , cs s ]
cs (Assign e f)  = S.List $ [ _set, ce e, ce f ]

-- Compile Bloop top-level forms -------------------------------------
ct (Procedure v name argl s) = [S.List $ [ _defproc, defl, defs, body]]
  where defl = S.List (ce name:(map ce argl))
        -- Scheme insists that all variables are defined before they
        -- are assigned to, so we go gather all mutable variables and
        -- put them in explicitly.
        defs = S.List ( [S.List [_output, ce v]] ++
                      nub (map (\x -> S.List [x, _zero]) (_scells s)) )
        -- Need to wrap outer block, in case we quit from the outer
        -- block, so that the procedure still returns a value.
        body = cs (Block (-1) [s])
ct (TopExp e) = [ S.List [ _show, _string (prettyShow e) ],
                  S.List $ [ _show, ce e ] ]

-- Compile Bloop programs --------------------------------------------
compile (Program ts) = S.prettyShow $
                       S.Scheme $ 
                       concat (map ct ts)

