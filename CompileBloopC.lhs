\documentclass{scrartcl}
%include lhs2TeX.fmt
%include lhs2TeX.sty
\title{Bloop/Floop to ``C'' Compiler}
\author{Jaap Weel}
\setlength{\parindent}{0pt}
\setlength{\parskip}{1ex plus 0.5ex minus 0.2ex} 
\begin{document}
\maketitle

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
module CompileBloopC ( compile ) where 
import Data.List ( nub )
import qualified AstBloop as B
import AstC
import OptimizeC ( optimize )
import AnalyzeBloop ( scells, Cell(..) )
\end{code}

\section{Low-level syntax munging} Booleans are represented by 0 and
1. Cells and the {\tt OUTPUT} special variable are represented by
special generated tokens, which contains underscores, which are
illegal in BlooP. Really, this is a kludge and could more properly be
implemented if there were a symbol table, but it works just fine. 

\begin{code}
_output             = "output"          -- OUTPUT
_quit i             = "quit" ++ show i  -- QUIT i
_abort i            = "abort" ++ show i -- ABORT i
_ctr i              = "ctr" ++ show i   -- Loop counter.
_cell i             = "cell" ++ show i  -- CELL(i)
_zero               = (Literal (Lit_int "0")) -- 0
_false              = (Literal (Lit_int "0")) -- NO
_true               = (Literal (Lit_int "1")) -- YES
_scell (C i)        = _cell i
\end{code}

The {\tt mung} function serves to convert BlooP identifiers into legal
C identifiers.
\begin{code}
_mung '-'           = '_'                -- Make a legal C identifier
_mung '?'           = 'p'                --  out of a legal Bloop identifier.
_mung x             = x                  -- 
\end{code}

\section{Expressions}
Most BlooP/FlooP expressions map quite nicely onto C, there's not much
here.
\begin{code}
csym :: B.Symbol -> String
csym (B.Symbol s) = map _mung s

ce :: B.Exp -> Expression
ce (B.Number i)          = Literal (Lit_int (show i))
ce (B.Var s)             = (Variable $ csym s)
ce (B.Boolean True)      = _true
ce (B.Boolean False)     = _false
ce (B.Cell i)            = Variable $ _cell i
ce (B.Output)            = Variable _output
ce (B.Binop o e1 e2)     = Binary (cop o) (ce e1) (ce e2)
ce (B.Funcall s es)      = Call (Variable $ csym s) (map ce es)
cop "*"    = Mul
cop "+"    = Add
cop "="    = Eq
cop "<"    = Lt
cop ">"    = Gt
cop "<="   = Le
cop ">="   = Ge
cop "AND"  = And
cop "OR"   = Or
\end{code}

\section{Statements}

The escaping BlooP/FlooP statements {\tt abort} and {\tt quit} are
translated as gotos. Bounded loops become for loops. The rest maps
naturally.

\begin{code}
cs :: B.Statement -> [ Statement ]
cs (B.Quit i)      = [ Goto $ GotoTarget (_quit i)]
cs (B.Abort i)     = [ Goto $ GotoTarget (_abort i)]
cs (B.Block i ss)  = [ Block (Body [] (concat $ map cs ss))] ++ 
                     [ Label $ LabelName (_quit i) ]
cs (B.Muloop i s)  = [ While _true (Block $ Body [] 
                                    (cs s ++
                                     [ Label $ LabelName (_abort i) ])),
                       Label $ LabelName (_abort i) ]
cs (B.Loop e i s)  = [ (Block $ Body 
                        [ Declare Int (Name $ _ctr i) ]
                        [ For (Binary Assign (Variable (_ctr i)) (ce e))
                              (Binary Gt (Variable (_ctr i)) _zero)
                              (Unary Posdecr (Variable (_ctr i))) 
                              (Block (Body [] (cs s)))])
                     , Label $ LabelName (_abort i) ]
cs (B.Ifthen e s)  = [ If (ce e) (Block $ Body [] (cs s)) Nothing ]
cs (B.Assign e f)  = [ Computation (Binary Assign (ce e) (ce f)) ]
\end{code}

\section{Procedures}

BlooP procedures become C functions. We use the {\tt scells} function
from AnalyzeBloop to find the local variables we have to generate
declarations for, much like in the BlooP to Scheme compiler.

\begin{code}
cp (B.Procedure v (B.Var name) argl s) = 
  [DeclareFun Int (csym name) args (Body decls body)]
  where -- C insists that all variables are defined before they are
        -- assigned to, so we go gather all mutable variables and put
        -- them in explicitly.
        decls    = [ Initialize Int (Name "output") 
                                (SimpleInit _zero) ] ++
                    (map (\x -> 
                            Initialize Int x (SimpleInit _zero)) 
                     (map (Name . _scell) $ nub $ scells s))
        body     = cs (s) ++ [ Return $ Just $ Variable _output ]
        cells s  = map (\(C i) -> _cell i) (scells s)
        args     = map ((Declare Int) . Name .  (\(B.Var s) -> csym s)) argl
        mung (B.Var (B.Symbol s)) = map _mung s
cp _ = []
\end{code}

\section{Initialization forms}
Initialization forms get gathered into a C {\tt main} function. This
slightly alters their semantics, since they will all execute in an
environment where all the procedures are defined.
\begin{code}
ci (B.TopExp e) = [ Computation $ Call (Variable "printf") 
                    [ Literal (Lit_string "%s\n")
                    , Literal (Lit_string $ B.prettyShow e)
                    ]
                  , Computation $ Call (Variable "printf") 
                    [ Literal (Lit_string "%d\n"), (ce e) ]
                  ]
ci _ = []
\end{code}

\section{Entire program}

Now we can compile entire BlooP/FlooP programs by just concatenating
the compiled procedures and adding a {\tt main} function. We call
optimize on it to at least eliminate some dead code.

\begin{code}
compile (B.Program ts) =  optimize $
                          File $
                          (concat $ map cp ts) ++
                          [DeclareFun Int "main" [] 
                           (Body [] $
                            (concat $ map ci ts) ++
                            [ Return $ Just $ Literal $ Lit_int "0" ])]
\end{code}
\end{document}

