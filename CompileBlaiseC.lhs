\documentclass{scrartcl}
%include lhs2TeX.fmt
%include lhs2TeX.sty
\title{The Compiler}
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
module CompileBlaiseC where
import qualified AstBlaise as B
import AstC
\end{code}

\section{Introduction} 

This compiler is an attempt to compile Blaise to relatively idiomatic
GNU C. It does {\sc \bf not} support function pointers.

\section{AST}
\begin{code}

{-
bork = error "Unimplemented."

cexp (B.LiteralInt i)           = Literal $ Lit_int (show i)
cexp (B.LiteralReal d)          = Literal $ Lit_float (show d)
cexp (B.LiteralChar c)          = Literal $ Lit_char (show c)
cexp (B.LiteralString s)        = Literal $ Lit_string (show s)
cexp (B.LiteralNil)             = Literal $ Lit_int "0"
cexp (B.DesignatorExpr d)       = cdes d
cexp (B.FunctionCall d el)      = Call (cdes d) (map cexp el)
cexp (B.Binop s e1 e2)          = Binary (cbin s) (cexp e1) (cexp e2)
cexp (B.Unop s e)               = Unary (cuna s) (cexp e)

cuna ("~")                      = Not
cuna ("-")                      = Minus
cuna ("+")                      = Plus
cbin ("*")                      = Mul
cbin ("/")                      = Div
cbin ("div")                    = Div -- !
cbin ("mod")                    = Mod
cbin ("&")                      = And
cbin ("+")                      = Add
cbin ("-")                      = Sub
cbin ("or")                     = Or
cbin ("=")                      = Eq
cbin ("#")                      = Ne
cbin ("<=")                     = Le
cbin ("<")                      = Lt
cbin (">=")                     = Ge
cbin (">")                      = Gt
cbin ("is")                     = bork

cdes (B.DesProject d (B.Id s))  = Memberof (cdes d) s
cdes (B.DesSubscript d xs)      = index d (reverse xs)
      where index d []          = cdes d
            index d (x:xs)      = Index (index d xs) (cexp x)
cdes (B.DesAssertTy _ _)        = bork
cdes (B.DesReference d)         = Unary Memof (cdes d)
cdes (B.DesVariable v)          = Variable (cqid v)

cqid (B.QId Nothing s)          = s
cqid (B.QId _ _)                = bork 
cgid (B.GId _ s)                = s -- ***

cblk sl                         = Block $ Body [] (map csta sl) 
cbdy sl                         = Body [] (map csta sl) 

csta (B.ProcCall d el)          = Computation (cexp (B.FunctionCall d el))
csta (B.If e s1 s2)             = If (cexp e) (cblk s1) (Just (cblk s2))
csta (B.Case _ _ _)             = bork
csta (B.While e s)              = While (cexp e) (cblk s)
csta (B.Repeat s e)             = Dowhile (Unary Not (cexp e)) (cblk s)
csta (B.Loop s)                 = While (Literal $ Lit_int "1") (cblk s)
csta (B.Exit)                   = Break -- ???
csta (B.Return e)               = Return (Just (cexp e))
csta (B.Assign d e)             = Computation expr 
     where expr                 = Binary Assign (cdes d) (cexp e)

cmod (B.Module n _ d s)         = File $ (map cdec d) ++ [initf s]
     where initf s              = DeclareFun Void (init n) [] (cbdy s)
           init (B.Id s)        =  "__init_" ++ s

cprg (B.Program ms)             = map cmod ms

ccns (B.GId _ s) t e = Initialize t (Name s) (SimpleInit (cexp e))

cdec (B.ConstantDec s e@(B.LiteralInt _))    = ccns s Int e
cdec (B.ConstantDec s e@(B.LiteralReal _))   = ccns s Double e
cdec (B.ConstantDec s e@(B.LiteralChar _))   = ccns s Char e
cdec (B.ConstantDec s e@(B.LiteralString _)) = ccns s (PointerTo Int) e
cdec (B.ConstantDec s e@(B.LiteralNil))    = ccns s Int e
cdec (B.TypeDec n t)            = Typedef (ctyp t) (cgid n)
cdec (B.VarDec  n t)            = Declare (ctyp t) (Name (cgid n))
cdec (B.ForwardDec (B.GId _ n) sig)       = ProtoFun (ret sig) n (argl sig)
  where ret  (B.Sig _ Nothing)  = Void
        ret  (B.Sig _ (Just q)) = ctyp (unformal (B.FTyId q))
        argl (B.Sig args _)     = map arg args
        arg ((B.Id id),B.ByValue,ty)= arg' id (unformal ty)
        arg ((B.Id id),B.ByReference,ty)= arg' id (B.TyPointer (unformal ty))
        unformal (B.FTyArray t) = B.TyPointer (unformal t)
        unformal (B.FTyId q)    = B.TyId q
        unformal (B.FTyProc sig)= B.TyProc sig
        arg' name ty@(B.TyId (B.QId _ q))  = Declare (ctyp ty) (Name q) -- ***
        arg' _ _ = bork

cdec (B.ProcDec n sig decl statl)= DeclareFun ty n args body 
  where body                    = Body (map cdec decl) (map csta statl)
        (ProtoFun ty n args)= cdec (B.ForwardDec n sig)


ctyp (B.TyId (B.QId Nothing "Integer")) = TypeName "int"
ctyp (B.TyId (B.QId Nothing "Real")) = TypeName "double"
ctyp (B.TyId (B.QId Nothing "Character")) = TypeName "char"
ctyp (B.TyId q) = TypeName (cqid q)
ctyp (B.TyArray _ t) = PointerTo (ctyp t) -- *** MAJOR KLUDGE *** ---
ctyp (B.TyRecord _) = bork
ctyp (B.TyPointer t) = PointerTo (ctyp t)
ctyp (B.TyProc _) = bork

-}

compile = error "Not implemented."

\end{code}
\end{document}
