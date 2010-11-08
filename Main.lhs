\documentclass{scrartcl}
\usepackage{amssymb}
%include lhs2TeX.fmt
%include lhs2TeX.sty
\title{Parse the command line}
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

\section{The main module}
\begin{code}
module Main where
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import System.IO
import System.IO.Error ( try )
import System.Environment ( getArgs )
import System.Posix ( unionFileModes, ownerExecuteMode,
                      setFileMode, fileMode, getFileStatus )

import qualified CompileBloopScheme as BloopScheme (compile)
import qualified CompileBloopC      as BloopC      (compile)
import qualified CompileBlaiseC     as BlaiseC     (compile)

import qualified AstScheme (prettyShow)
import qualified AstC      (prettyShow)
import qualified AstBloop  (prettyShow)
import qualified AstBlaise  (prettyShow)

import ParseBloop ( parseBloop )
\end{code}

\section{Drive the compiler}
\begin{code}
main = do args <- getArgs
          argv <- parseOptions args
          runCompiler argv
\end{code}

\section{Metadata about bloop}
\begin{code}
bloopVersion :: String
bloopVersion = 
  "This is bloop 0.2\n\n" ++
  "Copyright (C) 2005 Jaap Weel\n" ++
  "This is free software; see the source for copying conditions.  There is NO\n" ++
  "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."

libPath :: String
libPath = "/usr/local/lib/bloop/"
\end{code}

\section{Parse command line arguments}
Much of this code is snarfed from the System.Console.GetOpt
documentation in GHC. Unfortunately, the fact that the i and o
arguments are required is not encoded in the type, so we have to later
again check for that.

These are the possible arguments that a the program accepts:
\begin{code}
data Flag  = Help  | Version | Scheme | C | IR | Input String | Output String
             deriving (Show,Eq)

options :: [ OptDescr Flag]
options =  [ Option ['h']  ["help"]     (NoArg Help)         "this message"
           , Option ['v']  ["version"]  (NoArg Version)      "show version"
           , Option ['s']  ["scheme"]   (NoArg Scheme)       "output Scheme"
           , Option ['c']  ["c"]        (NoArg C)            "output \"C\""
           , Option ['i']  ["ir"]       (NoArg IR)           "output IR"
           , Option ['b']  ["bloop"]    (NoArg IR)           "input BlooP/FlooP"
           , Option ['B']  ["blaise"]   (NoArg IR)           "input Blaise"
           , Option ['o']  ["output"]   (ReqArg Output "FILE") "output FILE"
           , Option ['f']  []           (ReqArg Input  "FILE") "input FILE" ]
\end{code}

This is how to parse them:
\begin{code}
parseOptions :: [String] -> IO [Flag]
parseOptions argv = 
   case getOpt Permute options argv of
      (o,[],[])        -> return o
      (_,unknown,err)  -> hPutStrLn stderr ("Don't understand: " ++
                                            concat unknown ++ 
                                            concat err) 
                          >> return []
\end{code}

\section{Run the compiler}
\begin{code}
data Target = SchemeTarget | CTarget | IRTarget

runCompiler :: [Flag] -> IO ()
runCompiler flags | elem Help flags     =  hPutStrLn stderr $ 
                                           usageInfo bloopVersion options
runCompiler flags | elem Version flags  =  hPutStrLn stderr $ 
                                           bloopVersion 
runCompiler flags | elem Scheme flags   =  compileTo SchemeTarget flags
runCompiler flags | elem C flags        =  compileTo CTarget flags
runCompiler flags | elem IR flags       =  compileTo IRTarget flags
runCompiler _                           =  runCompiler [Help]

compileTo :: Target -> [Flag] -> IO ()
compileTo target [Output outf,Input inf] = 
  do  bloopProgram  <- readFile inf
      outH <- openFile outf WriteMode
      case parseBloop inf bloopProgram of
       Left err   -> ioError $ userError $ show err
       Right ast  -> compile target ast >>= hPutStr outH
         where compile SchemeTarget ast =
                 do  prelude <- getInstalledFile libPath "prelude.scm"
                     return (prelude ++ BloopScheme.compile ast)
               compile CTarget ast =
                 do  prelude <- getInstalledFile libPath "prelude.h"
--                     return (show $ C.compile ast)
                     return (prelude ++ (AstC.prettyShow $ BloopC.compile ast))
               compile IRTarget ast = return $ AstBloop.prettyShow ast
      hClose outH
      case target of 
       SchemeTarget -> do mode <- getFileStatus outf
                          let executable = (unionFileModes (fileMode mode)
                                            ownerExecuteMode) in
                              setFileMode outf executable 
       _ -> return ()

compileTo target [] = ioError $ userError "Internal compiler error."
compileTo target (_:xs) = compileTo target xs
\end{code}

Read library files from an obvious location:
\begin{code}
getInstalledFile :: String -> String -> IO String
getInstalledFile path fn = do s <- try (readFile fn)
                              case s of
                               Left _ -> readFile (path ++ fn)
                               Right a -> return a
\end{code}

\end{document}