% Lexers.#ext
% This file was produced from Parser.lit

% ABRHLibs -- a personal library of Haskell modules
% Copyright (C) 2007, 2008,  Andrew Rock
% 
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

\module{Parser.Lexers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The \highlighttt{ABR.Parser.Lexers} module provides some
frequently used lexers for common syntatic elements. 

\begin{code}
module ABR.Parser.Lexers (
      spaceL, tabL, vertabL, formfeedL, newlineL, returnL,
      whitespaceL, dropWhite, stringL, cardinalL, fixedL,
      floatL, signedCardinalL, signedFixedL, signedFloatL
   ) where
\end{code}

\begin{code}
import Data.Char
\end{code}

\begin{code}
import ABR.Parser
\end{code}

%\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%
%
%Reviewed 2015-02-03. Passed {\tt hlint}.\\
%Reviewed 2014-05-30: Made all the {\tt -Wall}s go away.\\
%Reviewed 2013-11-22.\\
%Reviewed 2009-04-13: Split from {\tt ABR.Parser}.
   

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\submodule{Frequently used lexers} %%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{spaceL}, \highlighttt{tabL},
\highlighttt{newlineL}, \highlighttt{vertabL},
\highlighttt{formfeedL}, and \highlighttt{returnL}
recognize individual whitespace characters.

\InputEBNF{Parser}{space}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\InputEBNF{Parser}{tab}

\InputEBNF{Parser}{newline}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\InputEBNF{Parser}{vertab}

\InputEBNF{Parser}{formfeed}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\InputEBNF{Parser}{return}

\begin{code}
spaceL, tabL, newlineL, vertabL, formfeedL, returnL
   :: Lexer
spaceL    = literalL ' ';   tabL      = literalL '\t'
newlineL  = literalL '\n';  vertabL   = literalL '\v'
formfeedL = literalL '\f';  returnL   = literalL '\r'
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{whitespaceL} recognizes any
amount of whitespace, returning it with tag {\tt "~"}.

\InputEBNF{Parser}{whitespace}

\begin{code}
whitespaceL :: Lexer
whitespaceL = some (satisfyL isSpace "") &%> " "
\end{code}

\noindent \highlighttt{dropWhite}~$l$ modifies $l$ by
filtering out lexemes. with tag {\tt "~"}.

\begin{code}
dropWhite :: Lexer -> Lexer
dropWhite = tagFilter " "
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{stringL} recognizes strings delimited
by double quotes that may extend across many lines. Use two double
quotes for one, {\it \`{a} la} Pascal. 

\InputEBNF{Parser}{string}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
stringL :: Lexer
stringL = 
   literalL '"'
   <&&> (many (     tokenL "\"\"" 
                <|> satisfyL (/= '"') "") &%> "")
   <&&> literalL '"'
   %> "string"
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{cardinalL} recognizes a
cardinal number, a sequence of decimal digits.

\InputEBNF{Parser}{cardinal}

\begin{code}
cardinalL :: Lexer
cardinalL = some (satisfyL isDigit "digit") &%> "cardinal"
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{fixedL} recognizes an
unsigned fractional number with no exponent.

\InputEBNF{Parser}{fixed}

\begin{code}
fixedL :: Lexer
fixedL = 
   cardinalL
   <&&> soft (optional (
           literalL '.' 
           <&&> soft (optional cardinalL)
        ))
   %> "fixed"
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{floatL} recognizes an
unsigned floating point number.

\InputEBNF{Parser}{float}

\begin{code}
floatL :: Lexer
floatL = 
   fixedL
   <&&> soft (optional (
           (literalL 'e' <|> literalL 'E')
           <&&> soft (optional (
                   literalL '-' <|> literalL '+'
                ))
           <&&> nofail' "exponent expected." cardinalL
        ))
    %> "float"
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{signedCardinalL} recognizes a
signed whole number.

\InputEBNF{Parser}{signedCardinal}

\begin{code}
signedCardinalL :: Lexer
signedCardinalL = 
   soft (optional (literalL '-')) <&&> cardinalL
   %> "signedCardinal"
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{signedFixedL} recognizes a
signed fixed number.

\InputEBNF{Parser}{signedFixed}

\begin{code}
signedFixedL :: Lexer
signedFixedL = 
   soft (optional (literalL '-')) <&&> fixedL
   %> "signedFixed"
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\InputEBNF{Parser}{signedFloat}

\begin{code}
signedFloatL :: Lexer
signedFloatL = 
   soft (optional (literalL '-')) <&&> floatL
   %> "signedFloat"
\end{code}

