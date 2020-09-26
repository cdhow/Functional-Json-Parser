% Pos.#ext
% This file was produced from Pos.lit

% ABRHLibs -- a personal library of Haskell modules
% Copyright (C) 2007, ... 2102  Andrew Rock
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

\module{Util.Pos} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Util.Pos} defines a type for a 
position in a source code. 

\begin{code}
module ABR.Util.Pos (
      Line, Col, Pos, HasPos(..), precedes
   ) where
\end{code}

%\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%
%
%Reviewed 2015-02-02. Passed {\tt hlint}.\\
%Reviewed 2014-05-29: Made all the {\tt -Wall}s go away.\\
%Reviewed 2013-11-06.\\
%Reviewed 2012-11-23: Moved to {\tt ABR.Util} from {\tt ABR.Parser}.\\
%Reviewed 2009-04-13: Split from {\tt ABR.Parser}.
   
\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\submodule{Positions in a source} %%%%%%%%%%%%%%%%%%%%%%%%

To report error the position, \highlighttt{Pos}, of
a character or token in a source is required. The
first line and column are indicated with
\highlighttt{Line} and \highlighttt{Col} values of
0. A negative {\tt Line} value indicates ``Don't
know where''.

\begin{code}
type Line = Int
type Col  = Int
type Pos  = (Line, Col)
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\submodule{Overloaded projector} %%%%%%%%%%%%%%%%%%%%%%%%

\noindent Positions get embedded in all kinds of
parse tree types. Having one overloaded function that projects
out a {\tt Pos} is useful. Make parse tree types with
postions in them an instance of \highlighttt{HasPos}.

\begin{code}
class HasPos a where
\end{code}

\noindent \highlighttt{getPos}~$x$ returns the position
of $x$. 

\begin{code}
   getPos :: a -> Pos
   getPos = error "undefined HasPos instance"
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Container intances}

\begin{code}
instance (HasPos a, HasPos b) => HasPos (Either a b) where
   getPos e = case e of
      Left x  -> getPos x
      Right x -> getPos x
\end{code}

\SPAGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\submodule{Relative positions} %%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent $p_1$~\highlighttt{precedes}~$p_2$ if $p_1$ comes
earlier in than $p_2$.

\begin{code}
precedes :: Pos -> Pos -> Bool
(l1,c1) `precedes` (l2,c2) = 
   l1 < l2 || l1 == l2 && c1 < c2
\end{code}

