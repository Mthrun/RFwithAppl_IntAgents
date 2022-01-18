\name{PlotStatesAndQstarPath}
\alias{PlotStatesAndQstarPath}

\title{PlotStatesAndQstarPath}
\description{
Plot grid states and path of optimal policy based on Q star.
}
\usage{
PlotStatesAndQstarPath = function(StatesMatrix, Traversedstates, OptimalPolicy,
                             StateColor = "black", TextColor = "white",
                             IdxNodes = NULL, NodeColors = NULL,
                             LineColor = "black", LineWidth = 2,
                             MarkerSize = 37, TextSize = 14)
}
\arguments{
\item{StatesMatrix}{[1:N, 1:M] Numeric matrix with enumeration of nodes which are
aranged in a matrix grid topology network.}
\item{Traversedstates}{[1:X] Numeric vector with the order of nodes making up a
shortest path from the first to last node.}
\item{OptimalPolicy}{[1:X-1] Numeric vector with numbers between 1 and 4 indicating
the directions to go in the matrix grid topology network in order to get from
the first node in parameter SP_NodeOrder to the last one in it.}
\item{IdxNodes}{[1:y] Numeric Vector with node numbers to colorize.}
\item{NodeColors}{[1:y] Numeric Vector with node colors. If IdxNodes given but no
NodeColors, then gray will be used as default.}
\item{StateColor}{(Optional. Default: StateColor="black"). Character indicating
the default color of states.}
\item{TextColor}{(Optional. Default: TextColor="white"). Character indicating
the text color.}
\item{IdxNodes}{(Optional. Default: IdxNodes=NULL). [1:n] Numeric vector
indicating
Nodes for specific coloring.}
\item{NodeColors}{[1:n] (Optional. Default: NodeColors=NULL). Character vector
with colors for each given node in IdxNodes. If NodeColors is NULL, then color
gray will be used as default.}
\item{LineColor}{(Optional. Default: LineColor="black"). Character indicating
the color of lines around the node markers.}
\item{LineWidth}{(Optional. Default: LineWidth=2). Numeric indicating the widht
of the lines.}
\item{MarkerSize}{(Optional. Default: MarkerSize=37). Numeric indicating the
size of the nodes in the plot.}
\item{TextSize}{(Optional. Default: TextSize=14). Numeric indicating the text
used within the nodes.}
}
\value{
\item{plotOut}{Plotly object for direct visualization.}
}
\author{Quirin Stier}

\examples{
Qtable=matrix(-10,4,4)
#grid env,4actions
colnames(Qtable)=c("left","right","up","down")
rownames(Qtable)=c("State0","State1","State2","State3")
#set wall, no pacman
Qtable[1,2]=-9999
#set optimal policy
Qtable[1,4]=-1
Qtable[3,2]=-1
Qtable[4,3]=-1
V=FindStatesPathOfOptimalPolicy(Qtable = Qtable,GridRows = 2,GridCols = 2,Start = 0,Goal = 1)
PlotStatesAndQstarPath(V$StatesMatrix, V$Traversedstates, V$OptimalPolicy)
}