\name{FindStatesPathOfOptimalPolicy}
\alias{FindStatesPathOfOptimalPolicy}
\title{FindStatesPathOfOptimalPolicy}
\description{
Finds the optimal actions to be taken based on the qbalues stored in the table
for the case of a grid envirnment either with four actions or eight actions
(diagonal moves).
}
\usage{
FindStatesPathOfOptimalPolicy(Qtable,GridRows,GridCols,Start,Goal)
}
\arguments{
\item{Qtable}{[1:N, 1:D] Numeric matrix where column entries denote edges
for all four cardinal directions for a set of nodes written rowwise. The nodes
are aranged in a grid matrix (NxD) to create a network graph without negative
cycles!}
\item{GridRows}{row number of grid environment.}
\item{GridCols}{col number of grid environment}
\item{Start}{intial state of agent}
\item{Goal}{ terminal state of agent}
}
\value{
\item{plotOut}{Plotly object for direct visualization.}
}
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
}
\author{Quirin Stier, Michael Thrun}