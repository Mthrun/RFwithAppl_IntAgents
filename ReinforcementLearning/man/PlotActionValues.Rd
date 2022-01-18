\name{PlotActionValues}
\alias{PlotActionValues}
\title{PlotActionValues}
\description{
Plots q table as pixelmatrix, expects 8 possible actions in the grid (i.e.,
diagonal movements).
}
\usage{
PlotActionValues(Environment,Agent,MarkGoal,main="",Normalize)
}
\arguments{
\item{Agent}{R6 object of package reinforcelearn, Intelligent agent is
instantiation of RL algorithm that intelligently interacts with the environment}
\item{Environment}{R6 object of package reinforce learn, Environment denotes an
abstraction of the world that an intelligent agent operates.}
\item{NumberOfStatesPerRow}{Number of states per row of grid}
\item{MarkGoal}{integer, marks state with NaN}
\item{main}{title of pixel matrix}
\item{Normalize}{sets a all values above quantile(Q,Normalize) to zero}
}
\value{
\item{Plot}{Plot of Pixelmatrix}
}
\author{Michael Thrun}