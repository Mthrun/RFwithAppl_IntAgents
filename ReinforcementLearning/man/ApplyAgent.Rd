\name{ApplyAgent}
\alias{ApplyAgent}
\title{ApplyAgent}
\description{
Performs a benchmarking of trials per algorithms defined by Agent and newly
intialized Environment
}
\usage{
ApplyAgent(Agent,Environment,Trials=100,Episodes=100)
}
\arguments{
\item{Agent}{R6 object of package reinforcelearn, Intelligent agent is
instantiation of RL algorithm that intelligently interacts with the environment}
\item{Environment}{R6 object of package reinforce learn, Environment denotes an
abstraction of the world that an intelligent agent operates.}
\item{Trials}{(Optional. Default: Trials=100).}
\item{Episodes}{(Optional. Default: Episodes=100).}
}
\value{
List of
\item{ReturnsPerTrial}{Plotly object for direct visualization.}
\item{QstatesPerTrial}{Plotly object for direct visualization.}
}
\author{Michael Thrun}