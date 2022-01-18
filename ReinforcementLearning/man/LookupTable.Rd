\name{LookupTable}
\alias{LookupTable}
\title{LookupTable}
\description{
Gets the lookup table given environment and Agent
}
\usage{
LookupTable(Environment, Agent,digits=2)
}
\arguments{
\item{Agent}{R6 object of package reinforcelearn, Intelligent agent is
instantiation of RL algorithm that intelligently interacts with the environment}
\item{Environment}{R6 object of package reinforce learn, Environment denotes an
abstraction of the world that an intelligent agent operates.}
\item{digits}{(Optional. Default: digits=2). Integer for rounding.}
}
\value{
\item{Matrix}{[1:s,1:a] numeric matrix of Q values per state s and action a.}
}
\author{Michael Thrun}