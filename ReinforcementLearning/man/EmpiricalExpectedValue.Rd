\name{EmpiricalExpectedValue}
\alias{EmpiricalExpectedValue}
\title{EmpiricalExpectedValue}
\description{
EmpiricalExpectedValue
}
\usage{
EmpiricalExpectedValue(Variable)
}
\arguments{
\item{Variable}{[1:n]: Zufallsvariable X mit n eintraegen.}
}
\value{
\item{EW}{Numeric indicating scalar product of Kernels and Probs.}
\item{Kernels}{[1:m] Numeric vector with m unique values of given Variable}
\item{Probs}{[1:m] Numeric vector with probability of being in each value.}
}
\author{Quirin Stier}