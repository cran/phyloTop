\name{topFuncs}
\alias{topFuncs}
\alias{fWidths}
\alias{fAvgLadder}
\alias{fNLadders}
\alias{fColless}
\alias{fNConfig}

\title{
Functions from Bases to Topological Properties
}
\description{
A collection of functions which return topological properties given a base. These functions are intented to be used with those in the Fast Model Analysis section. Note that these functions have very different inputs.
}
\usage{
fWidths(nodeDists)
fAvgLadder(ladderNumbers)
fNLadders(ladderNumbers)
fColless(treeImbalance)
fNConfig(nTipDes,configSize)
}
\arguments{
\item{nodeDists}{
The result of running \code{dists} on a tree. That is, a vector organised by \code{nodeId} giving the distance of each node from the root.
}
\item{ladderNumbers}{
The result of running \code{ladderNums} on a tree. That is, a vector organised by \code{nodeId} giving the largest ladder size going away from the root starting from each node.
}
\item{treeImbalance}{
The result of running \code{treeImb} on a tree. See \code{\link{treeImb}} for more information.
}
\item{nTipDes}{
The result of running \code{nTipDescendants} on a tree. That is, a vector organised by \code{nodeId} giving the number of tip descendants of each node.
}
\item{configSize}{
An integer specifying the size of configuration to count.
}
}
\details{
These functions all correspond to other functions based on trees. For example, \code{fColless} corresponds to \code{colless}. These functions are much faster but take inputs which already require a reasonable amount of work to calculate. To learn more about \code{fColless} see the documentation for \code{colless}. 

For example, the intention is that when calculating many topological properties it may be faster to calculate \code{ladderNumbers} and \code{nodeDists} first and then run these functions. 
}
\value{
These functions all have the same value as their corresponding function.
}
\author{
Michael Boyd
}
\seealso{
\code{\link{fastModelAnalysis}} for more deatiled examples.
}
\examples{
## Sets up an example tree
tree <- rtree4(50)

## This returns the same ouput as avgLadder(tree)
ladderNumbers <- ladderNums(tree)
fAvgLadder(ladderNumbers)

## But now we can calculate the number of ladders more efficiently.
fNLadders
}

