\name{treeAnalysis}
\alias{treeAnalysis}
\alias{splitTop}
\alias{sackin}
\alias{widths}
\alias{avgLadder}
\alias{nLadders}
\alias{colless}
\alias{topSumm}

\title{
Tree Analysis Functions
}
\description{
A collection of functions to provide summary characteristics of phylogenetic trees. There are other functions which do this included in other documentation files (links below).
}
\usage{
splitTop(tree,dist)
sackin(tree)
widths(tree)
avgLadder(tree)
nLadders(tree)
colless(tree,normalize=TRUE)
topSumm(tree,topList)
}

\arguments{
\item{tree}{
An object of class \code{phylo4}
}
\item{dist}{
An integer specifying the distance at which to do the splitting
}
\item{normalize}{
A boolean specifying whether to normalize the Colless Imbalance
}
\item{topList}{
This is a list of functions. Each function in the list acts on an object of class \code{phylo4} and returns a number - which is intended to be a topological property of the tree. 
}
}
\details{
\code{splitTop} gives the number of tip children of each of the nodes at the given distance from the node. Note that, in \code{phylobase}, \emph{Depth} is how far the node is from the root taking edge length into account. I have stuck with this convention so \emph{Dist} is the number of steps required to go to that node from the root. Returns an error if there are no nodes of the given distance. Note that it has been ordered to make it a topological porperty; if there is no order then trees with the same topology can give different results.

\code{sackin} gives the sum of the distance of each of the tips from the root. This is a form of Sackin Imbalance. You may need to check that the definition you are using is the same as the one given here.

\code{widths} gives the number of nodes at each distance from the root. Note that the elements of the returned vector give the lenths of the \code{splitTop} vectors for each distance. It uses \code{dists}.

\code{avgLadder} gives the average length of all the ladders in the tree. Uses internal function \code{laddItr}.

\code{nLadders} gives the number of distict ladders in the tree. Also uses \code{laddItr}.

\code{colless} returns the normalised Colless Imblanace. That is the sum of all the node imbalances divided by \emph{2/((n-1)(n-2))} where \emph{n} is the number of tips. See \code{nodeImb} for the calculation of the imblalance for particular nodes. There is also an option to return the unnormalized Colless Imbalance.

\code{topSumm} returns the result of each of the functions in \code{topList} on the specified tree. Similar functions (mostly applied to many trees or a model for generating trees) can be found in \code{modelSummary}.
}

\value{
\item{splitTop}{
An integer vector of length equal to the number of nodes of the tree of the given distance - it is their number of tip children. Note that taking split topologies at every level is a very descriptive topological property and is useful for testing if trees with many nodes have the same topology.
}
\item{sackin}{
A numeric vector of length one giving the sum of the distance of each of the tips from the root.
}
\item{widths}{
An integer vector of length equal to the largest distnace of a tip to the root.
}
\item{avgLadder}{
A number.
}
\item{nLadders}{
A number.
}
\item{colless}{
A number.
}
}
\author{
Michael Boyd
}
\seealso{
\code{\link{modelSummary}} for functions to produce data frames containing the results of several of these functions on many trees.

\code{\link{allNodeAnalysis}} for more functions which give results about every node in the tree. 

\code{\link{configurations}} for functions examining configurations in a tree. A cherry is a type of congiguration (it is a 2-configuration).

\code{\link{ladderShow}} for a function which plots the tree highlighting the ladders.

\code{\link{nodeImb}}, \code{\link{dists}} and \code{\link{laddItr}}. 
}
\examples{
## Creates a random tree of class phylo4 and plots it with nodes labelled by ID
tree <- rtree4(50)
tree <- idNodeLabel(tree)
plot(tree,show.tip.label=FALSE,show.node.label=TRUE)

## Finds the split topology of the fourth level
splitTop(tree,4)

## Finds the Sackin Imbalance
sackin(tree)

## Finds the width topology of the tree
widths(tree)

## Finds the average ladder length
avgLadder(tree)

## Finds the number of distinct ladders in the tree
nLadders(tree)

## Finds the Colless Imbalance (normalized)
colless(tree)
}
