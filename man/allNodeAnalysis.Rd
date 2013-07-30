\name{allNodeAnalysis}
\alias{allNodeAnalysis}
\alias{nDescendants}
\alias{nTipDescendants}
\alias{dists}
\alias{ladderNums}
\alias{treeImb}

\title{
All Node Analysis
}
\description{
A collection of functions designed to examine all of the nodes in a tree at once. Note that not all of these functions return tolological properties of the trees - two trees could be topologically identical but give different results. Many of the functions below gives results organized by \code{nodeId}.
}
\usage{
nDescendants(tree)
nTipDescendants(tree)
dists(tree)
ladderNums(tree)
treeImb(tree)
}

\arguments{
\item{tree}{
An object of class \code{phylo4}
}
}
\details{
\code{nDescendants} gives the number of descendants (including internal nodes) of all the nodes in the tree. It is organized by \code{nodeId}. Note that when \code{phylobase}'s \code{descendants} is given a tip it returns that tip as an output.

\code{nTipDescendants} gives the number of tip descendants of all the nodes in the tree. It is organized by \code{nodeId}.

\code{dists} gives the distance of each node from the root of the tree. It is organized by \code{nodeId}. Note that, in \code{phylobase}, \emph{Depth} is how far the node is from the root taking edge length into account. I have stuck with this convention so \emph{Dist} is the number of steps required to go to that node from the root.

\code{ladderNums} gives the largest ladder size (away from the root) starting from the given node. It is based on \code{rootLaddDist}. Note that it returns a vector of length equal to the number of internal nodes of the tree. Organized by \code{nodeId}. \code{laddItr} is intended to be applied to this result of this function.

\code{treeImb} gives the node imbalance of every internal node in the tree using \code{nodeImb}. The output ordered by \code{nodeId}.
}
\value{
\item{nDescendants}{
An integer vector of length equal to the number of nodes in the tree.
}
\item{nTipDescendants}{
An numeric vector of length equal to the number of nodes in the tree. All entries are integers.
}
\item{ladderNums}{
A numeric vector of length equal to the number of internal nodes of the tree. All entries are integers.
}
\item{treeImb}{
A matrix with two rows and number of columns equal to the number of internal nodes of the tree.
}
}
\author{
Michael Boyd
}
\seealso{
\code{\link{nodeId}}, \code{\link{rootLaddDist}}.
}
\examples{
## Creates a random tree of class phylo4
tree <- rtree4(50)

## Label the nodes of a tree by their nodeId
labelledTree <- idNodeLabel(tree)
plot(labelledTree,show.tip.label=FALSE,show.node.label=TRUE)

## Finds the number of descendants of the nodes
nDescendants(tree)

## Finds the number of tip descendants of the nodes in the tree
nTipDescendants(tree)

## Finds the distance of each node from the root of the tree
dists(tree)

## Finds the length of the internal ladders in the tree
ladderNums(tree)

## Calculates the imbalance of each of the internal nodes
treeImb(tree)
}
