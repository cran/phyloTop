\name{nodeAnalysis}
\alias{nodeAnalysis}
\alias{nodeImb}
\alias{ladderDist}
\alias{nodeDist}

\title{
Node Analysis Functions
}
\description{
A collection of functions which analyse the properties of particular nodes of the tree.
}
\usage{
nodeImb(tree,node)
ladderDist(tree,node)
nodeDist(tree,node)
}

\arguments{
\item{tree}{
An object of class \code{phylo4}
}
\item{node}{
An integer specifying a node in a tree by it's nodeId
}
}
\details{
\code{nodeImb} (stands for node imbalance) returns the number of tip descendants below the node on the left side and the right as a vector of size 2. \code{treeImb} calculates this for every node.

\code{ladderDist} (ladder distance) gives the largest ladder size (away from the root) starting from the given node. It is based on \code{rootLaddDist}. Note that ladder size is given by the number of edges it is made up of.

\code{nodeDist} gives the number of steps from the root to the given node. In \code{phylobase}, \emph{Depth} is how far the node if from the root taking edge length into account. I have stuck with this convention so \emph{Dist} is the number of steps required.
}
\value{
\item{nodeImb}{
A numeric vector of size 2 (with integer values) giving the size (by number of nodes) of the trees descending from the node
}
\item{ladderDist}{
A number - the length of the ladder from that node
}
\item{nodeDist}{
A number - the number of steps required to get from that node to the root
}
}
\author{
Michael Boyd
}
\seealso{
\code{\link{treeImb}}, \code{\link{rootLaddDist}} and \code{\link[phylobase]{nodeDepth}}
}
\examples{
## Creates a random tree of class phylo4 and plots it with nodes labelled by ID
tree <- rtree4(50)
tree <- idNodeLabel(tree)
plot(tree,show.tip.label=FALSE,show.node.label=TRUE)

## Calculate the imbalance of a node
nodeImb(tree,60)

## Calculates the length of the ladder from that node
ladderDist(tree,60)

## Finds the number of steps required to get from that node to the root
nodeDist(tree,60)
}
