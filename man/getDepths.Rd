% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDepths.R
\name{getDepths}
\alias{getDepths}
\title{Find the depth of each node}
\usage{
getDepths(tree)
}
\arguments{
\item{tree}{a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.}
}
\value{
A list of two vectors: \code{tipDepths} gives the depths of the tips, and \code{nodeDepths} gives the depths of the internal nodes.
}
\description{
Determines the depth of each node, defined as the number of steps from the root.
(So the root has depth zero, its children have depth one, etc.)
The output is given as a list of two vectors: \code{tipDepths} gives the depths of the tips, and \code{nodeDepths} gives the depths of the internal nodes.
This replaces the deprecated \code{dists} function.
The code is based on that used in the function \code{computeHeights} in package \code{treeCentrality}.
}
\examples{
## Find the node depths in a random tree:
tree <- rtree(20)
treeDepths <- getDepths(tree)
## Plot tree with node depths labelled:
tree$tip.label <- treeDepths$tipDepths
tree$node.label <- treeDepths$nodeDepths
plot(tree, show.node.label=TRUE)


}
\seealso{
\code{\link{nodeDepth}}, \code{\link{nodeDepthFrac}}
}
\author{
Michelle Kendall \email{michelle.louise.kendall@gmail.com}
}
