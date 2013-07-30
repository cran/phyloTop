\name{utility}
\alias{utility}
\alias{rtree4}
\alias{idNodeLabel}
\alias{nTipChildren}

\title{
Utility Functions
}
\description{
A collection of short, useful functions to save time and allow more intuitive use of the package.
}
\usage{
rtree4(n)
idNodeLabel(tree)
nTipChildren(tree,node)
}
\arguments{
\item{n}{
An integer
}
\item{tree}{
An object of class \code{phylo4}
}
\item{node}{
An integer specifying a node in a tree by it's nodeId
}
}
\details{
\code{rtree4} uses \code{ape} function \code{rtree} and the \code{as} function of \code{phylobase} to produce a random tree of class \code{phylo4} with a given number of tips. The algorithm used randomly splits one of the edges at each step.

\code{idNodeLabel} labels the nodes of the tree by nodeId. Note that this includes the tips.

\code{nTipChildren} gives the number of children of a node which are tips.
}

\value{
\item{rtree4}{
An object of class \code{phylo4} with \code{n} tips.
}
\item{idNodeLabel}{
An object of class \code{phylo4} with nodes labelled by \code{nodeId}.
}
\item{nTipChildren}{
An integer specifying the exact number of children of that node - note \emph{children} not descendants.
}
}
\author{
Michael Boyd
}
\seealso{
\code{\link[ape]{rtree}} and \code{\link[phylobase]{nodeId}}
}
\examples{
## Creates a random tree of class phylo4
tree <- rtree4(50)

## Label the nodes of a tree by their nodeId
labelledTree <- idNodeLabel(tree)
plot(labelledTree,show.tip.label=FALSE,show.node.label=TRUE)

## Find the number of tip hildren of a node
nTipChildren(tree,70)
}
