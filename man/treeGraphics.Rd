\name{treeGraphics}
\alias{treeGraphics}
\alias{ladderShow}
\alias{configShow}
\alias{subtreeShow}

\title{
Tree Graphics
}
\description{
A collection of functions to display features of trees graphically. 

Note that simply typing (for example) \code{ladderShow} will show the code for that function - the code can be edited to display different features or add more options.
}
\usage{
ladderShow(tree)
configShow(tree,configSize)
subtreeShow(tree,nodeList)
}

\arguments{
\item{tree}{
An object of class \code{phylo4}.
}
\item{configSize}{
An integer specifying the size of configuration for which to highlight.
}
\item{nodeList}{
A numeric vector specifying a subset of the nodes of the tree by their \code{nodeId}.
}
}
\details{
\code{ladderShow} prints the tree with the ladders displayed in red.

\code{configShow} prints the tree with the configurations of the required size displayed in red.

\code{subtreeShow} prints the tree with the descendants of the nodes in \code{nodeList} highlighted. It is used in \code{configShow} and may be useful if you wish to write similar functions to highlight features of interest.
}

\value{
These functions have no output - they simply print the tree.
}

\author{
Michael Boyd
}
\seealso{
\code{\link{configurations}}
}
\examples{
## Creates a random tree of class phylo4 and plots it with nodes labelled by ID
tree <- rtree4(50)
tree <- idNodeLabel(tree)
plot(tree,show.tip.label=FALSE,show.node.label=TRUE)

## Plots the tree showing the ladders
ladderShow(tree)

## Plots the tree showing the 3-configurations
configShow(tree,3)

## Highlights the descendents of two particular nodes
subtreeShow(tree,c(60,90))
}