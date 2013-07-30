\name{rootLaddDist}
\alias{rootLaddDist}

\title{
Root Ladder Distance
}
\description{
Gives the length of the longest ladder starting from the root of the tree.
}
\usage{
rootLaddDist(tree)
}

\arguments{
\item{tree}{An object of class phylo4}
}
\details{
Used by ladderDist. Otherwise not intended to be used.
}
\value{
A numeric vector of size one
}
\author{
Michael Boyd
}
\seealso{
\code{\link{ladderDist}}
}
\examples{
## Creates a random tree of class phylo4 and plots it with nodes labelled by ID
tree <- rtree4(50)
tree <- idNodeLabel(tree)
plot(tree,show.tip.label=FALSE,show.node.label=TRUE)

## Finds the length of the ladder staring from the root
rootLaddDist(tree)
}
