\name{configurations}
\alias{configurations}
\alias{nConfig}
\alias{configFind}
\alias{configTop}
\alias{classifyConfigFour}
\alias{classifyConfigFive}

\title{
Configurations in Trees
}

\description{
A collection of functions for examining configurations in trees. A configuration of size \emph{n} is a subtree with \emph{n} tips. So a cherry (for example) is a 2-configuration. Note that not all of these functions return tolological properties of the trees. For example, \code{configFind} can give very different results for two trees with the same topology.
}

\usage{
nConfig(tree,configSize)
configFind(tree,configSize)
configTop(tree)
classifyConfigFour(tree)
classifyConfigFive(tree)
}

\arguments{
\item{tree}{
An object of class \code{phylo4}
}
\item{configSize}{
An integer specifying the size of configuration for which to search
}
}

\details{
\code{nConfig} (number of configurations) finds the number of configurations of the specified size in the tree. This function (and many of those listed below) use \code{nTipDescendants}.

\code{configFind} returns a vector with each element being the \code{nodeId} of the root of a configuration of the specified size. That is, it gives all the nodes which have the specified number of tip descendants.

\code{configTop} returns a vector giving the number of \emph{n}-congigurations for each n for which one could exist. If there are \emph{t} tips then there is always one configuration of size \emph{t} (the whole tree) and \emph{t} configurations of size one (the tips). So the vector is length \emph{t}. Note that this is a topological property and is very useful for distinguishing trees with many tips.

\code{classifyConfigFour} gives the number of each of the different types of configurations of size four. There are two such types one called \emph{laddType} and the other \emph{branchType}. There are many ways in which they are topologically different - this function uses the number of tip children of the root. \emph{laddType} has only one while the other has none.

\code{classifyConfigFour} works similarly but except it looks at configurations size 5. Here there are three types. It first seperates by number of tip children of the root (one has none and is called \emph{branchType}) and then by size of ladder from the root (the type with a longer ladder is \emph{laddType} and the other is \emph{fourType} as there is a 4-1 split at the top).
}
\value{
\item{nConfig}{
An integer - the number of configurations of the specified size in the tree.
}
\item{configSize}{
An integer vector of length given by the number of configuratios of the specified size.
}
\item{configTop}{
An integer vector of length given by the number of tips of the tree.
}
\item{classifyConfigFour}{
A data frame with one row and two columns with headers specifying which type is which. 
}
\item{classifyConfigFive}{
A data frame with one row and three columns with headers specifying which type is which. 
}
}
\author{
Michael Boyd
}
\seealso{

\code{\link{configShow}} for a function which plots the tree highlighting configurations of a given size.

\code{\link{treeAnalysis}} for more functions examining topological properties of trees.

\code{\link{nTipDescendants}}.
}
\examples{
## Creates a random tree of class phylo4 and plots it with nodes labelled by ID
tree <- rtree4(50)
tree <- idNodeLabel(tree)
plot(tree,show.tip.label=FALSE,show.node.label=TRUE)

## Finds the number of cherries (also called 2-configurations) in the tree
nConfig(tree,2)

## Gives you the nodeId of the roots of the cherries
configFind(tree,2)

## Finds the complete configuration profile of the tree
configTop(tree)

## Classifies the configurations of size four...
classifyConfigFour(tree)

##...and the configurations of size five
classifyConfigFive(tree)
}
