\name{fastModelAnalysis}
\alias{fastModelAnalysis}
\alias{baseCreate}
\alias{baseListCreate}
\alias{baseAnalysis}
\alias{baseListAnalysis}

\title{
Fast Model Analysis
}
\description{
A collection of functions which calculate properties of trees more quickly. These functions do a similar job to those in \code{modelAnalysis} but they may be slightly more difficult to use and adapt. 

Many functions in this package share preliminary calculations. This means that in using \code{topSumm}, for example, the same code is run several times. The idea behind these functions is to improve the speed by doing these calculations just once. These preliminary computations result in what I call a \emph{base}. This will mostly be done with \code{baseListCreate} and is often most of the work in the calculation of a topology profile. Then, the final topologies of the given trees are created using \code{baseListAnalysis}. I refer to functions which create a base from a tree as \emph{baseFuncs} and those which create a topology from a base as \emph{topFuncs}.
}
\usage{
baseCreate(tree,baseFuncs)
baseListCreate(treeList,baseFuncs,loadingBar = TRUE)
baseAnalysis(base,topFuncs)
baseListAnalysis(baseList,topFuncs,loadingBar = FALSE)
}
\arguments{
\item{tree}{
An object of class \code{phylo4}.
}
\item{baseFuncs}{
A list of functions which each take a phylogenetic tree as an input and give any output.
}
\item{treeList}{
A list where the elements of the list are cobjects of class \code{phylo4}.
}
\item{loadingBar}{
A Boolean. If true, it results in the number of elements evaluated so far being printed as each is evaluated. This gives some indication of how long the function will take.
}
\item{base}{
A list. Each element of the list is intented to be the result of an element of \code{BaseFuncs} on a phylogenetic tree.
}
\item{baseList}{
A list where each element is a base. This results in a nested list.
}
\item{topFuncs}{
A list of functions which ecah takes an element ofthe base and returns a number.
}
}
\details{
\code{baseCreate} creates the base for a tree. That is, it runs a number of functions (contaied in list \code{baseFuncs}) on the tree and stores the result in a list.

\code{baseListCreate} does the same as \code{baseCreate} but for a list of trees (called \code{treeList}). The result is therefore a list of bases. This is quite a complicated object. It is a list of the same length as \code{treeList}. Each element of this list is a base corresponding to a tree in \code{treeList} - that is a list of the same length as \code{baseFuncs}. \code{loadingBar} defaults to \code{TRUE} as this function may take a long time 

\code{baseAnalysis} analyses a given base using functions (contained in \code{topFuncs}) which act on each member of the base. That is, the first element of \code{topFuncs} will take the fist element of the base and return a number. The list of all these numbers is a topological profile of the tree. This list is what \code{baseAnalysis} returns.

\code{baseListAnalysis} does the same as \code{baseList} but to each element of a list of bases. The result is a dataframe where each row corresponds to a different base and each column to a different topological property.
}
\value{
\item{baseCreate}{
A base. That is a list where each element is a result of a function on a tree. The length of the list will be the same as the length as \code{baseFuncs}
}
\item{baseListCreate}{
A list where each element is the base (for the given \code{baseFuncs}) for an element of \code{treeList}. So it's length will be the same as that of \code{treeList}
}
\item{baseAnalysis}{
A matrix with one row and number of columns equal to the length of \code{topFuncs}.
}
\item{baseListAnalysis}{
A dataframe where each row corresponds to a different base and each column to a different topological property. So the number of columns is equal to the length of \code{topFunc} and the number of rows is equal to the length of \code{baseList}.
}
}
\author{
Michael Boyd
}
\seealso{
\code{\link{modelAnalysis}} contains simpler but slower functions with similar effects. There are also a number of ways to create \code{treeList} in this section.

See \code{\link{allNodeAnalysis}},\code{\link{configurations}} and \code{\link{treeAnalysis}} for some possibilities for what to put in \code{baseFuncs}.

\code{\link{topFuncs}} conatins a number of useful options for putting in \code{topFuncs}
}
\examples{
## This is an example of using bases to speed up calculation of topological properties.

## Creates a tree
tree <- rtree4(50)

## Sets up the list of bases 
baseFuncs <- c(ladderNums, treeImb, nTipDescendants)

## Creates the base
base <- baseCreate(tree,baseFuncs)

## Sets up the list of functions from the base
## Note that the functions are matched with the correct base element
topFuncs <- c(fNLadders,fColless,function(x) {fNConfig(x,2)})

## Finds the topological properties
baseAnalysis(base,topFuncs)

\dontrun{
## Now a new list of topFuncs
## Some of these use the same elements of the base
## This means that there will be a speed improvement
topFuncs2 <- c(topFuncs, function(x) {fNConfig(x,3)},fAvgLadder)

## Now we must create an appropriate base for these topFuncs
## Note that the elements are matched to those of topFuncs
base2 <- c(base,base[3],base[1])

## And the new topological properties
baseAnalysis(base2,topFuncs2)
}
}
