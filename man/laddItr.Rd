\name{laddItr}
\alias{laddItr}

\title{
Ladder Iteration Tool
}
\description{
A function for internal use in relation to calculating the number of ladders in a tree.
}
\usage{
laddItr(ladderNumbers)
}

\arguments{
\item{ladderNumbers}{A numeric vector which is a result of calling \code{ladderNums} to a tree.}
}
\details{
This function reduces the maximum value of \code{ladderNumbers} by one. Say \emph{m} is the maximum number and there are \emph{n} of them. Then this function removes them and also \emph{n} of the natural numbers up to \emph{m} - essentially removing all the ladders of size \emph{m} from the tree.
}
\value{
Another numeric vector suitable for passing to laddItr.
}
\author{
Michael Boyd
}
\seealso{
\code{\link{nLadders}} and \code{\link{avgLadder}} which use this function.
}
\examples{
## Creates a tree
tree <- rtree4(50)

## Creates an appropriate vector
ladderNumbers <- ladderNums(tree)

## Perfoms the operation
laddItr(ladderNumbers)
}
