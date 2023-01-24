#' Fraction of nodes beyond a given depth
#' 
#' Calculate the fraction of nodes with a depth greater than or equal to a given threshold.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @param threshold a threshold value for node depth.
#' 
#' @return The fraction of nodes with a depth greater than or equal to the threshold (see \code{\link{nodeDepth}} for more details on the meaning of node depth).
#' 
#' @import ape
#' 
#' @seealso \code{\link{nodeDepth}}
#'   
#' @examples
#' ## Find the fraction of nodes with a depth of 5 or more, in a random tree with 20 tips:
#' tree <- rtree(20)
#' tree$edge.length <- rep(1,38) # so that the depths are easier to view in a plot
#' plot(tree)
#' nodeDepthFrac(tree,threshold=5) 
#' 
#' @export
nodeDepthFrac <- function(tree,threshold) {
  # initial tree check
  tree <- phyloCheck(tree)
  
  ntips <- length(tree$tip.label) # number of tips

  depths <- getDepths(tree)
  alldepths <- c(depths$tipDepths,depths$nodeDepths)
  count <- sum(alldepths >= threshold)
  
  return(count/(2*ntips - 1))
}