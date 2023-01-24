#' Fraction of nodes with given imbalance
#' 
#' Calculate the fraction of internal nodes with an imbalance greater than or equal to a given threshold.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @param threshold a threshold value for node imbalance. 
#' 
#' @return For any internal node, the function \code{\link{nodeImb}} gives the number of tip descendants of each of the node's descending branches. The function \code{nodeImbFrac} returns the fraction of internal nodes where the difference between these numbers is greater than or equal to the threshold.
#' 
#' @import ape
#' 
#' @seealso \code{\link{nodeImb}}
#'   
#' @examples
#' ## Find the fraction of internal nodes with an imbalance of 5 or more, 
#' ## in a random tree with 20 tips:
#' tree <- rtree(20)
#' plot(tree)
#' nodeImbFrac(tree,threshold=5) 
#' 
#' @export
nodeImbFrac <- function(tree,threshold) {
  # initial tree check
  tree <- phyloCheck(tree)
  
  ntips <- length(tree$tip.label) # number of tips
  tImb <- treeImb(tree)
  
  tImb <- tImb[(ntips+1):(2*ntips - 1),]
  
  count <- sum(abs(tImb[,1]-tImb[,2]) >= threshold)
  
  return(count/(ntips-1))
}