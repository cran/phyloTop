#' Stairs
#' 
#' Calculates the staircase-ness measure defined in Norstrom et al., Evolutionary Bioinformatics online, 8:261 (2012) \doi{10.4137/EBO.S9738}.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @return Two numbers corresponding to the two staircase-ness measures for a tree. These are two related measures:
#' \itemize{ 
#' \item 1: the proportion of subtrees that are imbalanced (i.e. subtrees where the left child has more tip descendants than the right child, or vice versa)
#' \item 2: the average of all the min(l,r)/max(l,r) values of each subtree, where l and r are the number of tips in the left and right children of a subtree.
#' }
#' 
#' @import ape
#'   
#' @examples
#' ## Find the staircase-ness measures in a random tree with 20 tips:
#' stairs(rtree(20))
#'  
#' 
#' @export
stairs <- function(tree)  {
  tree <- phyloCheck(tree)
  N <- length(tree$tip.label)
  # if N=2:
  if (N==2) {
    stair1 <- 0
    stair2 <- 1
  }
  else {
    tImb <- treeImb(tree)
    tImb <- tImb[(N+1):(2*N-1),]
    stair1 <- (1/(N - 1)) * sum(tImb[,1] != tImb[,2]) 
    stair2 <- (1/(N - 1)) * sum(pmin(tImb[,2], tImb[,1])/pmax(tImb[,2], tImb[,1]))
  }
  return(c(stair1, stair2))
}