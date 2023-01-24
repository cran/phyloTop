#' Tree imbalance
#' 
#' Find the imbalance of each node, that is the number of tip descendants of each of its two children. With thanks to Leonid Chindelevitch for use of code from \code{computeLRValues} from \code{treeCentrality}.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' 
#' @return A matrix where rows correspond to nodes of the tree. The two column entries correspond to the number of tip descendants of each of its two children. (Note that this is the transform of the output in phyloTop version 1.0.0.)
#' Where the row number corresponds to a tip, the entries are (0,0).
#' 
#' @seealso \code{\link{nodeImb}}
#'
#' @import ape
#' 
#' @examples
#' ## Find the imbalance numbers in a random tree with 10 tips:
#' tree <- rtree(10)
#' plot(tree)
#' nodelabels()
#' treeImb(tree)
#' 
#' @export
treeImb <- function(tree) {
  # tree check:
  tree <- phyloCheck(tree)
  # basic values:
  ntips <- length(tree$tip.label)
  nNodes <- ntips + tree$Nnode
  edges <- tree$edge
  
  imbalance <- matrix(NA, ntips-1, 2) # initialise matrix of descendant numbers
  
  for (tmp in (nNodes - 1):1) {
    tmpRow <- edges[tmp,] - ntips # row "tmp" of edge matrix gives the labels of a (parent, child) pair of nodes; subtract the number of tips from these labels
    # if imbalance[tmpRow[1],1] is NA, fill it in; if it already has a value, fill in imbalance[tmpRow[1],2] as follows:
    # if tmpRow[2] <= 0, fill in with the value 1
    # otherwise, fill with 1 + sum(imbalance[tmpRow[2],] (the total of the row tmpRow[2])
    imbalance[tmpRow[1], 2 - is.na(imbalance[tmpRow[1],1])] <- 1 + ifelse(tmpRow[2] <= 0, 0, sum(imbalance[tmpRow[2],]))
  }
  
  imbalance <- (imbalance + 1) / 2 # for each internal node (row), this gives the number of tip descendants of each of its two children (two columns)
  
  imbalance <- rbind(matrix(0, ntips, 2), imbalance) # include zeroes for tips
  
  rownames(imbalance) <- c(tree$tip.label, paste0("node",(ntips+1):nNodes))
   
  return (imbalance)
}
