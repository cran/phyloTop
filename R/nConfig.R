#' Configuration sizes in a tree
#' 
#' Finds the sizes of configurations in the tree. 
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @param maxClade an integer between 1 and the number of tips (the default), specifying the maximum clade size of interest.
#' 
#' @return A list with 2 entries: 
#' \itemize{
#' \item cladeSizes is a vector giving the size of the clade descending at each node:tips all have the value 1, internal nodes have their number of tip descendants. 
#' \item numClades is a vector where numClades[[i]] is the number of clades of size i in the tree. 
#' All clade sizes are calculated, but the output can be restricted using \code{maxClade} to just those of size up to 'maxClade'.
#' }
#' 
#' @import ape
#'  
#' @examples
#' ## Configuration sizes on a random tree with 10 tips:
#' tree <- rtree(10)
#' plot(tree)
#' nodelabels()
#' nConfig(tree)
#' 
#' 
#' @export
nConfig <- function(tree,maxClade=NULL) {
  tree <- phyloCheck(tree)
  
  # basic values:
  ntips <- length(tree$tip.label)
  nNodes <- ntips + tree$Nnode
  
  # check maxClade
  if (is.null(maxClade)) {maxClade <- ntips}
  else if(maxClade > ntips) {maxClade <- ntips} # maxClade greater than number of tips makes no sense and would append unnecessary zeroes to output
  
  imbalance <- treeImb(tree)
  cladeSizes <- c(rep(1,ntips),rowSums(imbalance[(ntips+1):nNodes,]))
  names(cladeSizes)[1:ntips]=tree$tip.label;
  names(cladeSizes)[(ntips+1):nNodes]=paste0("node",(ntips+1):nNodes)
  
  numClades <-  vapply(1:maxClade, function(x) sum(cladeSizes==x),FUN.VALUE=1)
  names(numClades) <- 1:maxClade
  
  return(list(cladeSizes=cladeSizes,numClades=numClades))
}
