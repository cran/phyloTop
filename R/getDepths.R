#' Find the depth of each node
#' 
#' Determines the depth of each node, defined as the number of steps from the root.
#' (So the root has depth zero, its children have depth one, etc.)
#' The output is given as a list of two vectors: \code{tipDepths} gives the depths of the tips, and \code{nodeDepths} gives the depths of the internal nodes.
#' This replaces the deprecated \code{dists} function.
#' The code is based on that used in the function \code{computeHeights} in package \code{treeCentrality}.
#' 
#' @author Michelle Kendall \email{michelle.louise.kendall@@gmail.com}
#'   
#' @param tree a tree of class \code{phylo} or \code{phylo4}. The tree should be binary and rooted; if not it will be coerced into a binary rooted tree using multi2di, if possible.
#' @return A list of two vectors: \code{tipDepths} gives the depths of the tips, and \code{nodeDepths} gives the depths of the internal nodes.
#' 
#' @seealso \code{\link{nodeDepth}}, \code{\link{nodeDepthFrac}}
#' 
#' @import ape
#'   
#' @examples
#' ## Find the node depths in a random tree:
#' tree <- rtree(20)
#' treeDepths <- getDepths(tree)
#' ## Plot tree with node depths labelled:
#' tree$tip.label <- treeDepths$tipDepths
#' tree$node.label <- treeDepths$nodeDepths
#' plot(tree, show.node.label=TRUE)
#' 
#' 
#' @export
getDepths=function(tree) {
  # perform tree checks:
  tree <- phyloCheck(tree)
  
  ntip <- length(tree$tip.label)
  N <- ntip + tree$Nnode
  
  depths = rep(0, N) # initialise
  edges = tree$edge
  for (ind in 1:(N - 1)) {
    curRow = edges[ind,]
    depths[curRow[2]] = 1 + depths[curRow[1]]
  }
  depths
  
  tipDepths <- depths[1:ntip]
  nodeDepths <- depths[(ntip+1):length(depths)]
  return(list(tipDepths=tipDepths, nodeDepths=nodeDepths))
}