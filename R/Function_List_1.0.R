##
## All NODE ANALYSIS ----
##

nDescendants <- function(tree) {
  desc <- descendants(tree,nodeId(tree),type='all')
  descNo <- apply(as.array(1:length(desc)),1,function(x){length(desc[[x]])})
  return(descNo)
}

nTipDescendants <- function(tree) {
  desc <- descendants(tree,nodeId(tree),type='tips')
  tipDes <- apply(as.array(1:length(desc)),1,function(x){length(desc[[x]])})
  return(tipDes)
}

dists <- function(tree) {
  edgeLength(tree) <- rep(1,nEdges(tree))
  arr <- as.array(1:length(nodeId(tree)))
  nodeDep <- apply(arr,1,function(x){nodeDepth(tree,x)-1})
  #-1 as the root has 'depth'=1
  return(nodeDep)
}

ladderNums <-function(tree) {
  arr <- as.array(nodeId(tree))
  locLadd <- function(x) {
    if (nodeType(tree)[x]=='tip') {return(0)}
    else {return(rootLaddDist(subset(tree,node.subtree=x)))}
  }
  ladderNumbers <- apply(arr,1,locLadd)
  return(ladderNumbers)
}

treeImb <- function(tree) {
  arr <- as.array(nodeId(tree))
  return(apply(arr,1,function(x){nodeImb(tree,x)}))
}

##
## TREE ANALYSIS ----
##

splitTop <- function(tree,dist) {
  nodeDists <- dists(tree)
  if (dist > max(nodeDists)) {stop('dist too large')}
  nodes <- as.array(nodeId(tree)[nodeDists==dist])
  splits <- apply(nodes,1,function(x){length(descendants(tree,x,type='tips'))}) 
  return(sort(splits))
}

sackin <- function(tree) {
  edgeLength(tree) <- rep(1, nEdges(tree))
  arr <- as.array(1:nTips(tree))
  nodeDep <- apply(arr, 1, function(x) {
    nodeDepth(tree, x) - 1
  })
  return(sum(nodeDep))
}

widths <- function(tree) {
  nodeDists <- dists(tree)
  arr <- as.array(0:max(nodeDists))
  wids <- apply(arr,1,function(x){sum(nodeDists==x)})
  return(wids)
}

avgLadder <- function(tree) {
  ladderNumbers <- ladderNums(tree)
  sum <- 0
  nLadds <- 0
  m <- max(ladderNumbers)
  while (m > 0) {
    count <- sum(ladderNumbers==m)
    nLadds <- nLadds + count
    sum <- sum + m*count
    ladderNumbers <- laddItr(ladderNumbers)
    m <- max(ladderNumbers)
  }
  return(sum/nLadds)
}

nLadders <- function(tree) {
  ladderNumbers <- ladderNums(tree)
  nLadds <- 0
  m <- max(ladderNumbers)
  while (m > 0) {
    count <- sum(ladderNumbers==m)
    nLadds <- nLadds + count
    ladderNumbers <- laddItr(ladderNumbers)
    m <- max(ladderNumbers)
  }
  return(nLadds)
}

colless <- function(tree,normalize=TRUE) {
  tImb <- treeImb(tree)
  diffs <- abs(apply(tImb,2,diff))
  if (normalize) {
    n <- nTips(tree)
    m <- 2/((n-1)*(n-2))
    return(sum(diffs)*m)
  }
  return(sum(diffs))
}

topSumm <- function(tree,topList) {
  l <- length(topList)
  if (l==1) {topList <- list(topList)}
  mat <- matrix(0,1,l)
  for (i in 1:l) {
    mat[1,i] <- topList[[i]](tree)
  }
  return(mat)
}

##
## CONFIGURATIONS ----
##

nConfig <- function(tree,configSize) {
  return(sum(nTipDescendants(tree)==configSize))
}

configFind <- function(tree,configSize) {
  nDes <- nTipDescendants(tree)
  return(nodeId(tree)[nDes==configSize])
}

configTop <- function(tree) {
  nTipDes <- nTipDescendants(tree)
  arr <- as.array(1:max(nTipDes))
  top <- apply(arr,1,function(x){sum(nTipDes==x)}) #Stands for topology
  return(top)
}

classifyConfigFour <- function(tree) {
  nTipDes <- nTipDescendants(tree)
  fourConfig <- nodeId(tree)[nTipDes==4]
  laddType <- 0
  branchType <- 0
  for (node in fourConfig) {
    case <- nTipChildren(tree,node)
    if (case==1) {laddType <- laddType +1}
    else {branchType <- branchType + 1}
  }
  return(data.frame(laddType=laddType,branchType=branchType))
}

classifyConfigFive <- function(tree) {
  nTipDes <- nTipDescendants(tree)
  fiveConfig <- nodeId(tree)[nTipDes==5]
  laddType <- 0 #Has the largest ladder
  branchType <- 0 #Most branchy
  fourType <- 0 #Has a (4,1) split in tip descendants at the top (but is not a ladder)
  for (node in fiveConfig) {
    case <- nTipChildren(tree,node)
    if (case==1) {
      laddSize <- ladderDist(tree,node)
      if (laddSize==3) {laddType <- laddType +1}
      else {fourType <- fourType + 1}
    }
    else {branchType <- branchType + 1}
  }
  return(data.frame(laddType=laddType,fourType=fourType,branchType=branchType))
}

##
## NODE ANALYSIS ----
##

nodeImb <- function(tree,node) {
  if (nodeType(tree)[node]=='tip') {return(c(0,0))}
  else {
    childs <- children(tree,node)
    left <- length(descendants(tree,childs[1],type='tips'))
    right <- length(descendants(tree,childs[2],type='tips'))
    if (left > 1) {left <- left + 1} # These lines add on the child itself in the case 
    if (right > 1) {right <- right + 1} # that the child is not a tip
    return(c(left,right))
  }
}

ladderDist <- function(tree,node) {
  return(rootLaddDist(subset(tree,node.subtree=node)))
}

nodeDist <- function(tree,node) {
  edgeLength(tree) <- rep(1,nEdges(tree))
  return(nodeDepth(tree,node)-1) #-1 as the root has dist=1
}

##
## MODEL TESTING ----
##

modelSummary <- function(model,topList,n, loadingBar=TRUE) {
  l <- length(topList)
  output <- matrix(0,n,l)
  if (loadingBar) {print('Number of topologies computed:')}
  for (i in 1:n){
    tree <- model() # Create new tree from model
    output[i,] <- topSumm(tree,topList)
    if (loadingBar) {print(i)}
  }
  output <- data.frame(output)
  return(output)
}

modelCreate <- function(model,n,loadingBar=FALSE) {
  output <- list()
  if (loadingBar) {print('Number of topologies computed:')}
  for (i in 1:n) {
    output <- c(output,model())
    if (loadingBar) {print(i)}
  }
  return(output)
}

treeListSummary <- function(treeList,topList,loadingBar=TRUE) {
  l1 <- length(treeList)
  l2 <- length(topList)
  output <- matrix(0,l1,l2)
  if (loadingBar) {print('Number of topologies computed:')}
  for (i in 1:l1) {
    output[i,] <- topSumm(treeList[[i]],topList)
    if (loadingBar) {print(i)}
  }
  return(data.frame(output))
}

##
## TREE GRAPHICS ----
##

# Modified. Rd changed
ladderShow <- function(tree) {
  ladderNumbers <- ladderNums(tree)
  col <- rep('black',nEdges(tree))
  # If the ladder number is at least one make the edge red
  E <- edges(tree)
  for (i in 2:nEdges(tree)) {
    if (ladderNumbers[E[i,1]] > 0) {col[i] <- 'red'}
  }
  plot(tree,edge.color=col)
}

# Modified. Rd changed
configShow <- function(tree,configSize) {
  nTipDes <- nTipDescendants(tree)
  # Finds the nodes of the correct configsize
  confFind <- nodeId(tree)[which(nTipDes==configSize)] 
  # Prints the tree
  subtreeShow(tree,confFind)
}

# New. Add Rd
subtreeShow <- function(tree,nodeList) {
  colNums <- numeric()
  col <- rep('black',nEdges(tree))
  # Finds the descendants of the nodes in nodeList
  for (node in nodeList) {
    colNums <- c(colNums,descendants(tree,node,type='all'))
  }
  E <- edges(tree)
  for (i in 2:nEdges(tree)) { # Colors the correct edges
    if (any(colNums==E[i,2])) {col[i] <- 'red'}
  }
  plot(tree,edge.color=col) # Prints the tree
}

##
## UTILITY ----
##

rtree4 <- function(n) {
  return(as(rtree(n), 'phylo4'))
}

idNodeLabel <- function(tree) {
  ndLbl <- paste(nodeId(tree,'internal'))
  tpLbl <- paste(nodeId(tree,'tip'))
  nodeLabels(tree) <- ndLbl
  tipLabels(tree) <- tpLbl
  return(tree)
}

nTipChildren <- function(tree,node){
  childs <- children(tree,node)
  return(sum(nodeType(tree)[childs]=='tip'))  
}

##
## OTHER ----
## These have independant documentation

rootLaddDist <- function(tree) {
  root <- nodeId(tree)[nodeType(tree)=='root']
  childs <- children(tree,root)
  case <- sum(nodeType(tree)[childs]=='tip') #Number of tip children of root
  if (case==1) {
    nonTipChild <- childs[nodeType(tree)[childs]=='internal']
    return(1+rootLaddDist(subset(tree,node.subtree=nonTipChild)))
  }
  else {return(0)} #Not part of a ladder or 2 tip children
}

laddItr <- function(ladderNumbers) {
  m <- max(ladderNumbers)
  count <- sum(ladderNumbers==m) # How many of the max there are
  while (m > 0) { # Take off the required number of m's
    newCount <- sum(ladderNumbers==m)
    ladderNumbers <- ladderNumbers[ladderNumbers!=m] # Take off every m
    ladderNumbers <- c(ladderNumbers,rep(m,newCount-count)) # Add on the required number
    m <- m - 1 # Reduce m
  }
  return(ladderNumbers)  
}
