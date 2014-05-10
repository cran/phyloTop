##
## All NODE ANALYSIS ----
##

nDescendants <- function(tree) {
	nodelist<- seq(nTips(tree)+1,2*nTips(tree)-1) 
desc<-apply(as.array(nodelist),1,function(x,tree) descendants(tree,x,type="all"),tree)
#  desc <- descendants(tree,nodeId(tree),type='all') # old version
  descNo <- apply(as.array(1:length(desc)),1,function(x){length(desc[[x]])})
  return(descNo)
}

nTipDescendants <- function(tree) {
nodelist<- seq(nTips(tree)+1,2*nTips(tree)-1)  
desc<-apply(as.array(nodelist),1,function(x,tree) descendants(tree,x,type="tips"),tree) # CC added
#   desc <- descendants(tree,nodeId(tree),type='tips') # old version
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

nodeApply <- function(tree, func, showId = TRUE) {
  ndTy <- nodeType(tree)
  intNds <- ndTy[which(ndTy!='tip')]
  arr <- as.array(1:length(intNds))
  f <- function(n) {
    node <- as(names(intNds)[n],'numeric')
    func(subset(tree, node.subtree = node))
  }
  output <- apply(arr,1,f)
  if (showId == TRUE) {names(output) <- names(intNds)}
  return(output)
}

##
## TREE ANALYSIS ----
##
cherries<-function(tree) {nConfig(tree,2)}
pitchforks<-function(tree){nConfig(tree,3)}

maxheight<-function(tree){
	edgeLength(tree)[2:length(edgeLength(tree))]=1
	alldepths<-apply(as.matrix(seq(1:nTips(tree))),1,function(x,tree) nodeDepth(tree,x), tree)
	return(max(alldepths))
}
stairs <- function (tree) 
{
  N <- nTips(tree)
  NDs <- treeImb(tree)[, (N + 1):(2 * N - 1)]
  stair1 <- (1/(N - 1)) * sum(abs(NDs[2, ] - NDs[1, ]))
  stair2 <- (1/(N - 1)) * sum(pmin(NDs[2, ], NDs[1, ])/pmax(NDs[2, ], NDs[1, ]))
  return(c(stair1, stair2))
}
ILnumber <- function(tree) {
N <- nTips(tree)
NDs <- treeImb(tree)[,(N+1):(2*N-1)]
return(sum(apply(NDs,2, function(x) sum(x==1)==1))) # IL number
}

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
  if (nTips(tree)==2) {return(0)}
  tImb <- treeImb(tree)
  diffs <- abs(apply(tImb,2,diff))
  if (normalize) {
    n <- nTips(tree)
    m <- 2/((n-1)*(n-2))
    return(sum(diffs)*m)
  }
  return(sum(diffs))
}

nodeFrac <- function(tree,func,threshold) {
  nodeVals <- nodeApply(tree,func)
  count <- sum(nodeVals >= threshold)
  return(count/length(nodeVals))
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
  if (loadingBar) {print('Complete')}
  return(output)
}

modelCreate <- function(model,n,loadingBar=FALSE) {
  output <- list(rep(0,n))
  if (loadingBar) {print('Number of trees created:')}
  for (i in 1:n) {
    output[[i]] <- model()
    if (loadingBar) {print(i)}
  }
  if (loadingBar) {print('Complete')}
  return(output)
}

treeListSummary <- function(treeList,topList,loadingBar=TRUE) {
  l1 <- length(treeList)
  l2 <- length(topList)
  output <- matrix(0,l1,l2)
  if (loadingBar) {print('Number of topology profiles computed:')}
  for (i in 1:l1) {
    output[i,] <- topSumm(treeList[[i]],topList)
    if (loadingBar) {print(i)}
  }
  if (loadingBar) {print('Complete')}
  return(data.frame(output))
}

##
## FAST MODEL TESTING ----
##

baseCreate <- function(tree,baseFuncs) {
  # Creates a base list for the tree to be used later
  # Check works for a single tree
  arr <- as.array(1:length(baseFuncs))
  output <- lapply(arr,function(x){baseFuncs[[x]](tree)})    
  return(output)
}

baseListCreate <- function(treeList,baseFuncs,loadingBar = TRUE) {
  # Creates a base list for each tree in treeList
  # Returns them as a list
  l <- length(treeList)
  output <- list(rep(0,l))
  arr <- as.array(1:length(baseFuncs))
  if (loadingBar) {print('Number of topology bases computed:')}
  for (i in 1:l) {
    output[[i]] <- lapply(arr,function(x){baseFuncs[[x]](treeList[[i]])})    
    if (loadingBar) {print(i)}
  }
  if (loadingBar) {print('Bases computed')}
  return(output)
}

baseAnalysis <- function(base,topFuncs) {
  l <- length(topFuncs)
  if (l==1) {topFuncs <- list(topFuncs)}
  mat <- matrix(0,1,l)
  for (i in 1:l) {
    mat[1,i] <- topFuncs[[i]](base[[i]])
  }
  return(mat)
}

baseListAnalysis <- function(baseList,topFuncs,loadingBar = FALSE) {
  # topFuncs is a list of functions that operate on elements of baseList
  # baseList is a of the type created by baseListCreate
  l1 <- length(baseList)
  l2 <- length(topFuncs)
  output <- matrix(0,l1,l2)
  if (loadingBar) {print('Number of topology profiles computed:')}
  for (i in 1:l1) {
    output[i,] <- baseAnalysis(baseList[[i]],topFuncs)
    if (loadingBar) {print(i)}
  }
  if (loadingBar) {print('Topology profiles computed')}
  return(data.frame(output))
}

##
## TOPFUNCS ----
##

fWidths <- function(nodeDists) {
  arr <- as.array(0:max(nodeDists))
  wids <- apply(arr,1,function(x){sum(nodeDists==x)})
  return(wids)
}

fAvgLadder <- function(ladderNumbers) {
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

fNLadders <- function(ladderNumbers) {
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

fColless <- function(treeImbalance) {
  n <- (length(treeImb)+2)/4
  if (n==2) {return(0)}
  diffs <- abs(apply(treeImbalance,2,diff))
  m <- 2/((n-1)*(n-2))
  return(sum(diffs)*m)
}

fNConfig <- function(nTipDes,configSize) {
  return(sum(nTipDes==configSize))
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

phyloCheck <- function(object,error=TRUE) {
  if (class(object) != 'phylo4') {
    if (error == TRUE) {
      print('Error in phyloCheck(tree): Object not of class "phylo4"')
      print('Try phy <- as(tree,phylo4) to create an object of the correct type')
    }
    return(FALSE)
  }
  if (nTips(object)-nNodes(object) !=1) {
    if (error == TRUE) {
      print('Error in phyloCheck(tree): The tree is not binary')
    }
    return(FALSE)
  }  
  return(TRUE)
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

##
## CAROLINE'S CODE ----
##
maketranstree <- function(lambda,duration=1,NumCases=50) {
	lambda<- c(lambda,0) # required because of < instead of <= in simNHP.fun
	epirecord<-matrix(0,1,4) # infectee, infector, infection time, recovery time
	cis<-1 #cis for current and pending infectors
	Infectee<-1; 
	rec<-duration; # note fixed duration of infection. 
	epirecord[1,]<-c(1,0,0, rec); 
	while (nrow(epirecord)<=NumCases & length(cis)>0) {
		itimes<-simNHP.fun(lambda)$posNH
		itimes<-itimes/(length(lambda)-1); # scaled so in [0,1]
		ci<-cis[1] # current infector. 
		NumInfected<- length(itimes) 
		if (NumInfected>0) {
			for (n in 1:NumInfected){
				Infectee<-Infectee+1; 
				epirecord<-rbind(epirecord,c(Infectee,ci, epirecord[ci,3]+itimes[n],epirecord[ci,3]+itimes[n]+rec))
				cis<-c(cis,Infectee)
			}
		}
		cis<-cis[-1] # 
	}
	colnames(epirecord)<-c("Infectee","Infector","InfnTime","RecTime")
	return(epirecord)
}


sortmyepi<-function(cmjepi) {
	sortinf<- sort(cmjepi[,"InfnTime"],decreasing=TRUE,index.return=TRUE)
	IND<-sortinf$ix
	return(cmjepi[IND,])	
}


getgenealogy <- function(epirecord,epsilon=0.01) {
	# make genealogy from the sorted tree info produced by sortmyepi.  
	
sortedtreeinfo<-sortmyepi(epirecord)

Infectors <- sortedtreeinfo[,"Infector"];
Infectees <- sortedtreeinfo[,"Infectee"];
InfTimes <- sortedtreeinfo[,"InfnTime"];
SortedRecTimes <- sortedtreeinfo[,"RecTime"]; 


SortedRecTimes[which(SortedRecTimes==InfTimes)] <- SortedRecTimes[which(SortedRecTimes==InfTimes)] + epsilon

NN <- length(InfTimes)-1; 
NumLeaves <- NN+1; 
BranchNums <- (NumLeaves+1):(2*NumLeaves-1); # IDs for internal branches of the genealogy
LengthstoInternals <- 0*(1:NN)
TipLengths <-0 *(1:length(Infectees))
B <- matrix(0,nrow=NN, ncol=2) # use a 2-col [desc desc] format; convert afterwards


for (n in 1:NN ) {
	#### PART 1 : connecting the internal branches / tips
	if (n ==1 ) { 
		B[n,]<-c(Infectors[n], Infectees[n]);
		} else { 
						
			# 1: did the infectEE go on to infect anyone else? 
			Onward <- is.element(Infectees[n],Infectors[1:(n-1)])
			if (Onward) {
				Desc1 <- BranchNums[max(which(Infectors[1:(n-1)] == Infectees[n]))]
			}else {
				Desc1 <- Infectees[n]
			}
			
			# 2: did the infectOR go on to infect anyone else? 
			Onward <- is.element(Infectors[n],Infectors[1:(n-1)])
			if (Onward){
				Desc2 <- BranchNums[max(which(Infectors[1:(n-1)]==Infectors[n]))]
			} else {
				Desc2 <- Infectors[n]
			}
			B[n,] <- c(Desc1,Desc2) # if n is NN, then this branch is the root. 
			} 
			
			
		### PART 2: lengths of internal branches	
HasInfBefore <- is.element(Infectors[n],Infectors[(n+1):length(Infectors)])
if (HasInfBefore){
	IND=n+ min(which(Infectors[(n+1):length(Infectors)]==Infectors[n])) # most recently
        LengthstoInternals[n]=InfTimes[n]-InfTimes[IND]; # corresponds to int pt n, 
			} else {
	IND<- which(Infectees==Infectors[n]) # index when this guy got infected
	LengthstoInternals[n]=InfTimes[n]-InfTimes[IND] 
			}
	 } 
	 
	### now lengths of tip branches
for (n in 1:length(Infectees)) {
	HasInfBefore<-is.element(Infectees[n],Infectors)  
	if (HasInfBefore) {
		IND=min(which(Infectors==Infectees[n])) 
		TipLengths[n] <- SortedRecTimes[n]-InfTimes[IND] 
	} else {
		TipLengths[n]<-SortedRecTimes[n]-InfTimes[n]
	}
}
## now create the genealogy: change format
 
Edges=matrix(0,nrow=2*NN, ncol=2); Lengths<-0*(1:(2*NN))

# the root is the chronologically first internal branch
for (n in 1:NN) {
	Edges[2*n-1,1]<- n+NN+1; Edges[2*n,1]=n+NN+1; Edges[2*n-1,2]<-B[n,1]; Edges[2*n,2]<-B[n,2]
	}
Lengths[1] <- 0;
for (n in 2:(2*NN)) {
	IsDescTip <- (Edges[n,2]<= NN)
	if (IsDescTip) {
Lengths[n]<- TipLengths[Edges[n,2]]		
	} else {
		Lengths[n]<- LengthstoInternals[Edges[n,2]-NN]
	}
} 
Lengths[Lengths==0]=epsilon
Edges[Edges>NumLeaves]= 3*NumLeaves - Edges[Edges>NumLeaves]; # so branches start at ROOT
Genealogy <- makephylotree(Edges,Lengths,NumLeaves+1) # NOW root is first branch, not last
return(Genealogy)
}


makephylotree <- function(Edges, Lengths, Root) {
G <- graph(edges=t(Edges));
orderT <- graph.dfs(G,Root)$order; 
 newLengths=0*Lengths; 

oE<-order(Edges[,2]); Edges <- Edges[oE, ];  Lengths <-Lengths[oE]; 
newEdges <- matrix(NA, nrow(Edges), ncol(Edges)) 
ooT<-order(orderT[-1]);
newEdges[ooT,] <- Edges
newLengths[ooT]= Lengths
# newLengths[newLengths==0]=0.01

Nnode <- (length(orderT)-1)/2;
Ntips <- Nnode+1; 
pt <- rtree(Ntips); 
pt$Nnode <- Nnode; #
pt$edge <-  newEdges;
pt$edge.length=newLengths;
pt$tip.label=paste("t",1:Ntips,sep="")
return(as(pt,"phylo4"))
}
    




