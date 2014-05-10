\name{SimulatedOutbreakGenealogy}
\alias{maketranstree}
\alias{getgenealogy}
\alias{makephylotree}
\alias{sortmyepi}

\title{
Simulated outbreaks with genealogies
}
\description{
A few functions to simulate a rather simple outbreak where the infectiousness is not homogeneous, and then to extract a genealogy for the simulated infections. The outbreak simulation uses the \code{simNHP.fun} function in the NHPoisson package. Functions makephylotree and sortmyepi are intended for use by getgenealogy, not direct use. 
}
\usage{
	maketranstree(lambda,duration=1,NumCases=50)
	getgenealogy(epirecord,epsilon=0.01)
}
\arguments{
\item{lambda}{
A vector specifying the rate of the non-homogeneous Poisson process over time (ie the infectivity). The size of lambda also specifies the resolution with which the infection process is simulated. 
}
\item{duration}{
The duration of infectiousness (fixed).
}
\item{NumCases}{
An integer specifying the desired number of cases in the outbreak. 
}
\item{epirecord}{A simulated outbreak returned by maketranstree}
\item{epsilon}{A small number to ensure that getgenealogy returns positive branch lengths. This is relevant when individuals recover/are sampled in the same time step in which they became infected or when individuals infect several others in the same time step.}
}
\details{
\code{maketranstree} generates a transmission tree, starting with one infectious individual, where the infectiousness through time is given by the argument lambda. 

\code{getgenealogy} extracts a genealogy from the simulated transmission tree returned by maketranstree. It assumes that branching points in the genealogy correspond to infection events in the outbreak and that cases are sampled at the time of recovery. 
}
\value{
\item{maketranstree}{
A matrix with 4 named columns: Infectee, Infector, InfnTime, RecTime corresponding to the case number of the infected individual, the individual infecting them, the time of infection, and the individual's recovery time (same as the sampling time). 
}
\item{getgenealogy}{
An object of class phylo4 in which tips correspond to cases in the simulated outbreak and internal nodes correspond to the infection events among them. 
}
}
\author{
Caroline Colijn
}
\seealso{
\code{\link{treeAnalysis}} and \code{link{configurations}} for functions to explore the shapes of the genealogies.
}
\examples{
# specify an inhomogeneous rate of new infections
lambda<- c(rep(0.2,10), rep(0.1,10)) # twice as infectious in the first half of the 
# infectious period, with a mean number of 3 secondary cases

## Create a simulated outbreak
 epirecord<-maketranstree(lambda,duration=2,NumCases=30)

## Extract the genealogy as a phylo4 object
simgeneal<-getgenealogy(epirecord)

\dontrun{
	# Intermediate functions: 
  # creates the phylo4 object from a list of edges and lengths. 
	# makephylotree(Edges,Lengths,Root) 
	
	sortmyepi(epirecord) # sorts the rows of the simulated epidemic
}
}
