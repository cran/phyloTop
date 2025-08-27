phyloTop v2.1.3
==================

Patch to update the format of cross-references to other packages in Rd files as required by new CRAN standards. Change of maintainer's email address.


phyloTop v2.1.2
===============

We are grateful to Orlando Schwery for bringing to our attention that 'is.binary.tree()' from ape is deprecated and was issuing a warning whenever phyloCheck was called. We have now changed it to the supported 'is.binary()'. 


phyloTop v2.1.0
===============

We are very grateful to Leonid Chindelevitch for his comments which have led to this significantly improved version of phyloTop: most of the functions are now calculated in linear time and we have fixed errors in the calculation of the Sackin and stairs measures. 


phyloTop v2.0.0
===============

phyloTop v1.1.1 was archived on CRAN on 2015-10-29 as it was no longer maintained by its creator, Michael Boyd.

The package has been revived by Michelle Kendall and Caroline Colijn. All of the functions in version 2.0.0 have been updated, with faster implementations, simplified format requirements and dependencies, and further help files and examples added. We have included explanations in the help files wherever version 2.0.0 is not backwards compatible, and documented the deprecated functions.

Major updates include:

* all functions accept trees of class phylo or phylo4, automatically changing them to class phylo and coercing them into binary, rooted trees wherever possible.

* the functions nConfig, ladderSizes and getDepths (on which many of the others depend) have been freshly implemented and are significantly faster.

* the new function phyloTop takes a list of trees and applies a variety of topological functions, returning a matrix of topological statistics (faster than calling each function individually). Other new functions are: makeEpiRecord, nodeImbFrac, nodeDepthFrac, phyloCheck.

* many minor functions were only called by one main function - these have been deprecated and subsumed into the code for the main function. Some minor functions are unnecessary now that compuations are performed on trees in phylo rather than phylo4 format - these have been deprecated. Others have been updated and marked as internal.

* the functions avgLadder, cherries, ILnumber, maxHeight, pitchforks, and sackin.phylo now contain normalisation options. We thank Giacomo Plazzotta for enabling this by providing their maximum values.

* errors arising from submitting a tree with two tips have now been removed. We thank Scott Ward for spotting this problem and supplying code fixes.

* further plotting options have been added to the tree visualisation functions configShow, ladderShow and subtreeShow.