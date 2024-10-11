* Re-write of CRFutil, a handy utility package to go along with R packages CRF (https://cran.r-project.org/web/packages/CRF/index.html) and gRbase (https://cran.r-project.org/web/packages/gRbase/index.html). 

* In CRFutil I chose to use a positive sign in the exponent of the Boltzmann/Gibbs distribution. I've regretted that stupid decision for years. It is now (the conventional) negative.

* Compatible with both Ising (two value discrete) and Potts (>2 value discrete) type node state spaces.

* Fully C++ compatible and extensible. 

* To install:

1. First install remotes, BiocManager, graph, Rgraphviz and RBGL:

	install.packages("remotes")

	install.packages("BiocManager")

	BiocManager::install("graph", force = TRUE)

	BiocManager::install("Rgraphviz", force = TRUE)
	
	BiocManager::install("RBGL", force = TRUE)

2. Turn on remotes and install CRFutil2 from github. CRFutil2 should automatically install CRF and gRbase:

	library(remotes)

	install_github("npetraco/CRFutil2")

3. Test and see if the CRFutil2 library loads. No error messages is a sign of success. Don't worry about any warning messages.

	library(CRFutil2)

* Extensive notes and tutorials on the use of this code with discrete  Markov Random Fields can be found in the inst directory. 

