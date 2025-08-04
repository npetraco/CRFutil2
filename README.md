-   Re-write of CRFutil, a handy utility package to go along with R packages:

    CRF (Wu): <https://github.com/wulingyun/CRF> (CRAN archived CRF here: <https://cran.r-project.org/web/packages/CRF/index.html>)

    gRbase (Høsjsgaard): <https://cran.r-project.org/web/packages/gRbase/index.html>

-   In CRFutil I chose to use a positive sign in the exponent of the Boltzmann/Gibbs distribution. I've regretted that stupid decision for years. It is now (the conventional) negative, i.e. $\Pr(X) \propto \exp(-E(X))$ and not $\Pr(X) \propto \exp(E(X))$.

-   Compatible with both Ising-like (two value discrete) and Potts-like (\>2 value discrete) node state spaces.

-   Fully C++ compatible and extensible.

-   To install:

1.  First install remotes, BiocManager, graph, Rgraphviz, RBGL:

    install.packages("remotes")

    install.packages("BiocManager")

    BiocManager::install("graph", force = TRUE)

    BiocManager::install("Rgraphviz", force = TRUE)

    BiocManager::install("RBGL", force = TRUE)

2.  Turn on remotes, install CRF and CRFutil2 from github. CRFutil2 should automatically install gRbase:

    library(remotes)

    install_github("wulingyun/CRF")

    install_github("npetraco/CRFutil2")

3.  Test and see if the CRFutil2 library loads. No error messages is a sign of success. Don't worry about any warning messages.

    library(CRFutil2)

-   Extensive notes and tutorials on the use of this code with discrete Markov Random Fields can be found in the inst directory.

    Old notes for CRFutil are here: <https://jjcweb.jjay.cuny.edu/npetraco/tutorials/R/MRF/Notes.pptx>

    Re-rewriting for CRFutil2, but what I have at this point is here: <https://github.com/npetraco/CRFutil2/blob/main/inst/doc/Notes/Notes_rev1.pptx>
