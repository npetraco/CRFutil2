# First install remotes and some packages now only available on Bioconductor
install.packages("remotes")
install.packages("BiocManager")
BiocManager::install("graph", force = TRUE)
BiocManager::install("Rgraphviz", force = TRUE)
BiocManager::install("RBGL", force = TRUE)


# Turn on remotes and install CRFutil2 from github
# CRFutil2 should automatically install CRF and gRbase:
library(remotes)
install_github("wulingyun/CRF")
install_github("npetraco/CRFutil2")


# Test and see if the CRFutil2 library loads. No error messages
# is a sign of success. Don't worry about any warning messages.
library(CRFutil2)
