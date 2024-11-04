remotes::install_github("bnprks/Rgperftools")

library(Rgperftools)

# Force devtools to compile without debug flags for optimal performance profiling
library(pkgbuild)
flags <- pkgbuild::compiler_flags(debug=FALSE)
new_compiler_flags <- function(debug=FALSE) {flags}
assignInNamespace('compiler_flags', new_compiler_flags, 'pkgbuild')

# Load your library
devtools::load_all()


# Collect profiling data
start_profiler(paste0(tempdir(), "/profile.out"))
#run_your_cpp_stuff()
ff0_C(st = "b", ss_dim = 2, st_vec = c("a","b"))
stop_profiler()
file.edit(paste0(tempdir(), "/profile.out"))

paste0(tempdir(), "/profile.out")
