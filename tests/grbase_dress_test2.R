library(CRFutil2)

grphf <- ~A:B + B:C + C:A
adj   <- ug(grphf, result = "matrix")
tri   <- make.empty.field(graph.eq = grphf, num.states = 3, parameterization.typ="standard", plotQ=T)

# Make up some random parameters and insert them into the model:
set.seed(1)
tri.rthetas <- rnorm(n = tri$num.par, mean = 0, sd = 1)
insert.params.and.pots(tri, params.samp = tri.rthetas)
dump.crf(tri)
make.gRbase.potentials(tri)
