library(rbenchmark)

tau3 <- c( 1, -1.3, 0.1)
ff1_C(ns = 3, dots = list(nss.dim=3) )

Eone(yA = 3, tA = tau3, ff = ff1, nss.dim=3)
Eone_C(yA = 3, tA = tau3, ff = ff1_C, dots = list(nss.dim=3))


benchmark(
  Eone(yA = 3, tA = tau3, ff = ff1, nss.dim=3),
  Eone_C(yA = 3, tA = tau3, ff = ff1_C, dots = list(nss.dim=3))
)[,1:3]


# edge weights:
omega22 <- rbind(
  c( 3.5, -1.4),
  c(-1.4,  2.5)
)

omega33 <- rbind(
  c( 3.5, -1.4,   2.5),
  c(-1.4,  2.5,  -0.8),
  c( 3.8,  0.95, -7.6)
)

Etwo_C(yA = 2, yB = 1, wAB = omega22, ff = ff1_C, list(nss.dim=2))
Etwo_C(yA = 2, yB = 1, wAB = omega22, ff = ff1_C, list(nss.dim=3)) # Throws error
Etwo_C(yA = 2, yB = 2, wAB = omega33, ff = ff1_C, list(nss.dim=3))

Etwo_C(yA = 3, yB = 2, wAB = omega33, ff = ff12_RC, list(nss.vec=1:3))

Etwo_C(yA = 2, yB = 1, wAB = omega22, ff = ff0_C)
Etwo_C(yA = 2, yB = 1, wAB = omega33, ff = ff0_C)
Etwo_C(yA = 2, yB = 1, wAB = omega33, ff = ff1_C) # Throws error bec nss.dim=2 assumed
Etwo_C(yA = 2, yB = 1, wAB = omega22, ff = ff1_C)


benchmark(
  Etwo(yA = 3, yB = 2, wAB = omega33, ff = ff0, ss.dim=3),
  Etwo_C(yA = 3, yB = 2, wAB = omega33, ff = ff12_RC, list(nss.vec=1:3))
)[,1:3]

