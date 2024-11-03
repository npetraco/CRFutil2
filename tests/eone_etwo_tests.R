# node weights:
tau2 <- c( 1, -1.3)
tau3 <- c( 1, -1.3, 0.1)

Eone(yA = 2, tA = tau3, ff = ff1, nss.dim=3)
Eone(yA = 3, tA = tau3, ff = ff1, nss.dim=3)

Eone(yA = 2, tA = tau2, ff = ff1)

Eone(yA = "a", tA = tau3, ff = ff1, nss.vec = c("a", "b", "c"))

Eone(yA = 2, tA = tau2, ff = ff0)
Eone(yA = 1, tA = tau2, ff = ff0)
Eone(yA = 3, tA = tau2, ff = ff0) # Throws error because less general ff0 used


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

Etwo(yA = 2, yB = 1, wAB = omega22, ff = ff1, nss.dim=2)
Etwo(yA = 2, yB = 1, wAB = omega22, ff = ff1, nss.dim=3) # Throws error
Etwo(yA = 2, yB = 2, wAB = omega33, ff = ff1, nss.dim=3) # Throws error

Etwo(yA = "c", yB = "b", wAB = omega33, ff = ff1, nss.vec=c("a", "b", "c"))

Etwo(yA = 2, yB = 1, wAB = omega22, ff = ff0)
Etwo(yA = 2, yB = 1, wAB = omega33, ff = ff0)
Etwo(yA = 2, yB = 1, wAB = omega33, ff = ff1) # Throws error bec nss.dim=2 assumed
Etwo(yA = 2, yB = 1, wAB = omega22, ff = ff1)
