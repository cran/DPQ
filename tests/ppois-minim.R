library(DPQ)
packageDescription("DPQ")

## Some platform stats :
str(.Machine, digits = 12)
Sys.info()
capabilities()

## Large Lam  where  exp(-lambda) underflows to 0 (even in 'long double'):

## "The minimal" large lambda is  11400
Lam <- 11400
M    <- ceiling((Lam + 4*sqrt(Lam))/50)*50
cat(sprintf("Lam = %g; M = %7.0f:\n", Lam, M))

pp. <- ppoisD( M,  lambda=Lam, verbose = 2)# "very verbose"
pp  <- ppois (0:M, lambda=Lam)

all.equal(pp, pp., tol = 1e-12)# gives '2' (factor of 2)  ONLY if running via valgrind ?????

