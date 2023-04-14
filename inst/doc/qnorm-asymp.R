### R code from vignette source 'qnorm-asymp.Rnw'

###################################################
### code chunk number 1: preliminaries
###################################################
pdfDB <- function(name, width, height, ...)
{
    grDevices::pdf(paste0(name, ".pdf"), ## "DB":             vvvvvvvvvvvvvvvv
                   width=width, height=height, onefile=FALSE, useDingbats=TRUE)
}
op.orig <-
options(width = 70, useFancyQuotes = FALSE
        ## , SweaveHooks = list(fig=function() par(mar=c(5.1, 4.1, 1.1, 2.1)))
        ## JSS--ugly!  , prompt="R> ", continue="+  "
      , continue = "   "
        )
Sys.setenv(LANGUAGE = "en")
if(.Platform$OS.type != "windows")#$
  Sys.setlocale("LC_MESSAGES","C")
if(Sys.getenv("USER") == "maechler") # my latest checked development version
    require("DPQ", lib="~/R/Pkgs/DPQ.Rcheck-64b")
## take CRAN's version, not development one:
##    require("DPQ", lib="~/R/Pkgs/CRAN_lib")


###################################################
### code chunk number 2: qnormLog-compute
###################################################
qs <- 2^seq( 0, 29, by=1/256) # => s >= 1.84
lp <- pnorm(qs, lower.tail=FALSE, log.p=TRUE)
s <- -lp # = -pnorm(..) = -log(1 - Phi(qs)) > 0
require("DPQ") # -->  qnormR():
qnrm    <- qnorm (-s, lower.tail=FALSE, log.p=TRUE)
qnrm405 <- qnormR(-s, lower.tail=FALSE, log.p=TRUE, version= "4.0.x") # R <= 4.0.5
qnrm410 <- qnormR(-s, lower.tail=FALSE, log.p=TRUE, version= "2020-10-17")
qnrm43  <- qnormR(-s, lower.tail=FALSE, log.p=TRUE, version= "2022")
Rver <- sfsmisc::shortRversion()
if(getRversion() <= "4.0.5") { # our qnormR(.., version="4.0.x")
    cat(sprintf("%s, \"4.0.5\",\n   all.equal(*, tol=0): %s;  identical(): %s\n", Rver,
                all.equal(qnrm, qnrm405, tolerance=0), identical(qnrm, qnrm405)))
    stopifnot(all.equal(qnrm, qnrm405, tolerance = 1e-12))
} else if(getRversion() < "4.3") { # our qnormR(*, version="2020-10-17") matches:
    cat(sprintf("%s, \"4.1.0\",\n   all.equal(*, tol=0): %s;  identical(): %s\n", Rver,
                all.equal(qnrm, qnrm410, tolerance=0), identical(qnrm, qnrm410)))
    stopifnot(all.equal(qnrm, qnrm410, tolerance = 1e-12))
} else { # R version >= 4.3.x
    cat(sprintf("%s, >= 4.3.x,\n   all.equal(*, tol=0): %s;  identical(): %s\n", Rver,
                all.equal(qnrm, qnrm43, tolerance=0), identical(qnrm, qnrm43)))
    rE6 <- qnorm(-1e6, log.p=TRUE)/-1414.2077829910174  - 1
    cat(sprintf("  rE(-1e6) = %g\n", rE6))
    if(abs(rE6) < 7e-16) # have R-devel with new 2022 code:
        stopifnot(all.equal(qnrm, qnrm43, tolerance = 1e-14))
}


###################################################
### code chunk number 3: p-mar (eval = FALSE)
###################################################
## par(mar = c(3.6, 3.8, 1, .1), mgp = c(2.5, .75, 0))


###################################################
### code chunk number 4: p-mar0 (eval = FALSE)
###################################################
## par(mar = c(3.5, 3.8, 0, .1), mgp = c(2.5, .75, 0))


###################################################
### code chunk number 5: qn-plot-def (eval = FALSE)
###################################################
## plot(qnrm405 ~ s, type="l", log="xy", col=2, ylim = c(1, max(qs)), asp = 1,
##      xaxt="n", yaxt="n"); require("sfsmisc"); eaxis(1); eaxis(2)
## lines(qs ~ s, col=(c4 <- adjustcolor(4, 1/4)), lwd=4)
## legend("top", c("qnorm(-s, lower.tail=FALSE, log.p=TRUE)", "true"),
##        col=c(palette()[2], c4), lwd=c(1,4), bty="n")


###################################################
### code chunk number 6: qn-plot-do
###################################################
local({ # larger range for s -- qnorm-extreme-bad.R.~1~ (Sep 25, 2020):
 qs <- 2^seq(0, 70, by=1/8)
 s <- -(lp <- pnorm(qs, lower.tail=FALSE, log.p=TRUE))
 ## with R 4.0.5: qp <- qnorm(pp, lower.tail=FALSE, log.p=TRUE)
 qnrm405 <- qnormR(lp, lower.tail=FALSE, log.p=TRUE, version= "4.0.x")
par(mar = c(3.5, 3.8, 0, .1), mgp = c(2.5, .75, 0))
plot(qnrm405 ~ s, type="l", log="xy", col=2, ylim = c(1, max(qs)), asp = 1,
     xaxt="n", yaxt="n"); require("sfsmisc"); eaxis(1); eaxis(2)
lines(qs ~ s, col=(c4 <- adjustcolor(4, 1/4)), lwd=4)
legend("top", c("qnorm(-s, lower.tail=FALSE, log.p=TRUE)", "true"),
       col=c(palette()[2], c4), lwd=c(1,4), bty="n")
})


###################################################
### code chunk number 7: ablaxis1
###################################################
ablaxis1 <- function(x) { abline(v = x^2, col=4, lty=2)
    axis(1, at=x^2, labels = substitute(X^2, list(X=x)), col=4, col.axis=4, line=-.61, cex.axis=0.75) }


###################################################
### code chunk number 8: p-relErr0 (eval = FALSE)
###################################################
## if(!exists("version.txt"))  version.txt <- R.version.string
## plot(abs(relE_qn) ~ s, type="l", log="xy",
##      main = "inaccuracy of qnorm(-s, log.p=TRUE, lower.tail=F)", axes=FALSE)
## eaxis(1, nintLog = 13, sub10 = 2); eaxis(2); ablaxis1(x=27)
## mtext(version.txt, line = -0.8, cex=.8, adj = 0.75)


###################################################
### code chunk number 9: p-relErr (eval = FALSE)
###################################################
## par(mar = c(3.6, 3.8, 1, .1), mgp = c(2.5, .75, 0))
## if(!exists("version.txt"))  version.txt <- R.version.string
## plot(abs(relE_qn) ~ s, type="l", log="xy",
##      main = "inaccuracy of qnorm(-s, log.p=TRUE, lower.tail=F)", axes=FALSE)
## eaxis(1, nintLog = 13, sub10 = 2); eaxis(2); ablaxis1(x=27)
## mtext(version.txt, line = -0.8, cex=.8, adj = 0.75)


###################################################
### code chunk number 10: relE405
###################################################
relE_qn <- relErrV(qs, qnrm405) ; version.txt <- "R versions up to R 4.0.5"


###################################################
### code chunk number 11: do-p-relE405
###################################################
par(mar = c(3.6, 3.8, 1, .1), mgp = c(2.5, .75, 0))
if(!exists("version.txt"))  version.txt <- R.version.string
plot(abs(relE_qn) ~ s, type="l", log="xy",
     main = "inaccuracy of qnorm(-s, log.p=TRUE, lower.tail=F)", axes=FALSE)
eaxis(1, nintLog = 13, sub10 = 2); eaxis(2); ablaxis1(x=27)
mtext(version.txt, line = -0.8, cex=.8, adj = 0.75)


###################################################
### code chunk number 12: delta-relE
###################################################
delta.relE <- function(q, qNorm = function(...) qnormR(..., version = "4.0.x")) {
  lp <- pnorm(q, lower.tail=FALSE, log.p=TRUE) # <==>  q = true qnorm(lp, *)
  ## the "delta" of the two relative errors qnorm() vs sqrt(2*s) approx:
  abs(1 - qNorm(lp, lower.tail=FALSE, log.p=TRUE) / q) -
  abs(1 - sqrt(-2*lp) / q)
}
plot(delta.relE(qs) ~ qs, subset = 10 < qs & qs < 4e6, type="l", log="x")
abline(h=0, col = adjustcolor(2, 1/2))


###################################################
### code chunk number 13: root-delta-raw
###################################################
cutP. <- uniroot(function(logq) delta.relE(exp(logq)) , c(3, 13))
exp(cutP.$root)


###################################################
### code chunk number 14: root-delta-fine
###################################################
str(cP. <- uniroot(delta.relE, interval = c(1000, 1300), tol = 1e-12))
qC <- cP.$root # 1153.242
(lpC <- pnorm(qC, lower.tail=FALSE, log.p=TRUE))


###################################################
### code chunk number 15: relE410
###################################################
relE_qn <- relErrV(qs, qnrm410); version.txt <- "R 4.1.0 to 4.2.x"


###################################################
### code chunk number 16: do-p-relE410
###################################################
par(mar = c(3.6, 3.8, 1, .1), mgp = c(2.5, .75, 0))
if(!exists("version.txt"))  version.txt <- R.version.string
plot(abs(relE_qn) ~ s, type="l", log="xy",
     main = "inaccuracy of qnorm(-s, log.p=TRUE, lower.tail=F)", axes=FALSE)
eaxis(1, nintLog = 13, sub10 = 2); eaxis(2); ablaxis1(x=27)
mtext(version.txt, line = -0.8, cex=.8, adj = 0.75)
ablaxis1(x=816)


###################################################
### code chunk number 17: qnormAsymp
###################################################
k.s <- 0:5; nks <- paste0("k=", k.s)
qnAsym <- sapply(setNames(k.s, nks), function(k) qnormAsymp(lp=lp, order = k))
relEasym <- apply(qnAsym, 2, relErrV, target = qs) # rel.errors for all


###################################################
### code chunk number 18: p-qnormAsymp (eval = FALSE)
###################################################
## matplot(-lp, abs(relEasym), log="xy", type="l", lwd=2, axes=FALSE, xlab = quote(s == -lp))
## eaxis(1, sub10=2); eaxis(2, sub10=c(-2,2), nintLog=16); grid(col="gray75")
## legend("right", nks, col=1:6, lty=1:5, lwd=2, bty="n")


###################################################
### code chunk number 19: do-qnormAsymp
###################################################
par(mar = c(3.5, 3.8, 0, .1), mgp = c(2.5, .75, 0))
matplot(-lp, abs(relEasym), log="xy", type="l", lwd=2, axes=FALSE, xlab = quote(s == -lp))
eaxis(1, sub10=2); eaxis(2, sub10=c(-2,2), nintLog=16); grid(col="gray75")
legend("right", nks, col=1:6, lty=1:5, lwd=2, bty="n")


###################################################
### code chunk number 20: qnormAsymp-zoom (eval = FALSE)
###################################################
## matplot(-lp, abs(relEasym), log="xy", type="l", lwd=2, axes=FALSE, xlab = quote(s == -lp),
##         xlim = c(40, 1e9), ylim = 10^c(-16, -3))
## eaxis(1, sub10=2); eaxis(2, sub10=c(-2,2), nintLog=16); grid(col="gray80")
## legend(4e7, 1e-9, nks, col=1:6, lty=1:5, lwd=2, bty="n")#, cex=.75, bg=adjustcolor("bisque", 3/4))


###################################################
### code chunk number 21: do-qnAsy-zoom
###################################################
par(mar = c(3.5, 3.8, 0, .1), mgp = c(2.5, .75, 0))
matplot(-lp, abs(relEasym), log="xy", type="l", lwd=2, axes=FALSE, xlab = quote(s == -lp),
        xlim = c(40, 1e9), ylim = 10^c(-16, -3))
eaxis(1, sub10=2); eaxis(2, sub10=c(-2,2), nintLog=16); grid(col="gray80")
legend(4e7, 1e-9, nks, col=1:6, lty=1:5, lwd=2, bty="n")#, cex=.75, bg=adjustcolor("bisque", 3/4))


###################################################
### code chunk number 22: p-relE-x5-zoom
###################################################
absE <- function(e) pmax(abs(e), 2^-54) # = 1/4 eps_c
local({ # larger range for s -- qnorm-extreme-bad.R.~1~ (Sep 25, 2020):
  qs <- seq(27, 40, by=1/256)
  lp <- pnorm(qs, lower.tail=FALSE, log.p=TRUE)
  s <- -lp # = -pnorm(..) = -log(1 - Phi(qs)) > 0
  qnrm43  <- qnormR(-s, lower.tail=FALSE, log.p=TRUE, version= "2022")
  relEq43 <- relErrV(qs, qnrm43)
par(mar = c(3.6, 3.8, 1, .1), mgp = c(2.5, .75, 0))
  par(mar=c(2, par("mar")[-1]))
  plot(absE(relEq43) ~ s, type="l", log="xy", ylim = 10^c(-16.3,-15.05), col=adjustcolor(1, 1/2), axes=FALSE)
  qnAsym5 <- qnormAsymp(lp=lp, order = 5)
  relE5 <- relErrV(qs, qnAsym5)
  lines(absE(relE5) ~ s, col=adjustcolor(2, 1/2),  lwd = 2)
  ## smoothing "discrete" relative errors:
  lines(smooth.spline(s, abs(relEq43)), col=adjustcolor(1, 2/3), lwd=4, lty="6132")
  lines(smooth.spline(s, abs(relE5)  ), col=adjustcolor(2, 2/3), lwd=4, lty="72")
  ## nice axes etc
  axis(1, at=c(17,19,20)^2)
  eaxis(1, nintLog = 16, sub10 = 2); eaxis(2); ablaxis1(27)
  r. <- c(17,19:23,25,27, 30, 35, 40) ;        ablaxis1(21.5); ablaxis1(22.8)
  axis(3, at=r.^2, label=r., col=4, col.axis=4, line=-.5, cex.axis=.75)
  axis(3, at=19.5^2, label=quote(r == {}), col=4, col.axis=4, col.ticks=NA, cex.axis=1.4, line=-.5)
  if(FALSE) {## visually inspect "table" values
    cbind(qs,  r = sqrt(s), s, relE5, relEq43)[21^2 <= s & s <= 27^2, ]
   cbind(qs, r=sqrt(s), s)[ which(abs(relE5) > 0),]  # very last one is r = 23.9081
  }
})


###################################################
### code chunk number 23: relE43
###################################################
relE_qn <- relErrV(qs, qnrm43)


###################################################
### code chunk number 24: relE43-tab
###################################################
table(2^52 * relE_qn)           # all in [-2.5, 3]
table(2^52 * relE_qn[s > 27^2]) #     in [-1,   1]


###################################################
### code chunk number 25: do-p-relE43 (eval = FALSE)
###################################################
## version.txt <- "R > 4.2.x (after 2022)"
## par(mar = c(3.6, 3.8, 1, .1), mgp = c(2.5, .75, 0))
## if(!exists("version.txt"))  version.txt <- R.version.string
## plot(abs(relE_qn) ~ s, type="l", log="xy",
##      main = "inaccuracy of qnorm(-s, log.p=TRUE, lower.tail=F)", axes=FALSE)
## eaxis(1, nintLog = 13, sub10 = 2); eaxis(2); ablaxis1(x=27)
## mtext(version.txt, line = -0.8, cex=.8, adj = 0.75)


###################################################
### code chunk number 26: sessionInfo
###################################################
toLatex(sessionInfo(), locale=FALSE)


###################################################
### code chunk number 27: DPQ-version
###################################################
unlist(packageDescription("DPQ")[c("Package", "Version", "Date")])


###################################################
### code chunk number 28: relErrV-def
###################################################
## Componentwise aka "Vectorized" relative error:
## Must not be NA/NaN unless one of the components is  ==> deal with {0, Inf, NA}
relErrV <- function(target, current, eps0 = .Machine$double.xmin) {
    n <- length(target <- as.vector(target))
    ## assert( <length current> is multiple of <length target>) :
    lc <- length(current)
    if(!n) {
        if(!lc) return(numeric()) # everything length 0
        else stop("length(target) == 0 differing from length(current)")
    } else if(!lc)
        stop("length(current) == 0 differing from length(target)")
    ## else n, lc  > 0
    if(lc %% n)
        stop("length(current) must be a multiple of length(target)")
    recycle <- (lc != n) # explicitly recycle
    R <- if(recycle)
             target[rep(seq_len(n), length.out=lc)]
         else
             target # (possibly "mpfr")
    R[] <- 0
    ## use *absolute* error when target is zero {and deal with NAs}:
    t0 <- abs(target) < eps0 & !(na.t <- is.na(target))
    R[t0] <- current[t0]
    ## absolute error also when it is infinite, as (-Inf, Inf) would give NaN:
    dInf <- is.infinite(E <- current - target)
    R[dInf] <- E[dInf]
    useRE <- !dInf & !t0 & (na.t | is.na(current) | (current != target))
    R[useRE] <- (current/target)[useRE] - 1
    ## preserve {dim, dimnames, names}  from 'current' :
    if(!is.null(d <- dim(current)))
        array(R, dim=d, dimnames=dimnames(current))
    else if(!is.null(nm <- names(current)) && is.null(names(R))) # not needed for mpfr
        `names<-`(R, nm)
    else R
}


###################################################
### code chunk number 29: def-r-cutoffs
###################################################
r0 <- c(27, 55, 109, 840, 36000, 6.4e8) # <-- cutoffs  <--> in ../R/norm_f.R
# use k =  5   4    3    2      1       0    e.g.  k = 0  good for r >= 6.4e8


###################################################
### code chunk number 30: do-p.qnormAsy2 (eval = FALSE)
###################################################
## r0 <- c(27, 55, 109, 840, 36000, 6.4e8) # <-- cutoffs  <--> in ../R/norm_f.R
## # use k =  5   4    3    2      1       0    e.g.  k = 0  good for r >= 6.4e8
## for(ir in 2:length(r0)) {
##   p.qnormAsy2(r0[ir], k = 5 +2-ir) # k = 5, 4, ..
##   if(interactive() && ir < length(r0)) {
##        cat("[Enter] to continue: "); cat(readLines(stdin(), n=1), "\n") }
## }


###################################################
### code chunk number 31: p.qnormAsy2-def
###################################################
## Zoom into each each cut-point region :
p.qnormAsy2 <- function(r0, k, # use k-1 and k in region around r0
                        n = 2048, verbose=TRUE, ylim = c(-1,1) * 2.5e-16,
                        rr = seq(r0 * 0.5, r0 * 1.25, length = n), ...)
{
  stopifnot(is.numeric(rr), !is.unsorted(rr), # the initial 'r'
            length(k) == 1L, is.numeric(k), k == as.integer(k), k >= 1)
  k.s <- (k-1L):k; nks <- paste0("k=", k.s)
  if(missing(r0)) r0 <- quantile(rr, 2/3)# allow specifying rr instead of r0
  if(verbose) cat("Around r0 =", r0,";  k =", deparse(k.s), "\n")
  lp <- (-rr^2) # = -r^2 = -s  <==> rr = sqrt(- lp)
  q. <- qnormR(lp, lower.tail=FALSE, log.p=TRUE, version="2022-08")# *not* depending on R ver!
  pq <- pnorm (q., lower.tail=FALSE, log.p=TRUE) # ~= lp
  ## the arg of pnorm() is the true qnorm(pq, ..) == q.  by construction
  r <- sqrt(- pq)
  stopifnot(all.equal(rr, r, tol=1e-15))
  qnAsy <- sapply(setNames(k.s, nks), function(ord)
                  qnormAsymp(pq, lower.tail=FALSE, log.p=TRUE, order=ord))
  relE <- qnAsy / q. - 1
  m <- cbind(r, pq, relE)
  if(verbose) {
    print(head(m, 9)); for(j in 1:2) cat(" ..........\n")
    print(tail(m, 4))
  }
  ## matplot(r, relE, type = "b", main = paste("around r0 = ", r0))
  matplot(r, relE, type = "l", ylim = ylim,
     main = paste("Relative error of qnormAsymp(*, k) around r0 = ", r0,
                  "for  k =", deparse(k.s)),
     xlab = quote(r == sqrt(-log(p))), ...)
  legend("topleft", nks, horiz = TRUE, col=1:2, lty=1:2, bty="n", lwd=2)
  for(j in seq_along(k.s))
    lines(smooth.spline(r, relE[,j]), col=adjustcolor(j, 2/3), lwd=4, lty="6132")
  cc <- "blue2"; lab <- substitute(r[0] == R, list(R = r0))
  abline(v  = r0, lty=2, lwd=2, col=cc)
  axis(3, at= r0, labels=lab, col=cc, col.axis=cc, line=-1)
  abline(h = (-1:1)*.Machine$double.eps, lty=c(3,1,3),
         col=c("green3", "gray", "tan2"))
  invisible(cbind(r = r, qn = q., relE))
}


###################################################
### code chunk number 32: plot-qnormAsy2
###################################################
sfsmisc::mult.fig(5, main = "qnormAsymp(*, k) approximations in the 5 cutpoint regions")
r0 <- c(27, 55, 109, 840, 36000, 6.4e8) # <-- cutoffs  <--> in ../R/norm_f.R
# use k =  5   4    3    2      1       0    e.g.  k = 0  good for r >= 6.4e8
for(ir in 2:length(r0))
    p.qnormAsy2(r0[ir], k = 5 +2-ir, n = 1024, verbose=FALSE, cex.main = .90)


###################################################
### code chunk number 33: finalizing
###################################################
options(op.orig)


