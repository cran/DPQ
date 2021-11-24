### R code from vignette source 'log1pmx-etc.Rnw'

###################################################
### code chunk number 1: require
###################################################
require(DPQ)


###################################################
### code chunk number 2: plot_D0tilde
###################################################
par(mar = 0.1 + c(2.5, 3, 0, 0), mgp = c(1.5, 0.6, 0), las=1)
curve(x*log(x)+1-x, 1e-7, 6, n=1001, col=2, lwd=2,
      panel.first=grid(), xlab=quote(u), ylab="")
mtext(quote(tilde(D[0])(u) == ~ u %.%~ log(u)+1-~u), line=-2, col=2)
abline(a = 1-exp(1), b=1, col=adjustcolor(4, 3/4), lwd=1.5, lty=2)
text(5, 2.5, quote(1%.%u - e+1), col=4)
axis(1, at=exp(1), quote(e), tck=0.2, col=4, col.axis=4, lty=3)


###################################################
### code chunk number 3: p.l1p1-def
###################################################
p.p1l1 <- function(from, to, ylim=NULL, cS = adjustcolor(6, 1/2),
                   n=1024, do.leg=TRUE) {
    stopifnot(is.numeric(from), is.numeric(to),
              is.character(cS), length(cS) == 1)
    cols <- palette()[c(2,4, 6, 3,5)]; cols[3] <- cS
    c1 <- curve(x*log1p(x), from=from, to=to, n=n,
                col=2, ylab="", ylim=ylim,
                panel.first = abline(h=0, v=0, lty=3, lwd=1/2))
    c2 <- curve(log1pmx(x), add=TRUE, n=n, col=4)
    with(c1, {
        lines(x, y+c2$y, col=cS, lwd=3)
        lines(x, x^2/2          , col=3, lty=2)
        lines(x, x^2/2*(1 - x/3), col=5, lty=4)
    })
    if(do.leg) {
        labexpr <- expression(
            x %.% log1p(x), log1pmx(x),
            p1l1(x) == log1pmx(x) + x %.% log1p(x),
            x^2 / 2, # frac(x^2, 2)  # is too large
            x^2 / 2 %.% (1 - x/3))
        legend("top", labexpr, col = cols,
               lwd=c(1,1,3,1,1), lty=c(1,1,1:2,4), bty="n")
    }
}


###################################################
### code chunk number 4: l1p1-curves
###################################################
par(mfcol=1:2, mar = 0.1 + c(2.5, 3, 1, 2), mgp = c(1.5, 0.6, 0), las=1)
p.p1l1(-7/8, 2, ylim = c(-1,2))
zoomTo <- function(x,y=x, tx,ty){ arrows(x,-y, tx, ty)
                                    text(x,-y, "zoom in", adj=c(1/3,9/8)) }
zoomTo0 <- function(x,y=x) zoomTo(x,y, 0,0)
zoomTo0(.3)
p.p1l1(-1e-4, 1.5e-4, ylim=1e-8*c(-.6, 1), do.leg=FALSE)


###################################################
### code chunk number 5: log1pmx-curves
###################################################
lcurve <- function(Fn, a,b, ylab = "", lwd = 1.5, ...)
    plot(Fn, a,b, n=1001, col=2, ylab=ylab, lwd=lwd, ...,
         panel.last = abline(h=0, v=-1:0, lty=3))

par(mfrow=c(2,2), mar = 0.1 + c(2.5, 3, 1, 2), mgp = c(1.5, 0.6, 0), las=1)
lcurve(log1pmx, -.9999, 7, main=quote(log1pmx(x) == log(1+x)-x))
                            rect(-.1,  log1pmx(-.1  ), .1  , 0); zoomTo0(1/2, 1)
lcurve(log1pmx, -.1,  .1 ); rect(-.01, log1pmx(-.01 ), .01 , 0); zoomTo0(.02, .001)
lcurve(log1pmx, -.01, .01); rect(-.002,log1pmx(-.002), .002, 0); zoomTo0(2e-3,1e-5)
lcurve(\(x) -log1pmx(x), -.002, .002, log="y", yaxt="n") -> l1r
sfsmisc::eaxis(2); abline(v=0, lty=3)
d1r <- cbind(as.data.frame(l1r), y.naive = with(l1r, -(log(1+x)-x)))
c4 <- adjustcolor(4, 1/3)
lines(y.naive ~ x, data=d1r, col=c4, lwd=3, lty=2)
legend("left", legend=expression(- log1pmx(x), -(log(1+x)-x)),
       col=c(palette()[2],c4), lwd=c(1,3), lty=1:2, bty="n")


###################################################
### code chunk number 6: log1pmx-naive-error
###################################################
par(mfrow=1:2, mar = 0.1 + c(2.5, 3, 1, 2), mgp = c(1.5, 0.6, 0), las=1)
d1r[, "relE.naive"] <- with(d1r, sfsmisc::relErrV(y, y.naive))
plot(relE.naive ~ x, data=d1r, type="l", ylim = c(-1,1)*1e-6)
y2 <- 1e-8
rect(-.002, -y2, .002, y2, col=adjustcolor("gray",1/2), border="transparent")
zoomTo(15e-4, 9*y2, 13e-4, -y2)
plot(relE.naive ~ x, data=d1r, type="l", ylim = c(-1,1)*y2); abline(h=0,lty=3)


