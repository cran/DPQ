\name{hyper2binomP}
\alias{hyper2binomP}
\title{Transform Hypergeometric Distribution Parameters to Binomial Probability}
\description{
  Transform the three parameters of the hypergeometric distribution
  function to the probability parameter of the \dQuote{corresponding} binomial
  distribution.
}
\usage{
hyper2binomP(x, m, n, k)
}
\arguments{
  \item{x, m, n, k}{see \code{\link{dhyper}}.}
}
\value{
  a number, the binomial probability.
}
\references{See those in \code{\link{phyperBinMolenaar}}.}
\seealso{
  \code{\link{phyper}},
  \code{\link{pbinom}}.

  \code{\link{dhyperBinMolenaar}()}, \code{\link{phyperBinMolenaar.1}()},
  \code{*.2()}, etc, all of which are crucially based on \code{hyper2binomP()}.
}
\examples{
hyper2binomP(3,4,5,6) # 0.38856

## The function is simply defined as
function (x, m, n, k) {
    N <- m + n
    p <- m/N
    N.n <- N - (k - 1)/2
    (m - x/2)/N.n - k * (x - k * p - 1/2)/(6 * N.n^2)
 }
}
\keyword{distribution}
\keyword{utilities}
