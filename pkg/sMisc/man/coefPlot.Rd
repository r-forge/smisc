\name{coefPlot}
\alias{coefPlot}
\alias{defaultInterval}
\alias{waldCi}

\title{
Compare the coefficients of different model fits
}
\description{
blablabla
}
\usage{
coefPlot(..., var.names = NULL, log.y = FALSE, exclude = NULL, extractor.funcs = NULL)
}
\arguments{
  \item{\dots}{
fitted model objects}
\item{var.names}{
vector with variables that are to be compared}
\item{log.y}{
is the y-axis to be logged?
}
\item{exclude}{
vector with variables that are to be excluded
}
\item{extractor.funcs}{
list with functions that will extract the coefficients and the intervals from the model objects
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An array
}
\references{
None
}
\author{
Sebastian Gerdes}
\note{
No notes
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#library("faraway")
fm1 <- lm(glucose ~ ., pima)
fm2 <- update(fm1, ~. - pregnant)
coefPlot(fm1, fm2)
coefPlot(fm1, fm2, exclude = c("(Intercept)", "test", "diabetes"))

fm1 <- glm(test ~ ., binomial, pima)
(coefs <- coef.plot(fm1, fm1, exclude = c("(Intercept)", "triceps", "pregnant", "test", "diabetes"), extractor.funcs = list(fm1 = default.interval, fm1 = wald.interval)))
coefs
aperm(coefs, c(1, 3, 2))
coefs[,"Upper",]
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
