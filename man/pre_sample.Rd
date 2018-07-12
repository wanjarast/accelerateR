\name{pre.sample}
\alias{pre.sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sample summary statistics
%%  ~~function to do ... ~~
}
\description{Pick a set amount of random samples of certain behaviours from the summary statistics output data.frame from                 sum.data().
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pre.sample(predictors , behaviours , precount)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{predictors}{the output data.frame of sum.data(). The behaviour column needs to be a factor.}
  \item{behaviours}{vector of behaviours the should be included in the model}
  \item{precount}{desired amount of samples per behaviour}
%%     ~~Describe \code{x} here~~
}
\details{This function will create a model data set. Only summary statics that correspond to the behaviours chosen for the           analysis are sampled from the full data set (output of sum.data()). The sample size for every behaviour can be             chosen at will but every behaviour will have the same sample size.
%%  ~~ If necessary, more details than the description above ~~
}
\value{The output will be a data.frame and a subset of the full data set (output of sum.data()).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Wanja Rast
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
