\name{like.val}
\alias{like.val}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate likelihood for a prediction to be the expected behaviour
%%  ~~function to do ... ~~
}
\description{This function compares the predicted behaviour of a machine learning algorithm to the expected behaviour. It                 calculates the proportion of predictions that match the expectation in relation to the total number this behaviour was predicted.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
like.val(data,algorithm,goal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data.frame that was the output from either pre.cross or pre.comp with known goals}
  \item{algorithm}{name of the algorithm in CAPITAL letters or Majority put in quotes " "}
  \item{goal}{name of the goal column in the prediction results data.frame put in quotes " "}
%%     ~~Describe \code{x} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{The output will be a vector of likelihood values.
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
