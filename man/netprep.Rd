\name{net.prep}
\alias{net.prep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transforms eobs acceleration raw data into an array that can be used in a keras neural network
%%  ~~function to do ... ~~
}
\description{To train or to predict behaviours from raw eobs acceleration data it needs to be transformed into an array with a specific form.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
net.prep(data =  ... , time = "..." , burst = ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data.frame containing original movebank data}
  \item{time}{name of the cloumn holding the timestamps - default is "timestamp"}
  \item{burst}{number of measurments per burst}
%%     ~~Describe \code{x} here~~
}
\details{So far this function only works for 3 dimentional data. Predicting or training a keras neural network was never tested with less than 3 axes.

The axes in the raw data.frame have to be named x, y and z.
%%  ~~ If necessary, more details than the description above ~~
}
\value{The output will be an array with dimentions of n,burst,1,3. Where n is the total number of bursts recorded and burst is the number of measurments taken per burst.
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
