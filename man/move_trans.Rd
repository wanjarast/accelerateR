\name{move.trans}
\alias{move.trans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Movebank transformation
%%  ~~function to do ... ~~
}
\description{Transforms acceleration data downloaded from movebank into a usable format.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
move.trans(data =  ... , time = "..." , acc = "..." , burst = ... , naxes = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data.frame containing original movebank data}
  \item{time}{name of the column holding the timestamps - default is "timestamp"}
  \item{acc}{name of the column holding the name for the raw acc data - default is "eobs.accelerations.raw"}
  \item{burst}{number of measurments per burst}
  \item{naxes}{number of axes recorded - default is 3 axes}
%%     ~~Describe \code{x} here~~
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{The output will be a data.frame with acceleration data in the same format as produced by the e-obs decoder.
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
