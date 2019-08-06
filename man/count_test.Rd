\name{count.test}
\alias{count.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test wether burst are lacking measurements
%%  ~~function to do ... ~~
}
\description{The function will test the imported raw data from an e-obs acceleration tag are missing any measurments within a burst.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
count_test(data = ... , burstcount = ... , time = "timestamp" , remove = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{imported raw data from e-obs acceleration tag}
  \item{burstcount}{expected number of measurments for one burst as set previous to tag deployment}
  \item{time}{column name of the column that stores the timestamp. Default is "timestamp".}
  \item{remove}{indicating if the faulty timestamps should be removed from the data set.}
%%     ~~Describe \code{x} here~~
}
\details{In some cases there are burst that miss some measurments. This function will check wether all burst have the                expected amount of measurments. If can either report the faulty timestamps or automaticly remove all data from faulty timestamps.
%%  ~~ If necessary, more details than the description above ~~
}
\value{The output will be a vector with either TRUE or FALSE in every row.
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
