\name{move_trans}
\alias{move_trans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Movebank transformation
%%  ~~function to do ... ~~
}
\description{Transforms acceleration data downloaded from movebank into a usable format.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
move_trans(file =  NULL , object = NULL , time = "timestamp" , acc = "eobs:accelerations-raw" ,
           id = "individual-local-identifier" , sample_fequency = "eobs:acceleration-sampling-frequency-per-axis"
           , naxes = 3, no_cores = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{object name of the Movebank ACC data, choose either this or file, in case data are not already read into R}
  \item{path}{file path to the downloaded csv file, choose either this or object, in case the object was read into R already}
  \item{timestamp}{name of the column holding the timestamps - default is "timestamp"}
  \item{acc}{name of the column holding the name for the raw acc data - default is "eobs:accelerations-raw"}
  \item{id}{name of the column holding the individual id - default is "individual-local-identifier"}
  \item{sample_fequency}{name of the column holding the sample frequency -
                        default is eobs:acceleration-sampling-frequency-per-axis"}
  \item{naxes}{number of axes recorded - default is 3 axes}
  \item{no_cores}{set the number of cores for parallel processing - default is 1(no parallel processing)}
%%     ~~Describe \code{x} here~~
}
\details{In cases where the data was recorded with multiple settings that are stored all in one file, the function will output a warning with information on the burst sizes of the other setting. The data from other settings will be ignored in the transformation. They need to be transformed seperatly.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A data.table with the acceleration data, timestamps, ids and sample frequencies in sperate columns. Rows represent single measuremnts. An additional columns that informs on the length of each burst is added.
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
