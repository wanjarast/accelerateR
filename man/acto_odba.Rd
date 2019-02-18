\name{acto.odba}
\alias{acto.odba}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot an actogram based on the ODBA
%%  ~~function to do ... ~~
}
\description{Plots the overall activity of the focal animal based on the ODBA at every timestamp. The plot can center around midday or midnight.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
acto.odba(data = ... , time = "..." , ODBA = "ODBA" , cutoff = 1 , night.shift = FALSE , sun = F , suncolor = "red" , timezone = "UTC" , long = ... , lat = ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data frame containing timestamps and ODBA values}
  \item{time}{column name of the timestamp in " "

  the date format needs to be in year-month-day hour-month-minute}
  \item{ODBA}{colum name of the ODBA values put in " "

  by default this is "ODBA"}
  \item{cutoff}{set a percentile to cap the highest ODBA value

  higher values will be lowered to the set percentile. The default is 1 were nothing is cutoff}
  \item{night.shift}{set to TRUE if the plot shall center around midnight

  by default the plot centers around midday}
  \item{sun}{set to TRUE if sunset and sunrise lines shall be added to the plot

  default is set to FALSE}
  \item{suncolor}{specify the color of the sunset/sunrise lines put in "..." (uses ggplot color codes)

  default is red}
  \item{timezone}{specify the timezone in "..."

  default is "UTC"}
  \item{long}{Longitude coordinate}
  \item{lat}{Latitude coordinate}
}
\details{In cases where it is expected to see very high acceleration values du to the animal manipulating the tag directly, it could be considered to cut off the highest ODBA values by setting the cutoff value.
}
\value{
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

\seealso{acto
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data("acto.data")

acto.odba(data = acto.data , time = "timestamp" , ODBA = "ODBA")
}

