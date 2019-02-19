\name{acto}
\alias{acto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot an actogram of a target behaviour
%%  ~~function to do ... ~~
}
\description{Plots the presents or absence of one or more target behaviours in an actogram style on a daily basis
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
acto(data = ... , time = "..." , behaviour = "..." , target.bev = ... , daily = FALSE , night.shift = FALSE , sun = F , suncolor = "red" , timezone = "UTC" , long = ... , lat = ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data frame containing the timestamps and the corresponding behaviour}
  \item{time}{column name of the timestamp in " "

  the date format needs to be in year-month-day hour-month-minute}
  \item{behaviour}{column name of the behaviour

  can be numeric, a factor or a character}
  \item{target.bev}{specify or or more behaviours from the behaviour column to display

  for more than one behaviour this needs to be a vector(c())}
  \item{daily}{set to TRUE if every day should be marked on the y-axis

  by default only the first day of the month is marked on the axis}
  \item{night.shift}{set to TRUE if the plot shall center around midnight

  by default the plot centers around midday}
  \item{sun}{set to TRUE if sunset and sunrise lines shall be added to the plot

  default is set to FALSE - see also details}
  \item{suncolor}{specify the color of the sunset/sunrise lines put in "..." (uses ggplot color codes)

  default is red}
  \item{timezone}{specify the timezone in "..."

  default is "UTC"}
  \item{long}{Longitude coordinate - needed for the calculation of the sunset/sunrise}
  \item{lat}{Latitude coordinate - needed for the calculation of the sunset/sunrise}
}
\details{This function assumes that the acceleration is sampled with short intervals between recordings. However the times for the sunset and sunrise are reported in a 1 minute resolution. If acceleration measurment intervall are further apart from each other than one minute there will inevetably sunset or sunrise times where no coresponding acceleration data exists. For this reason the sunset/sunrise times will be adjusted to the nearest acceleration measurement time. This could however lead to distorted sun lines if measuring intervals are to far apart.
%%  ~~ If necessary, more details than the description above ~~
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

\seealso{acto.odba
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data("acto.data")

acto.odba(data = acto.data , time = "timestamp" , behaviour = "behaviour" , target.bev = c(1))
}
