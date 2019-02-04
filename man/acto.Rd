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
acto(data = ... , time = "..." , date = "..." , behaviour = "..." , target.bev = ... , daily = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data frame containing the timestamps and the corresponding behaviour}
  \item{time}{column name of the time of day in " "}
  \item{date}{column name of the date in " "

  the date format needs to be in year-month-day}
  \item{behaviour}{column name of the behaviour

  can be numeric, a factor or a character}
  \item{target.bev}{specify or or more behaviours from the behaviour collumn to display

  for more than one behaviour this needs to be a vector(c())}
  \item{daily}{set to TRUE if every day should be marked on the y-axis

  by default only the first day of the month is marked on the axis}
}
\details{
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

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data("acto.data")

acto(data = acto.data , time = "time" , date = "date" , behaviour = "behaviour" , target.bev = c(1))
}
