\name{sum.data}
\alias{sum.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary statistics from e-obs raw acceleration data
%%  ~~function to do ... ~~
}
\description{Calculate summary statistics from e-obs raw acceleration data. Statistical variables composition can be chosen                at will. Read the Details for the necessary preparetions of the raw data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{sum.data(data = ... , time = "..." , IntDur=NA, x = "..." , y=NULL , z=NULL , ID=NA , Tag.ID=NA , frequency=NA ,
          sex=NA , stats)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data.frame of raw acceleration e-obs data exported without subseconds (for necessary format see Details.)}
  \item{time}{column where the timestamps of each burst are stored - cloumn name needs to be put in " "}
  \item{IntDur}{duration of a single burst in seconds(only needed when the weigthed mean should be calculated)}
  \item{x, y, z}{column in which the acceleration measurments for the axis are stored - column name needs to be put in    " "}
  \item{ID}{ID the the focal animal (not needed for calculation)}
  \item{Tag.ID}{ID of the tag (not needed for calculation)}
  \item{frequency}{samples frequency for a single axis in single burst (not needed for calculation)}
  \item{sex}{sex of the focal animal (not needed for calculation)}
  \item{stats}{vector of statiscial variables that should be calculated (for possible input see Details)}

%%     ~~Describe \code{x} here~~
}
\details{This function will prepare the raw data from an e-obs acceleration tag for the use with machine learning. When the data from the tag is extracted from the logger.bin file it has to be in the format without subseconds. Every row of data belonging to the same timestamp need to have the same value in the time column.

To avoide confuison were data for one time of the day is recorded on several day the columns of the date and time     have to be combied 1 column. The name of that cloumn has to be specified as the time argument in the function.

The coloumn corresponding to the x-,y-,and z-axis can be named at will. The coloumn names of every axis have to be specified. By default calculations for the y and z axis are disabled. In cases where there are 2 or 3 axes measured there names can be included. The predictors q, Pitch, and Roll are dependend on all three axes so they will not be calculated for data set with only  or 2 axes. The cloumn names have to be put in quotes to be recognised.

The arguments ID, Tag.ID, frequency and sex will create additional columns with supporting information on the focal animal. This implies that the raw acceleration data only represents a single animal. Those information can be left out if not needed or unknown.

The stats argument provides a handle to choose predictors that will be calculated for the model. Possible inputs are:

          "mean" for the mean of x, y and z

          "sd" for the standard deviation of x, y and z

          "Wmean" for a weighted mean of the periodic components of x, y and z (IntDur has to be provided)

          "Var" for the variance (1/N) of x, y and z

          "ICV" for the inverse coefficient of variation of x, y and z

          "q" for the square root of the sum of squares of x, y and z

          "Pitch" for rotation of up and down

          "Roll" for the rotation from side to side

          "Kurtosis" for the kurtosis of x, y and z

          "Skewness" for the skewness of x, y and z

          "ODBA" for the overall dynamic body acceleration for all 3 axes
%%  ~~ If necessary, more details than the description above ~~
}
\value{The output will be a data.frame with the summary statistics for a single time stamp in one row.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Wanja Rast, Anne Berger
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
acceleration <- data.frame(time = rep(seq(5),each=20) , x = runif(n = 100,min = 1900,max=2100) ,
y = runif(n = 100,min = 2100,max=2300) , z = runif(n = 100,min = 1800,max=2000))

sumstats <- sum.data(data=acceleration , time="time" , IntDur=20 , x="x" ,
y="y" , z="z" , stats=c("mean" , "sd" , "Wmean"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
