\name{sum.data}
\alias{sum.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summary statistics from e-obs raw acceleration data
%%  ~~function to do ... ~~
}
\description{Calculate summary statistics from e-obs raw acceleration data. Statistical variables composition can be chosen                at will. Read the Details for the necessary preparetions of the raw data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{sum.data(data = ... , time = "..." , windowstart = 1 , burstcount = NULL , x = "..." , y = NULL , z = NULL ,  IntDur = NA , ID = NA , Tag_ID = NA , frequency = NA ,
          sex = NA , stats , bev = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data.frame of raw acceleration e-obs data exported without subseconds (for necessary format see Details.)}
  \item{time}{column where the timestamps of each burst are stored - column name needs to be put in " "}
  \item{windowstart}{in case a sliding window is used within the bursts this parameter set the start of the window,
  can be used in a loop to shift the starting value of the window}
  \item{IntDur}{duration of a single burst in seconds(only needed when the weigthed mean should be calculated (functional but not advised))}
  \item{burstcount}{expected number of measurments per burst per axis (needed for Fast Fourier Transformation), also necessary for the sliding window approach to define window length}
  \item{x, y, z}{column in which the acceleration measurments for the axis are stored - column name needs to be put in    " "}
  \item{ID}{ID the the focal animal (not needed for calculation)}
  \item{Tag_ID}{ID of the tag (not needed for calculation)}
  \item{frequency}{samples frequency for a single axis in single burst (not needed for calculation)}
  \item{sex}{sex of the focal animal (not needed for calculation)}
  \item{stats}{vector of statiscial variables that should be calculated (for possible input see Details)}
  \item{behaviour}{if the raw data set has a column containing behaviour labels it can be named here and the labels will be added to the output data frame.}

%%     ~~Describe \code{x} here~~
}
\details{This function will prepare the raw data from an e-obs acceleration tag for the use with machine learning. When the data from the tag is extracted from the logger.bin file it has to be in the format without subseconds. Every row of data belonging to the same timestamp need to have the same value in the time column.

To avoide confuison were data for one time of the day is recorded on several day the columns of the date and time have to be combied in one column. The name of that cloumn has to be specified as the time argument in the function.

The coloumn corresponding to the x-,y-,and z-axis can be named at will. The coloumn names of every axis have to be specified. By default calculations for the y and z axis are disabled. In cases where there are 2 or 3 axes measured there names can be included. The predictors q, Pitch, and Roll are dependend on all three axes so they will not be calculated for data set with only  or 2 axes. The cloumn names have to be put in quotes to be recognised.

The arguments ID, Tag.ID, frequency and sex will create additional columns with supporting information on the focal animal. This implies that the raw acceleration data only represents a single animal. Those information can be left out if not needed or unknown.

The stats argument provides a handle to choose predictors that will be calculated for the model. Possible inputs are:

          "mean" for the mean of x, y and z

          "sd" for the standard deviation of x, y and z

          "Var" for the variance (1/N) of x, y and z

          "CV" for the coefficient of variation of x, y and z

          "ICV" for the inverse coefficient of variation of x, y and z

          "q" for the square root of the sum of squares of x, y and z

          "Pitch" for rotation of up and down

          "Roll" for the rotation from side to side

          "Kurtosis" for the kurtosis of x, y and z

          "Skewness" for the skewness of x, y and z

          "ODBA" for the overall dynamic body acceleration for all 3 axes

          "FFT" for adding the complete fast fourier spectrum to the predictor set, if this is used a burstcount has to be provided with burstcount = ...

This function can be used for a sliding window approach. For this the function can be put in a loop with the windowstart parameter set to i. The burstcount parameter will set the size of the window. The summary statistics will be calculated seperatly for every window fragment.

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
\author{Wanja Rast
%%  ~~who you are~~
}
\note{The weighted mean was inspired by Anne Berger
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
acceleration <- data.frame(time = rep(seq(5),each=20) , x = runif(n = 100,min = 1900,max=2100) ,
y = runif(n = 100,min = 2100,max=2300) , z = runif(n = 100,min = 1800,max=2000))

sumstats <- sum.data(data=acceleration , time="time" , x="x" ,
y="y" , z="z" , stats=c("mean" , "sd" , "Var"))
}

