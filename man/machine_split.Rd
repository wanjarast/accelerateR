\name{machine.split}
\alias{machine.split}
\title{Split data into training, validation and test data sets
%%  ~~function to do ... ~~
}
\description{Splits the data into three data sets of custom size. Lable proportions in the original data set will be repsected for all three data sets.
}
\usage{
machine.split(data = predictors , group = "timestamp" , train.size = 0.6 , val.size = 0.2 , names = c("train.data","val.data","test.data"))
}
\arguments{
  \item{data}{Complete data set that will be split in three sub sets
}
  \item{group}{Variable that identifies data belonging to the same group. Usually timestamp in acc behaviour predictions is the default.
}
  \item{train.size}{Proportion of data that will create the training data. Default is 0.6.
}
  \item{val.size}{Proportion of data that will create the validation data. Default is 0.2.
}
  \item{names}{Vector of names for the three data sets. Default is c("train.data","val.data","test.data")
}
}
\details{
This function should be run without saving into an object. The resulting subsets will be created from within the function. The sizes of the sets are free to choose. the test data set will be of size 1- train.size - val.size. In case train.size and val.size add up to 1 no test data will be created.
All data sets will have exclusive data meaning no data can be in more than one of the created data subsets. For this reason train.size + val.size must not exceed 1.
}
\value{
Outputs three data frames of variable size.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Wanja Rast
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

}
\examples{
labels <- sample(c("a","b","c","d") , size = 1000 ,
                  replace = T , prob = c(0.5,0.25,0.2,0.05))

data <- data.frame(timestamp = rep(1:1000 , each = 10) ,
                    measurments = rep(1:10 , n = 10000),
                    label = rep(labels , each = 10))

machine_split(data = data , group = "timestamp" , train.size = 0.6 , val.size = 0.2)

}
