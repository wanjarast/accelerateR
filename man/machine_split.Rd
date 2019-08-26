\name{machine_split}
\alias{machine_split}
\title{Split data into training, validation and test data sets
%%  ~~function to do ... ~~
}
\description{Splits the data into three data sets of custom size. Lable proportions in the original data set will be repsected for all three data sets.
}
\usage{
machine_split(data = predictors , group = "timestamp" , behaviour , train_size = 0.6 , val_size = 0.2 , names = c("train_data","val_data","test_data"))
}
\arguments{
  \item{data}{Complete data set that will be split in three sub sets
}
  \item{group}{Variable that identifies data belonging to the same group. Usually timestamp in acc behaviour predictions is the default.
}
  \item{behaviour}{Coulmn name with the behaviour labels.
}
  \item{train_size}{Proportion of data that will create the training data. Possibile values are between 0 and 1. Default is 0.6.
}
  \item{val_size}{Proportion of data that will create the validation data. Possibile values are between 0 and 1. Default is 0.2.
}
  \item{names}{Vector of names for the three data sets. Default is c("train_data","val_data","test_data")
}
}
\details{
This function should be run without saving into an object. The resulting subsets will be created from within the function. The sizes of the sets are free to choose. the test data set will be of size 1- train_size - val_size. In case train_size and val_size add up to 1 no test data will be created.
All data sets will have exclusive data meaning no data can be in more than one of the created data subsets. For this reason train_size + val_size must not exceed 1.
}
\value{
Outputs three dplyr tibbles of variable size.
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

machine_split(data = data , group = "timestamp" , train_size = 0.6 , val_size = 0.2)

}
