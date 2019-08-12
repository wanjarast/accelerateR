\name{tag.import}
\alias{tag.import}
\title{Import and combine multiple files
}
\description{Import and combine multiple raw data acceleration files
}
\usage{
tag_import(tag.id , rounding , eobs = TRUE , data.path = NULL)
}
\arguments{
  \item{tag.id}{Tag id of the tag that is read in. Multiple files of the same tag should be indicated with either a number or a letter (capital or small). Example: tag.id_acc_1 or tag.id_acc-a. Works also if only one file exists. Example: tag.id_acc
}
  \item{rounding}{Rounds the timestamp down to the desired position set in seconds. No rounding when set to 1. The default is 10 which will round the timestamp down to full 10 seconds.
}
  \item{eobs}{Indicating wether the raw data come from an eobs tag and got their standart format or not. Default is TRUE.
}
  \item{data.path}{Data path of the data in case they are not stored in the RStudio project folder. If the data is located in the project folder no path is needed.
}
}
\details{
This function will only accept txt files. All files of the same individual schould be named in order. Multiple files of the same individual should be indicated with either a number or a letter (capital or small). Excepted formats are "tag.id_acc_1.csv" or "tag.id_acc_a.csv". In case there is only one behaviour file it should be named "tag.id_acc.csv".
All data will be read in in order of the file naming. Timestamp order will have no influence.
}
\value{
The output will be a data.table.data frame which should work like a base R data.frame in most cases.
}
\author{
Wanja Rast
}
\note{
%%  ~~further notes~~
}


\seealso{
bev.import
}
\examples{
##---- NOT RUN ----
data <- tag.import(tag.id = "tag6566" , rounding = 10 , eobs = T)
}
