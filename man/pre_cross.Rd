\name{pre.cross}
\alias{pre.cross}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Predticon with leave one out cross validation
%%  ~~function to do ... ~~
}
\description{Preform a prediction of behaviours with LDA,QDA,KNN,CART,RF and SVM on data frame of summary statistics.
              The predicted behaviours will be validated in a leave one out cross validation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pre.cross(pre.frame,Formula)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pre.frame}{a data.frame containing summary statistics and the known behaviour corresponding to each row of summmary statistics.
  \item{Formula}{formula by which the algorithms will handle the data(see Details)}
%%     ~~Describe \code{x} here~~
}
}
\details{The functionality of this pre.cross() is based on the output of sum.data(). Every row of the summary statistics             data.frame corresponds to one behaviour recording.

The formula will specify which summary statistic values will be used for the analysis. It has the same format             that the machine learning algorithms rely on. Here this is taken from the output of the function sum.data().
          The variables in the formula have to correspond to the column names of the summary statistics data.frame.
          Example: formula=Bev ~ meanx + meany + meanz + ...
The behaviours in the behaviour column have to be of the class factor.
%%  ~~ If necessary, more details than the description above ~~
}
\value{The outut will be a data.frame.
\item{goal}{The goal column is taken from the summary statistics data.frame and should be compared to the predictions made             by the algorithms.}
\item{Majority}{The majority column represents the behaviour that was predicted by the majorty of the algorithms.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Wanja Rast, Alexandre Coutriol
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{pre.cross(hedgehog_sum_data)
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
