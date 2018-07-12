\name{pre.comp}
\alias{pre.comp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Prediction of behaviour using LDA, QDA, CART, KNN, RF SVM
%%  ~~function to do ... ~~
}
\description{The function uses an existing model data set of summary statistics with corresponding behaviours to predict                 the behaviours of the novel data set also consisting of summary statistics.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pre.comp(pre.frame=... , test.frame=... , algorithms=c() ,
          Formula=c() , Goals=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pre.frame}{data.frame with summary statistics and corresponding behaviours}
  \item{test.frame}{data.frame with summary statistics for that the behaviour should be predicted}
  \item{algorithms}{vector of algorithms which should be used to predict behaviours(see Details)}
  \item{Formula}{formula by which the algorithms will handle the data(see Details)}
  \item{Goals}{TRUE if the data to be predicted has known behaviours that should be compared to the predictions (default                  is FALSE)}
%%     ~~Describe \code{x} here~~
}
\details{Predictions work as they should on  r class "factor". If a numeric code is used for behaviour variables they have           to be converted to the class "factor" otherwiese the results will not be interpretable.

          Predictions can be made by the algorithms Linear Discriminat Analysis("LDA"), Quadratic Discriminat Analysis               ("QDA"), k-nearest neigbour("KNN"), Classification and regression tree("CART"), Random Forest("RF")
          and Support vector machine("SVM"). The algorithms that should be used can be specified with the algorithms                 argument by their corresponding abbrevation. Whenever more than two algorithms are chosen a majority vote is               included that choses the behaviour as a prediction that was predicted by most of the algorithms. Ties are decided           by chance.

          The formula will specify which summary statistic values will be used for the analysis. It has the same format             that the machine learning algorithms rely on. Here this is taken from the output of the function sum.data().
          The variables in the formula have to correspond to the column names of the summary statistics data.frame.
          Example: formula=Bev ~ meanx + meany + meanz + ...

%%  ~~ If necessary, more details than the description above ~~
}
\value{The output will be a data.frame.
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
\examples{
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
