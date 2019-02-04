\name{pre.metrics}
\alias{pre.metrics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute a confusion matrix recall, precision and the amount of unknown behaviours for a model
%%  ~~function to do ... ~~
}
\description{Convenient function to calculate a confisuion matrix, recall and precision for a maschine learning model
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pre_metrics(predicted = ... , expected = ... , uncertain = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{predicted}{Vector with predicted behaviours from the maschine learning output}
  \item{expected}{Vector with expected behaviours which are already known}
  \item{uncertain}{if the prediction process is designed to put all unceartain predictions in the same class, the class name can be specified here in "..."}
%%     ~~Describe \code{x} here~~

}
\details{Takes a vector of predicted behaviours and compares it to a vector of known behaviours. A confusion matrix is computed. The recall and precision are computed after Bidder et al. 2014. In cases were unknown behaviours are considered it computes the proportion of unknown behaviours in contrast to the total number of behaviours recoreded.
%%  ~~ If necessary, more details than the description above ~~
}
\value{The output is a list:
  \item{confusion_matrix}{A confusion matrix from the predicted and expected behaviours of class "table"}
  \item{metrics}{Recall and precision for every behaviour class in a matrix}
  \item{uncertain_predictions}{proportion of uncertain predictions in relation to to total number of redictions as a numeric value}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Bidder, O. R., Campbell, H. A., Gomez-Laich, A., Urge, P., Walker, J., Cai, Y., Wilson, R. P. (2014). Love Thy Neighbour: Automatic Animal Behavioural Classification of Acceleration Data Using the K-Nearest Neighbour Algorithm. PLoS ONE, 9(2), e88609. doi:10.1371/journal.pone.0088609
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
predicted <- c(rep("A",7),"B","C","C",rep("B",8),"A","A",rep("C",9),"B")
expected <- c(rep("A",10),rep("B",10),rep("C",10))

pre.metrics(predicted = predicted , expected = expected)

In case the predictions contain unidentifiable behaviours:

predicted <- c(rep("A",7),"B","C","other",rep("B",8),"A","other",rep("C",9),"B")
expected <- c(rep("A",10),rep("B",10),rep("C",10))

pre.metrics(predicted = predicted , expected = expected , uncertain = "other")
}

