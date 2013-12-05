\name{process.pred}
\alias{process.pred}
\title{Prediction data processing}
\description{The function combines and precess primary prediction data of a few 
classifications by various predefined schemes.}
\usage{process.pred(pred, scheme = "model1+", bin, window)}
\arguments{
  \item{pred}{predictions dataframe}
  \item{scheme}{processing scheme to use; possible scemes are 'model1+', 'equal.contribution'}
  \item{bin}{list with 3-4 sublists called 'exp', 'pro', 'reg', 'rtb' containing binary vectors where 1 corresponds to TFBS}
  \item{window}{window for sliding; we use a length of TFBS}
}



\author{Evgenia Temlyakova}





