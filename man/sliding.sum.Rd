\name{sliding.sum}
\alias{sliding.sum}
\title{Calculation of sliding sums}
\description{Converts pred for each DNA position into sliding sum representation}
\usage{sliding.sum(pred, window, dim = 1, columns = c(1, 3, 5), tail = 500)}
\arguments{
  \item{pred}{table with pred in columns}
  \item{window}{window for summation}
  \item{dim}{dimensions; use 1 for rows and 2 for columns}
  \item{columns}{colomns to use}
  \item{tail}{number of flanking positions at the end of the prediction table}
}



\author{Evgenia Temlyakova}





