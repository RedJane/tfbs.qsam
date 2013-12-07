\name{QSAM.seq}
\alias{QSAM.seq}
\title{DNA sequences from QSAM-vector}
\description{The function converts QSAM-vector back into DNA sequence. There are 2 predefined types of QSAM
matricies for nuclotides, but it is also possible to set another one for calculations.}
\usage{QSAM.seq(num, QSAM = "QSAM1")}
\arguments{
  \item{num}{QSAM-vector}
  \item{QSAM}{QSAM matrix 4*n, where n is a number of properties; possible values are 'QSAM1' and 'QSAM2'}
}

\value{string of latters A, T, C, G}

\author{Evgenia Temlyakova}



\seealso{\code{\link{seq.QSAM}}}
\examples{
num<-c(-2.23, 0.79, -1.15, -0.78, 2.37, 0.72, -2.07, 1.77, 2.37, 0.72, -2.07, 1.77)
QSAM.seq(num)
}
