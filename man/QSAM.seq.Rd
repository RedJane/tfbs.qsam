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

\references{
Liang, G., & Li, Z. (2007). Scores of generalized base properties for quantitative sequence-activity modelings for< i> E. coli</i> promoters based on support vector machine. Journal of Molecular Graphics and Modelling, 26(1), 269-281.
Sandberg, Maria, Michael Sjöström, and Jörgen Jonsson. "A multivariate characterization of tRNA nucleosides." Journal of Chemometrics 10.5‐6 (1996): 493-508.
}

\seealso{\code{\link{seq.QSAM}}}

\examples{
num<-c(-2.23, 0.79, -1.15, -0.78, 2.37, 0.72, -2.07, 1.77, 2.37, 0.72, -2.07, 1.77)
QSAM.seq(num)
}
