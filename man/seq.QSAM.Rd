\name{seq.QSAM}
\alias{seq.QSAM}
\title{QSAM-transformation of DNA sequences}
\description{The function converts DNA sequence into QSAM-vector. There are 2 predefined types of QSAM
values nuclotides, but it is also possible to set another one for calculations.}
\usage{\method{seq}{QSAM}(s, QSAM = "QSAM1")}
\arguments{
  \item{s}{DNA-sequence}
  \item{QSAM}{QSAM matrix 4*n, where n is a number of properties; possible values are 'QSAM1' and 'QSAM2'}
}

\value{a numeric vector with length equal to number_of_nucleotides*n}

\author{Evgenia Temlyakova}

\references{
Liang, G., & Li, Z. (2007). Scores of generalized base properties for quantitative sequence-activity modelings for< i> E. coli</i> promoters based on support vector machine. Journal of Molecular Graphics and Modelling, 26(1), 269-281.
Sandberg, Maria, Michael Sjöström, and Jörgen Jonsson. "A multivariate characterization of tRNA nucleosides." Journal of Chemometrics 10.5‐6 (1996): 493-508.
}


\seealso{\code{\link{QSAM.seq}}}
\examples{
seq.QSAM('AAATTGCGC')

myQSAM<-as.data.frame(matrix(c(-2.23, 0.79, 2.37, 0.72,  0.32, -3.76, 1.92, 2.52), nrow=4, byrow=TRUE))
seq.QSAM('AAATTGCGC', QSAM=myQSAM)
}
