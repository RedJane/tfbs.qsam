\name{tf_equallength}
\alias{tf_equallength}
\docType{data}
\title{
The list contains an information taken from RegulonDB 8.2 about 8 E.coli TFBS (ArcA, CRP, Fis, FNR, IHF, Lrp, NarL, Fur) with equal length.  
}
\description{
Very often RegulonDB contains entries for one TF with different binding site lengths. It might cause difficulties for TFBS analysis. To avoid the problem we created this list, all entries in it provide sequences with equal (most frequent) lengths. 
}
\usage{data(tf_equallength)}
\format{
  The format is:
List of 8
 $ ArcA:List of 104
  ..$ :List of 11
  .. ..$ EC      : chr "ECK120011345"
  .. ..$ pos     : num [1:2] 41981 41995
  .. ..$ strand  : chr "reverse"
  .. ..$ seq     : chr "tatattaaatGTTAACAAAAATAAAacaaacggga"
  .. ..$ effect  : chr "+"
  .. ..$ promoter: chr "caiTp"
  .. ..$ from_tss: num 50
  .. ..$ evidence: chr "[GEA|W|Gene expression analysis],[AIBSCS|W|Automated inference based on similarity to consensus sequences]"
  .. ..$ length  : int 15
  .. ..$ mpot    : num [1:121] -0.0613 -0.0612 -0.0611 -0.0611 -0.0611 ...
  .. ..$ weight  : num -6.2
 .. [list output truncated]
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
http://regulondb.ccg.unam.mx/menu/download/datasets/files/BindingSiteSet.txt
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{data(tf_dataset)}}}
\examples{
data(tf_equallength)
## maybe str(tf_equallength) ; plot(tf_equallength) ...
}
\keyword{datasets}
