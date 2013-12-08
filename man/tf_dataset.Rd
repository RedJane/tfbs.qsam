\name{tf_dataset}
\alias{tf_dataset}
\docType{data}
\title{
The list contains all information about E.coli TFBS taken from RegulonDB 8.2 
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(tf_dataset)}
\format{
  The format is:
List of 160
 $ AcrR :List of 3
  ..$ AcrR1:List of 8
  .. ..$ EC      : chr "ECK120015994"
  .. ..$ pos     : num [1:2] 484933 484956
  .. ..$ strand  : chr "reverse"
  .. ..$ seq     : chr "gcgttagattTACATACATTTGTGAATGTATGTAccatagcacg"
  .. ..$ effect  : chr "-"
  .. ..$ promoter: chr "acrAp"
  .. ..$ from_tss: num -22.5
  .. ..$ evidence: chr "[BCE|W|Binding of cellular extracts],[GEA|W|Gene expression analysis]"
 [list output truncated]
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{\code{\link{data(tf_equallength)}}}
\examples{
data(tf_dataset)
## maybe str(tf_dataset) ; plot(tf_dataset) ...
}
\keyword{datasets}
