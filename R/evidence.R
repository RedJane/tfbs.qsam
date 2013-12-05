evidence <-
function # Short evidence representation
### The function converts long RegulonDB strings about the evidence into 
### short and clear representation with letters |S| and |W|. 
(evidence.string ##<< RegulonDB string, containing the evidence for a TFBS or a promoter
){
  if ((gregexpr('|S|', evidence.string, fixed=TRUE)[[1]][1])>0){ evidence<-'|S|' }
  if ((gregexpr('|W|', evidence.string, fixed=TRUE)[[1]][1])>0){ evidence<-'|W|' }
  else evidence<-NA
  return(evidence)
}
