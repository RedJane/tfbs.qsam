evidence <-
function # Short evidence representation
### The function converts long RegulonDB strings about the evidence into 
### short and clear representation with letters |S| and |W|. 
(evidence.string ##<< RegulonDB string, containing the evidence for a TFBS or a promoter
){
  if(class(evidence.string)!='character'){
      stop("The argument evidence.string should be a character string.\n")
  }
  if ((gregexpr('|S|', evidence.string, fixed=TRUE)[[1]][1])>0){ evidence<-'|S|' }
  if ((gregexpr('|W|', evidence.string, fixed=TRUE)[[1]][1])>0){ evidence<-'|W|' }
  else evidence<-NA
  ##<<value character string; |S| for objects with strong evidence, |W| - for weak evidence.
  return(evidence)
}
