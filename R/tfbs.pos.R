tfbs.pos <-
function #TFBS positions on the E.coli chromosome
### The function locates all TFBS on E.coli chromosome for one or both DNA strands.
(tfname, ##<< a name of TF 
strand='both' ## which strand to show; possible values are 'forward', 'reverse', 'both'
)
{
  strand.<-strand
  if (strand.=='both'){ strand.<-c('reverse', 'forward') }
  res<-data.frame(stringsAsFactors = FALSE)
  tfs<-tf_dataset[[tfname]]
  for (i in 1:length(tfs)){
    res<-rbind(res, data.frame(st=tfs[[i]]$pos[1], fn=tfs[[i]]$pos[2],  strand=tfs[[i]]$strand))
  }
  res<-subset(res, strand%in%strand.)
  rownames(res)<-NULL
  ### value<< dataframe with the first and the last positions and strand information. 
  return(res)
}
