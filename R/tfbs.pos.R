tfbs.pos <-
function #TFBS positions on the E.coli chromosome
### The function locates all binding sites for specified TF on E.coli chromosome for one or both DNA strands. It uses information taken from RegulonDB version 8.2.
(tfname, ##<< a name of TF 
strand='both' ## which strand to show; possible values are 'forward', 'reverse', 'both'
)
{
  if (class(strand)!='character'| !strand%in%c('both', 'forward', 'reverse')){
      stop('Wrong argument: correct the argument strand! It could be equal to "forward", "reverse" or "both".\n')
  }
  if (!tfname%in%names(tf_dataset)){
    stop(paste0('Wrong argument: there is no information about TF ', tfname, ' in RegulonDB! \n'))
  }

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
