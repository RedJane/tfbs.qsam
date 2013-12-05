coli.tfbs <-
function # E.coli TFBS
### The function returns main information about all known TFBS of E.coli genome.
(
)
{
  
  coli.tf<-data.frame()
  for (tf in names(tf_dataset)){
    for (i in 1:length(tf_dataset[[tf]])){
      coli.tf<-rbind(coli.tf, data.frame(name=tf, st=tf_dataset[[tf]][[i]]$pos[1], fn=tf_dataset[[tf]][[i]]$pos[2],  strand=tf_dataset[[tf]][[i]]$strand, evidence=evidence(tf_dataset[[tf]][[i]]), effect=tf_dataset[[tf]][[i]]$effect, promoter=tf_dataset[[tf]][[i]]$promoter))
    }
  }
  ##<<value dataframe with tf name, location, strand, evidence, effect and promoter.
  return(coli.tf)
}
