sliding.sum <-
function # Calculation of sliding sums
### Converts pred for each DNA position into sliding sum representation
(pred, ##<< table with pred in columns
window, ##<< window for summation
dim=1, ##<< dimensions; use 1 for rows and 2 for columns
columns=c(1,3, 5), ##<< colomns to use
tail=500 ##<< number of flanking positions at the end of the prediction table
){
  res<-apply(pred[ , columns], dim=dim, sum)
  res<-rbind(res, res[1:tail])
  slide<-sapply(1:(length(res)-window+1), function(x) sum(res[x:(x+window-1)]))
  slide<-as.data.frame(slide)
  rownames(slide)<-paste0('pos', 1:dim(slide)[1])
    
  return(slide)  
}
