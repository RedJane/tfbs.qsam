process.pred <-
function # Prediction data processing
### The function combines and precess primary prediction data of a few 
### classifications by various predefined schemes.
(pred, ##<< predictions dataframe
scheme='model1+', ##<< processing scheme to use; possible scemes are 'model1+', 'equal.contribution'
bin, ##<< list with 3-4 sublists called 'exp', 'pro', 'reg', 'rtb' containing binary vectors where 1 corresponds to TFBS 
window ##<< window for sliding; we use a length of TFBS
)
{
  if (scheme=='model1+'){
    tf.m1<-pred[,1]+(as.vector(pred[,3]*bin$pro$f)+as.vector(pred[,5]*bin$reg$f)-1)/2 
    tf.m1<-c(tf.m1, tf.m1[1:500])
    tf.slide.f<-sapply(1:dim(pred)[1], function(x) sum(tf.m1[x:(x+window-1)]))
    tf.slide.f<-as.data.frame(tf.slide.f)
    
    tf.m1<-pred[,2]+(as.vector(pred[,4]*bin$pro$r)+as.vector(pred[6]*bin$reg$r)-1)/2 
    tf.m1<-c(tf.m1, tf.m1[1:500])
    tf.slide.r<-sapply(1:dim(pred)[1], function(x) sum(tf.m1[x:(x+window-1)]))
    tf.slide.r<-as.data.frame(tf.slide.r)
  }
  ##<<details Scheme 'equal.contribution' simply summarises all predictions for each 
  ## nucleotide position.
  ## Scheme 'model1+' takes sum of all predictions from the model 1 and a half values
  ## of models 2 predictions (pro and reg) for nucleotide positions with positive PWM
  ## predictions.

  if (scheme=='equal.contribution'){
    tf.slide.f<-sliding.sum(pred=pred, window=window)
    tf.slide.r<-sliding.sum(pred=pred, window=window, columns=c(2, 4, 6))    
  }
  
  res<-cbind(tf.slide.f, tf.slide.r)
  rownames(res)<-paste0('pos', 1:dim(res)[1])
  colnames(res)<-c('forward', 'reverse')
  ##<<value dataframe with a column for each strand
  return(res)
}
