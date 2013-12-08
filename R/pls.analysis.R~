pls.analysis <-
structure(function # PLS-DA analysis
### The function performs PLS-DA analysis and returns the detailed result about the created model. 
### There are two possible input formats: set1 and set2 (and the function would prepare and form
### all nessacery sets by itself), and train and test (user defines these sets in the proper format). 
(set1=NULL, ##<< a matrix or a data.frame for set1 with objects in rows and descriptors in columns
set2=NULL,  ##<< a matrix or a data.frame for set2 with objects in rows and descriptors in columns
train=NULL, ##<< a data.frame with two subobjects of class AsIs - descriptors and binary classes
test=NULL, ##<< a data.frame with two subobjects of class AsIs - descriptors and binary classes
shuffle=TRUE, ##<< logical: do you want to shuffle objects order in each set?
train.part=50 ##<< a proportion of objects to be put in the training process; only if you specify set1 and set2
){
  if(length(train)+length(test)+length(set1)+length(set2)==0){
      stop('Wrong argument: no data specified.\n')
  }
  if(length(train)+length(test)>0 & length(set1)+length(set2)>0){
      stop('Wrong argument: specify set1 and set2 or train and test! It is imposible to use both at one time.\n')
  }
  if(!require(pls)){
      stop("Method requires library 'pls'. Please install and relaunch.\n")
  }  
  if(train.part<0 | train.part>100){
      stop('Wrong argument: the value train.part should be from 0 to 100%.\n')
  }

  
  if (length(train)+length(test)>0 & length(set1)+length(set2)==0){
    if(dim(train)[2]!=2 | dim(test)[2]!=2){
	stop('Wrong argument: train and test sets do not contain two subobjects - descriptors and classes.\n')
    }
    if(class(train[, 1])[1]!='AsIs' | class(test[, 1])[1]!='AsIs'){
	stop('Wrong argument: train and test arguments should be of class "AsIs".\n')
    }
    if(dim(train)[2]!=dim(test)[2]){
	stop('Wrong argument: different number of columns in train and test sets!\n')
    }
    if(dim(train[,1])[2]<5){
	stop('Wrong argument: number of descriptors is too small!\n')
    }
    if(dim(train[,1])[1]<10){
	stop('Wrong argument: number of objects is too small!\n')
    }

    tf.train<-train
    tf.test<-test
    colnames(tf.train)<-c('CH', 'TY')
    colnames(tf.test)<-c('CH', 'TY')
    tp.i<-which(tf.test$TY==1)
    
    tf.pls <-plsr(TY ~ CH, ncomp=10, data=tf.train, scale=TRUE, centered=TRUE, validation='CV')
    tf.predict<-predict(tf.pls, tf.test, ncomp=1:10, type='response')
    
    res<-c()
    for (threshold in seq(0, 1, 0.05)){
	tp<-length(which(tf.predict[tp.i]>=threshold))
	fn<-length(which(tf.predict[tp.i]<threshold))
	tn<-length(which(tf.predict[-tp.i]<threshold))
	fp<-length(which(tf.predict[-tp.i]>threshold))
	r<-tp/(tp+fn) 
	p<-tp/(tp+fp)
	a<-(tp+tn)/(tp+tn+fp+fn)
	s<-tn/(fp+tn)  
	res<-rbind(res, c(tp, fn, tn, fp, r, s, p, a))
    }
    colnames(res)<-c('tp', 'fn', 'tn', 'fp', 'r', 's', 'p', 'a')
    rownames(res)<-seq(0, 1, 0.05)

    r2<-100*drop(R2(tf.pls, estimate='all', intercept=FALSE)$val)
    bc<-which.max(r2[2,])
  
    ##<<source pls package: http://cran.r-project.org/web/packages/pls/index.html
    result<-list(pls=tf.pls, train=tf.train, test=tf.test, predict=tf.predict, perform=res, bc=bc)
  }
  
  if (is.null(train) & is.null(test)==0 & length(set1)[1]+length(set2)[1]>0){ 
    if(!class(set1)%in%c('data.frame', 'matrix')){
      stop('Wrong argument: check set1 class! It should be a matrix or a data.frame.\n')
    }
    if(!class(set2)%in%c('data.frame', 'matrix')){
	stop('Wrong argument: check set2 class! It should be a matrix or a data.frame.\n')
    } 
    if(dim(set1)[2]!=dim(set2)[2]){
	stop('Wrong argument: different number of columns in set1 and set2 sets!\n')
    }
    if(dim(set1)[2]<5){
	stop('Wrong argument: number of descriptors is too small!\n')
    }
    if(dim(set1)[1]<10 | dim(set2)[1]<10){
	stop('Wrong argument: number of objects is too small!\n')
    }
    l1<-dim(set1)[1]
    l2<-dim(set2)[1]
    
    if (is.null(rownames(set1))){ rownames(set1)<-paste('set1', 1:l1, sep='_') }
    if (is.null(rownames(set2))){ rownames(set2)<-paste('set2', 1:l2, sep='_') }

    if (shuffle==TRUE){ 
      set1<-set1[sample(1:l1, l1),]
      set2<-set2[sample(1:l2, l2),]
    }
    l<-round(min(l1, l2)/(100/train.part))

    tr<-rbind(set1[1:l, ], set2[1:l,])
    te<-rbind(set1[1:(l1-l), ], set2[1:(l2-l),])

    Y.train<-c(rep(1, l), rep(0, l))
    Y.test<-c(rep(1,(l1-l)), rep(0, (l2-l)))

    tf.train<-data.frame(CH=I(as.matrix(tr)), TY=I(Y.train))
    tf.test<-data.frame(CH=I(as.matrix(te)), TY=I(Y.test))   
    
    tf.pls <-plsr(TY ~ CH, ncomp=10, data=tf.train, scale=TRUE, centered=TRUE, validation='CV')
    tf.predict<-predict(tf.pls, tf.test, ncomp=1:10, type='response')
    ll<-dim(tf.predict)[1]
    
    res<-c()
    for (threshold in seq(0, 1, 0.05)){
	tp<-length(which(tf.predict[1:l]>=threshold))
	fn<-length(which(tf.predict[1:l]<threshold))
	tn<-length(which(tf.predict[(l+1):ll]<threshold))
	fp<-length(which(tf.predict[(l+1):ll]>threshold))
	r<-tp/(tp+fn) 
	p<-tp/(tp+fp)
	a<-(tp+tn)/(tp+tn+fp+fn)
	s<-tn/(fp+tn)  
	res<-rbind(res, c(tp, fn, tn, fp, r, s, p, a))
    }
    colnames(res)<-c('tp', 'fn', 'tn', 'fp', 'r', 's', 'p', 'a')
    rownames(res)<-seq(0, 1, 0.05)

    r2<-100*drop(R2(tf.pls, estimate='all', intercept=FALSE)$val)
    bc<-which.max(r2[2,])
    
    ##<<source pls package: http://cran.r-project.org/web/packages/pls/index.html
    result<-list(pls=tf.pls, train=tf.train, test=tf.test, predict=tf.predict, perform=res, bc=bc)
  }
  result<-NA
  ##<<value list with results from PLS-DA analysis
  return(result)
}, ex = function(){
  set1<-matrix(rnorm(500, 1), nrow=50,  byrow=TRUE)
  set2<-matrix(rnorm(500, 2), nrow=50,  byrow=TRUE)
  res<-pls.analysis(set1=set1, set2=set2)

  train.set<-rbind(set1[1:25,], set2[1:25,])
  test.set<-rbind(set1[26:50,], set2[26:50,])
  train<-data.frame(CH=I(train.set), TY=I(c(rep(1, 25), rep(0, 25))))
  test<-data.frame(CH=I(test.set), TY=I(c(rep(1, 25), rep(0, 25))))
  res<-pls.analysis(train=train, test=test)
})
