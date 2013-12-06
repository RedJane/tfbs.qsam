QSAM.seq <-
structure(function # DNA sequences from QSAM-vector
### The function converts QSAM-vector back into DNA sequence. There are 2 predefined types of QSAM
### matricies for nuclotides, but it is also possible to set another one for calculations.
(num, ##<< QSAM-vector  
QSAM='QSAM1' ##<< QSAM matrix 4*n, where n is a number of properties; possible values are 'QSAM1' and 'QSAM2'
){ 

  if(!class(num)=='numeric'){
      stop("Wrong argument: num should be numeric!.\n")
  }
  if(!class(QSAM)%in%c('character', 'data.frame', 'matrix')){
      stop("Wrong argument: correct the argument QSAM! Choose one of predefined matrices ('QSAM1', 'QSSAM2') or set it as a data.frame or a matrix.\n")
  }
  if(class(QSAM)%in%c('data.frame', 'matrix')){
    if(dim(QSAM)[1]!=4){ 
      stop("Wrong argument: the number of rows in the QSAM matrix should be equal to 4 (as a number of canonical nucleotides).\n") 
    }
    classinfo<-unique(apply(QSAM, 1, class))
    if(length(classinfo)>1 | classinfo!='numeric'){
      stop("Wrong argument: tha QSAM matrix should contain only numeric values!\n")
    }
    if(length(num)%%dim(QSAM)[2]!=0){
      stop(paste0('Wrong argument: the number of values in num should be divisible by ', dim(QSAM)[2], '.\n'))
    }    
  }
 
  
  if (is(QSAM)[1]=='character'){
    QSAM1<-as.data.frame(matrix(c(-2.23, 0.79, -1.15, -0.78, 2.37, 0.72, -2.07, 1.77, 0.32, -3.76, 0.03, -1.40, 1.92, 2.52, -1.14, 0.23), nrow=4, byrow=TRUE))
    QSAM2<-as.data.frame(matrix(c(-3.9505, 4.0764, -1.1507, 1.2426, 4.3677, 1.0541, 1.5173, 3.2084, -2.7552, -4.8467, 1.1540, 1.4321, 0.4217, 0.8763, 3.3983, -4.0915), nrow=4, byrow=TRUE))
    rownames(QSAM1)<-c('A', 'C', 'G', 'T')
    rownames(QSAM2)<-c('A', 'C', 'G', 'T')
    
    s<-''
    for (i in seq(1, length(num), dim(QSAM)[2])){
      if (QSAM=='QSAM1'){ s<-paste0(s, rownames(QSAM1)[which(QSAM1[,1]==num[i])]) }
      if (QSAM=='QSAM2'){ s<-paste0(s, rownames(QSAM2)[which(QSAM2[,1]==num[i])]) }
    }
  }
  
  if (class(QSAM)%in%c('data.frame','matrix')){
    s<-''
    for (i in seq(1, length(num),  dim(QSAM)[2])){
      s<-paste0(s, rownames(QSAM)[which(QSAM[,1]==num[i])])
    }
  }
  ##value<< string of latters A, T, C, G
  return(s)
  ##seealso<< \code{\link{seq.QSAM}}
}, ex = function(){
  num<-c(-2.23, 0.79, -1.15, -0.78, 2.37, 0.72, -2.07, 1.77, 2.37, 0.72, -2.07, 1.77)
  QSAM.seq(num)
})
