seq.QSAM <-
structure(function # QSAM-transformation of DNA sequences
### The function converts a DNA sequence into QSAM-vector. There are 2 predefined types of QSAM
### values nuclotides, but it is also possible to set another one for calculations.
(s, ##<< DNA-sequence
QSAM='QSAM1' ##<< QSAM matrix 4*n, where n is a number of properties; possible values are 'QSAM1' and 'QSAM2'
){
  
  if(!require(seqinr)){
      stop("Method requires library 'seqinr'. Please install and relaunch.\n")
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
  }
  
  s<-s2n(s2c(s))+1
  
  if (is(QSAM)[1]=='character'){
    QSAM1<-matrix(c(-2.23, 0.79, -1.15, -0.78, 2.37, 0.72, -2.07, 1.77, 0.32, -3.76, 0.03, -1.40, 1.92, 2.52, -1.14, 0.23), nrow=4, byrow=TRUE)
    QSAM2<-matrix(c(-3.9505, 4.0764, -1.1507, 1.2426, 4.3677, 1.0541, 1.5173, 3.2084, -2.7552, -4.8467, 1.1540, 1.4321, 0.4217, 0.8763, 3.3983, -4.0915), nrow=4, byrow=TRUE)
    
    num<-c()
    if (QSAM=='QSAM1'){ q<-QSAM1 }
    if (QSAM=='QSAM2'){ q<-QSAM2 }
    num<-array(NA, length(s)*4)
    j<-1
    for (i in s){
      num[j:(j+3)]<-q[i,]
      j<-j+4
    }
  }
  if (is(QSAM)[1]=='data.frame' || is(QSAM)[1]=='matrix'){
    q<-QSAM
    num<-array(NA, length(s)*4)
    j<-1
    for (i in s){
      num[j:(j+3)]<-q[i,]
      j<-j+4
    }
    num<-unlist(num)
  }
  ##value<< a numeric vector with length equal to number_of_nucleotides*n
  return(num)
  ##seealso<< \code{\link{QSAM.seq}}
}, ex = function(){
  seq.QSAM('AAATTGCGC')
  
  myQSAM<-as.data.frame(matrix(c(-2.23, 0.79, 2.37, 0.72,  0.32, -3.76, 1.92, 2.52), nrow=4, byrow=TRUE))
  rownames(myQSAM)<-c('A', 'C', 'G', 'T')
  seq.QSAM('AAATTGCGC', QSAM=myQSAM)
})
