myQSAM<-matrix(rnorm(16, 5), nrow=4, byrow=TRUE)
rownames(myQSAM)<-c('A', 'C', 'G', 'T')
num<-c(myQSAM[1,], myQSAM[2,], myQSAM[1,], myQSAM[3,], myQSAM[4,]) 

res<-QSAM.seq(num, myQSAM)

expect_true(class(res)=='character')
expect_true(nchar(res)==4)
expect_equal(res, 'ACAGT')

###
rownames(myQSAM)<-c('A', 'T', 'H', 'C')
expect_warning(QSAM.seq(num, myQSAM), 'Chech rownames in the QSAM matrix!')

expect_error(QSAM.seq(num, QSAM=c(1, 2, 3, 4)), "Wrong argument: correct the argument QSAM! Choose one of predefined matrices ('QSAM1', 'QSAM2') or set it as a data.frame or a matrix.")
