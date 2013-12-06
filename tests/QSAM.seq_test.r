myQSAM<-matrix(rnorm(16, 5), nrow=4, byrow=TRUE)
rownames(myQSAM)<-c('A', 'C', 'G', 'T')
num<-c(myQSAM[1,], myQSAM[2,], myQSAM[1,], myQSAM[3,], myQSAM[4,]) 

res<-QSAM.seq(num, myQSAM)

expect_true(class(res)=='character')
expect_true(nchar(res)==4)
expect_equal(res, 'ACAGT')


