# for RegulonDB version 8.2
res<-tfbs.pos('Ada')
expect_true(class(res)=="data.frame")
expect_equal(dim(res)[1], 4)

###

res<-tfbs.pos('Ada', strand='forward')
expect_true(class(res)=="data.frame")
expect_equal(dim(res)[1], 1)
expect_equal(as.character(unique(res$strand)), 'forward')

###

res<-tfbs.pos('Ada', strand='reverse')
expect_true(class(res)=="data.frame")
expect_equal(dim(res)[1], 3)
expect_equal(as.character(unique(res$strand)), 'reverse')