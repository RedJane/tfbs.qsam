num<-c(-2.23, 0.79, -1.15, -0.78, 2.37, 0.72, -2.07, 1.77, 0.32, -3.76, 0.03, -1.40, 1.92, 2.52, -1.14, 0.23) 
res<-seq.QSAM('ACGT')

expect_true(class(res)=='numeric')
expect_true(length(res)==16)
expect_equal(res, num)

###

num<-c(-3.9505, 4.0764, -1.1507, 1.2426, 4.3677, 1.0541, 1.5173, 3.2084, -2.7552, -4.8467, 1.1540, 1.4321, 0.4217, 0.8763, 3.3983, -4.0915) 
res<-seq.QSAM('ACGT', QSAM='QSAM2')

expect_true(class(res)=='numeric')
expect_true(length(res)==16)
expect_equal(res, num)

###

