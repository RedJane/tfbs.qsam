\name{pls.analysis}
\alias{pls.analysis}
\title{PLS-DA analysis}
\description{The function performs PLS-DA analysis and returns the detailed result about the created model. 
There are two possible input formats: set1 and set2 (and the function would prepare and form
all nessacery sets by itself), and train and test (user defines these sets in the proper format). }
\usage{pls.analysis(set1 = NULL, set2 = NULL, train = NULL, test = NULL, 
    shuffle = TRUE, train.part = 50)}
\arguments{
  \item{set1}{a matrix or a data.frame for set1 with objects in rows and descriptors in columns}
  \item{set2}{a matrix or a data.frame for set2 with objects in rows and descriptors in columns}
  \item{train}{a data.frame with two subobjects of class AsIs - descriptors and binary classes}
  \item{test}{a data.frame with two subobjects of class AsIs - descriptors and binary classes}
  \item{shuffle}{logical: do you want to shuffle objects order in each set?}
  \item{train.part}{a proportion of objects to be put in the training process; only if you specify set1 and set2}
}



\author{Evgenia Temlyakova}




\examples{
set1<-matrix(rnorm(500, 1), nrow=50,  byrow=TRUE)
set2<-matrix(rnorm(500, 2), nrow=50,  byrow=TRUE)
res<-pls.analysis(set1=set1, set2=set2)

train.set<-rbind(set1[1:25,], set2[1:25,])
test.set<-rbind(set1[26:50,], set2[26:50,])
train<-data.frame(CH=I(train.set), TY=I(c(rep(1, 25), rep(0, 25))))
test<-data.frame(CH=I(test.set), TY=I(c(rep(1, 25), rep(0, 25))))
res<-pls.analysis(train=train, test=test)
}
