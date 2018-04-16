lernmatrix <- function(test,train,output=""){
  #Training
  Y <- class.matrix(train[,ncol(train)])
  X <- as.matrix(-(-1)^train[,-ncol(train)])
  M <- Y %*% X
  #Classification
  YP <- class.matrix(test[,ncol(test)])
  XP <- t(as.matrix(test[,-ncol(test)]))
  Z <- M %*% XP
  Z <- sapply(1:nrow(test),function(i){return(as.integer(Z[,i]==max(Z[,i])))})
  return(list(predictions=Z,accuracy=mean(sapply(1:nrow(test),function(i){return(as.integer(identical(Z[,i],YP[,i])))}))))
  #print(list(predictions=Z,accuracy=mean(sapply(1:nrow(test),function(i){return(as.integer(identical(Z[,i],YP[,i])))}))))
}

class.matrix <- function(classes){
  lev <- levels(as.factor(classes))
  return(sapply(1:length(classes),function(i){return(as.integer(lev==classes[i]))}))
}

lernmatrix.legacy <- function(test, train, output=""){
  correct <- 0
  total <- nrow(test)
  classes <- unique(rbind(test,train)[,ncol(test)])
  n.classes <- length(classes)
  features <- ncol(test)-1
  #Train
  M <- matrix(0,nrow=n.classes,ncol=features)
  for(i in 1:nrow(train)){
    x <- c(t(train[i,1:features]))
    y <- c()
    for(j in 1:n.classes){
      if(classes[j]==train[i,ncol(train)]){
        y <- c(y, 1)
      }
      else{
        y <- c(y,0)
      }
    }
    m = outer(y,x,FUN=Vectorize(lm.learn.rule))
    M = M + m
  }
    #Test
  for(i in 1:nrow(test)){
    x <- c(t(test[i,1:features]))
    y <- c()
    for(j in 1:n.classes){
      if(classes[j]==test[i,ncol(test)]){
        y <- c(y, 1)
      }
      else{
        y <- c(y,0)
      }
    }
    y <- as.integer(y)
    w <- am.classify(M,x)
    #
    if(identical(w,y)){
      correct <- correct + 1
    }
  }
  return(correct / total)
}


lm.learn.rule <- function(x,y){
    if(x==1 && y==1) z=1
    if(x==1 && y==0) z=-1
    if(x==0) z=0
    return(z)
}