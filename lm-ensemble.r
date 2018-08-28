rand.code.ensemble <- function(n, dataset, test.set = c(), train.pct = 0.6, factor = 0, counter = c()){
  source("binarization.r")
  source("lernmatrix.r")
  source("gamma.r")
  if(length(test.set) == 0){
    n.train = floor(nrow(dataset)*train.pct)
    n.test = nrow(dataset) - n.train
    train.idx = sort(sample(1:nrow(dataset),n.train,replace=F))
    test.idx = (1:nrow(dataset))[-train.idx]
  }
  else{
    n.train = nrow(dataset)
    n.test = nrow(test.set)
  }
  predictions = matrix(0,ncol=n.test,nrow=length(levels(dataset[,ncol(dataset)])))
  # Evaluate classifiers on training data. If they clear the threshold on the training data,
  # get the predictions on the test set
  for(i in 1:n){
    #cat("\014")
    #print(paste("-------",i,"-------"))
    if(length(test.set) > 0){
      res = gamma(random.coding(dataset,factor,counter=counter), random.coding(dataset,factor,counter=counter))
      if(res$accuracy > 0.5) test = gamma(random.coding(test.set,factor,counter=counter), random.coding(dataset,factor,counter=counter))
    }
    else{
      rand.code.data = random.coding(dataset,factor,counter=counter)
      res = gamma(rand.code.data[train.idx,],rand.code.data[train.idx,])
      if(res$accuracy >= 0.5) test = gamma(rand.code.data[test.idx,],rand.code.data[train.idx,])
    }
    #print(res$accuracy)
    #print(test$accuracy)
    if(res$accuracy > 0.5) predictions = predictions + log10(res$accuracy / (1-res$accuracy))*test$predictions
    #if(res$accuracy > 0.5) predictions = predictions + test$predictions
    #print(predictions)
  }
  #print(predictions)
  predictions = apply(predictions,2,function(x){floor(x/max(x))})
  if(length(test.set) > 0) true.labels = class.matrix(test.set[,ncol(test.set)])
  else true.labels = class.matrix(dataset[test.idx,ncol(dataset)])
  #print(true.labels)
  return((n.test-sum(apply(abs(true.labels-predictions),2,max)))/n.test)
}

multi.size.run <- function(dataset, factor, counter, runs, ds.name){
  return(list(
    x25 = multi.run(25, runs, dataset, factor, counter, ds.name),
    x50 = multi.run(50, runs, dataset, factor, counter, ds.name),
    x100 = multi.run(100, runs, dataset, factor, counter, ds.name)#,
    #x200 = multi.run(200, runs, dataset, factor, counter, ds.name)
  ))
}

multi.run <- function(size,runs,dataset,factor,counter=c(), ds.name){
  var = c()
  for(i in 1:runs){
    write(paste("[",ds.name," x",size,"] ",i,"/",runs,sep=""), "log_gamma.txt", append = T)
    var = c(var,rand.code.ensemble(size,dataset,factor=factor,counter=counter))
  }
  return(summary(var))
}

multi.run.add <- function(size,runs,dataset,factor){
  var = c()
  for(i in 1:runs) var = c(var,rand.code.ensemble(size,dataset,factor=factor))
  return(summary(var))
}

additive.code.ensemble <- function(n, dataset, train.pct = 0.6, factor = 0){
  source("binarization.r")
  source("lernmatrix.r")
  n.train = floor(nrow(dataset)*train.pct)
  n.test = nrow(dataset) - n.train
  predictions = matrix(0,ncol=n.test,nrow=length(levels(dataset[,ncol(dataset)])))
  train.idx = sort(sample(1:nrow(dataset),n.train,replace=F))
  test.idx = (1:nrow(dataset))[-train.idx]
  # Evaluate classifiers on training data. If they clear the threshold on the training data,
  # get the predictions on the test set
  for(i in 0:(n-1)){
    cat("\014")
    print(paste("-------",i,"-------"))
    det.code.data = additive.coding(dataset,factor,n=n)
    #res = lernmatrix(det.code.data[train.idx,],det.code.data[train.idx,])
    #if(res$accuracy > 0.5) 
    predictions = predictions + lernmatrix(det.code.data[test.idx,],det.code.data[train.idx,])$predictions
  }
  #print(predictions)
  predictions = apply(predictions,2,function(x){floor(x/max(x))})
  true.labels = class.matrix(dataset[test.idx,ncol(dataset)])
  #print(true.labels)
  return((n.test-sum(apply(abs(true.labels-predictions),2,max)))/n.test)
}