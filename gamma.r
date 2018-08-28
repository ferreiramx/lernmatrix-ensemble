gamma <- function(test, train){
  classes = sort(unique(rbind(train,test)[,ncol(train)]))
  p = min(apply(train[-ncol(train)],2,max))
  ret = apply(test,1,function(V){
    th = 0
    x = as.numeric(t(as.matrix(V[-ncol(test)])))
    while(th <= p){
      class.support = c(rep(0,length(classes)))
      m = as.matrix(train[,-ncol(train)])
      sums = rowSums((abs(sweep(m,2,x))<=th)+0)
      
      for(i in 1:length(classes)){
        idx = which(train[,ncol(train)]==classes[i])
        class.support[i] = sum(sums[idx])/length(idx)
      }
      #print(class.support)
      max.supp = which(class.support==max(class.support))
      
      if(length(max.supp) > 1){
        #print(paste("no single maximum exists. increasing threshold (",th+1,")"))
        th = th + 1
        if(th > p){
          #print(paste("th=",th))
          retval = c(-1,0)
          retval[1] = (max.supp[1])
          retval[2] = (0+(classes[max.supp]==V[length(V)]))
          return(retval)
        }
      }
      else{
        retval = c(-1,0)
        retval[1] = (max.supp[1])
        retval[2] = (0+(classes[max.supp]==V[length(V)]))
        return(retval)
      }
    }
  })
  Z = matrix(0,nrow = length(classes), ncol=nrow(test))
  for(i in 1:nrow(test)){
    Z[ret[1,i],i] = 1
  }
  return(list(predictions=Z,accuracy=mean(ret[2,])))
}
