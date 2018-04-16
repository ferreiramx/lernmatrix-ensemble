binarize.std <- function(dataset, factor = 0, bits = -1){
  library("GA")
  data = dataset[-ncol(dataset)]
  # Add the most negative value 
  #val = min(data)
  #data = data + val
  # Multiply by the factor
  data = data * 10^factor
  if(bits < 0){
    max.val = max(data)
    bits = length(decimal2binary(max.val))
  }
  bin.data = unlist(sapply(t(data),function(x){decimal2binary(x,length=bits)}))
  dim(bin.data) = c((ncol(dataset)-1)*bits,nrow(dataset))
  bin.data <- t(bin.data)
  return(as.data.frame(cbind(bin.data,dataset[,ncol(dataset)])))
}

binarize.gray <- function(dataset, factor = 0, bits = -1){
  library("GA")
  data = dataset[-ncol(dataset)]
  # Add the most negative value 
  val = min(data)
  data = data + val
  # Multiply by the factor
  data = data * 10^factor
  if(bits < 0){
    max.val = max(data)
    bits = length(decimal2binary(max.val))
  }
  bin.data = unlist(sapply(t(data),function(x){binary2gray(decimal2binary(x,length=bits))}))
  dim(bin.data) = c((ncol(dataset)-1)*bits,nrow(dataset))
  bin.data <- t(bin.data)
  return(as.data.frame(cbind(bin.data,dataset[,ncol(dataset)])))
}

binarize.jmm <- function(dataset,factor=0){
  library("GA")
  data = dataset[-ncol(dataset)]
  # Add the most negative value 
  #val = min(data)
  #data = data + val
  # Multiply by the factor
  data = data * 10^factor
  maxval = max(data)
  #print(maxval)
  bin.data = unlist(sapply(t(data),function(x){c(rep(0,maxval-x),rep(1,x))}))
  dim(bin.data) = c((ncol(dataset)-1)*maxval,nrow(dataset))
  bin.data <- t(bin.data)
  return(as.data.frame(cbind(bin.data,dataset[,ncol(dataset)])))
}
nbits <- function(dataset,factor){
  library("GA")
  data = dataset[-ncol(dataset)]
  # Add the most negative value, if needed
  min.val = min(data)
  if(min.val < 0) data = data + abs(min.val)
  #truncate to <factor> decimals
  data = signif(data,factor+1)
  # Multiply by the factor
  data = data * 10^factor
  n = max(data)
  bits = length(decimal2binary(n))
  return(bits)
}
random.coding <- function(dataset, factor = 0, n = -1, counter = c()){
  library("GA")
  data = dataset[-ncol(dataset)]
  # Add the most negative value, if needed
  min.val = min(data)
  if(min.val < 0) data = data + abs(min.val)
  #truncate to <factor> decimals
  data = signif(data,factor+1)
  # Multiply by the factor
  data = data * 10^factor
  if(n < 0) n = max(data)
  bits = length(decimal2binary(n))
  if(length(counter) == 0){  
    normal = unlist(sapply(0:(2^bits-1),function(x){decimal2binary(x,length=bits)}))
    dim(normal) = c(bits,2^bits)
    normal = t(normal)
  }
  else{
    if(bits==ncol(counter)) normal = counter[1:2^bits,]
      else normal = counter[1:2^bits,-(1:(ncol(counter)-bits))]
    }
  

  map = sample(1:(n+1),replace=F)
  rand.code = normal[map,]

  bin.data = unlist(sapply(t(data),function(x,code){
    code[x+1,]
  },code=rand.code))
  dim(bin.data) = c((ncol(dataset)-1)*bits,nrow(dataset))
  bin.data <- as.data.frame(t(bin.data))
  return(as.data.frame(cbind(bin.data,dataset[,ncol(dataset)])))
}

additive.coding <- function(dataset,factor=0, bits = -1, n=0){
  library("GA")
  library("binaryLogic")
  data = dataset[-ncol(dataset)]
  # Add the most negative value 
  #val = min(data)
  #data = data + val
  # Multiply by the factor
  data = data * 10^factor
  if(bits < 0){
    max.val = max(data)
    bits = length(as.binary(max.val))
  }
  bin.data = unlist(sapply(t(data),function(x){decimal2binary(x,length=bits)}))
  #bin.data = unlist(sapply(t(data),function(x){as.binary(x,n=bits)}))
  
  dim(bin.data) = c((ncol(dataset)-1)*bits,nrow(dataset))
  bin.data <- t(bin.data)
  bits.final = ncol(bin.data)
  for(i in 1:nrow(bin.data)){
  #  bin.data[i,] = binAdd(as.binary(bin.data[i,],logic=TRUE),as.binary(n))
    bin.data[i,] = decimal2binary(binary2decimal(bin.data[i,])+n, length=bits.final)
  }
  ret=as.data.frame(cbind(bin.data,dataset[,ncol(dataset)]))
  return(ret)
}

bin.count.gen <- function(bits){
  library("GA")
  normal = unlist(sapply(0:(2^bits-1),function(x){decimal2binary(x,length=bits)}))
  dim(normal) = c(bits,2^bits)
  normal = t(normal)
  return(normal)
}