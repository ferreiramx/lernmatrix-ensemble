library("future.apply")
plan("multiprocess")
res.gamma = future.apply::future_lapply(lst,function(x){
  write(paste("Processing: ", x[1]),"log_gamma.txt", append = T);
  data = eval(parse(text=x[1]));
  ret = multi.size.run(data,eval(parse(text=x[2])),bin.count, 100, x[1]);
  #ret = multi.run(1000,100,data,eval(parse(text=x[2])),bin.count,x[1]);
  write(paste("Finished: ", x[1]),"log_gamma.txt", append = T);
  return(ret);
  }, future.globals = ls());
# print("Processing: banknote.auth")
# stats.banknote = multi.size.run(banknote.auth,4,bin.count,100)
# print("Finished processing: banknote.auth")
# 
# print("Processing: blood")
# stats.blood = multi.size.run(blood,0,bin.count,100)
# print("Finished processing: banknote")
# 
# print("Processing: breast.tissue")
# stats.breast.tissue = multi.size.run(breast.tissue,0,bin.count,100)
# print("Finished processing: breast.tissue")
# 
# print("Processing: cardio3")
# stats.cardio3 = multi.size.run(cardio3,3,bin.count,100)
# print("Finished processing: cardio3")
# 
# print("Processing: column2")
# stats.column2 = multi.size.run(column2,3,bin.count,100)
# print("Finished processing: column2")
# 
# print("Processing: column3")
# stats.column3 = multi.size.run(column3,3,bin.count,100)
# print("Finished processing: column3")
# 
# print("Processing: creditcard")
# stats.creditcard = multi.size.run(creditcard,0,bin.count,100)
# print("Finished processing: creditcard")
# 
# print("Processing: drugs.amphetamine")
# stats.drugs.amphetamine = multi.size.run(drugs.amphetamine,4,bin.count,100)
# print("Finished processing: drugs.amphetamine")
# 
# print("Processing: drugs.cannabis")
# stats.drugs.cannabis = multi.size.run(drugs.cannabis,4,bin.count,100)
# print("Finished processing: drugs.cannabis")
# 
# print("Processing: drugs.cocaine")
# stats.drugs.cocaine = multi.size.run(drugs.cocaine,4,bin.count,100)
# print("Finished processing: drugs.cocaine")
# 
# print("Processing: drugs.ecstasy")
# stats.drugs.ecstasy = multi.size.run(drugs.ecstasy,4,bin.count,100)
# print("Finished processing: drugs.ecstasy")
# 
# print("Processing: drugs.mushrooms")
# stats.drugs.mushrooms = multi.size.run(drugs.mushrooms,4,bin.count,100)
# print("Finished processing: drugs.mushrooms")
# 
# print("Processing: ecoli")
# stats.ecoli = multi.size.run(ecoli,2,bin.count,100)
# print("Finished processing: ecoli")
# 
# print("Processing: glass")
# stats.glass = multi.size.run(glass,3,bin.count,100)
# print("Finished processing: glass")
# 
# print("Processing: haberman")
# stats.haberman = multi.size.run(haberman,0,bin.count,100)
# print("Finished processing: haberman")
# 
# print("Processing: ionosphere")
# stats.ionosphere = multi.size.run(ionosphere,5,bin.count,100)
# print("Finished processing: ionosphere")
# 
# print("Processing: iris")
# stats.iris = multi.size.run(iris,1,bin.count,100)
# print("Finished processing: iris")
# 
# print("Processing: leaf")
# stats.leaf = multi.size.run(leaf,4,bin.count,100)
# print("Finished processing: leaf")
# 
# print("Processing: letter")
# stats.letter = multi.size.run(letter,1,bin.count,100)
# print("Finished processing: letter")
# 
# print("Processing: mfeat.fac")
# stats.mfeat.fac = multi.size.run(mfeat.fac,0,bin.count,100)
# print("Finished processing: mfeat.fac")
# 
# print("Processing: mfeat.fou")
# stats.mfeat.fou = multi.size.run(mfeat.fou,5,bin.count,100)
# print("Finished processing: mfeat.fou")
# 
# print("Processing: mfeat.kar")
# stats.mfeat.kar = multi.size.run(mfeat.kar,3,bin.count,100)
# print("Finished processing: mfeat.kar")
# 
# print("Processing: mfeat.mor")
# stats.mfeat.mor = multi.size.run(mfeat.mor,1,bin.count,100)
# print("Finished processing: mfeat.mor")
# 
# print("Processing: mfeat.pix")
# stats.mfeat.pix = multi.size.run(mfeat.pix,1,bin.count,100)
# print("Finished processing: mfeat.pix")
# 
# print("Processing: mfeat.zer")
# stats.mfeat.zer = multi.size.run(mfeat.zer,2,bin.count,100)
# print("Finished processing: mfeat.zer")
# 
# print("Processing: parkinsons")
# stats.parkinsons = multi.size.run(parkinsons,2,bin.count,100)
# print("Finished processing: parkinsons")
# 
# print("Processing: sonar")
# stats.sonar = multi.size.run(sonar,4,bin.count,100)
# print("Finished processing: sonar")
# 
# print("Processing: wdbc")
# stats.wdbc = multi.size.run(wdbc,1,bin.count,100)
# print("Finished processing: wdbc")
# 
# print("Processing: wine")
# stats.wine = multi.size.run(wine,2,bin.count,100)
# print("Finished processing: wine")
# 
# print("End of experiment.")