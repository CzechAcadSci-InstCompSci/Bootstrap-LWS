### creating a univariate dataset
library(robustbase);

### parameters chosen by the user
  n=30;
  PocetOut=0; #how many contaminated data points  
  nr=30; #number of datasets
  beta=c(2,1);
  chcilws=0; #0 or 1  

############# generate random datasets
outsig=outlimat=matrix(0, nrow=nr, ncol=5);
for (i in 1:nr)
 {print(i); 
  pom=CreateA();
    x=pom$x; y=pom$y; 
  sm=ContaminateE(x,y); cont=sm$out; x=sm$regresor;
  if (chcilws==0) {pom=estimate(cont)};
  if (chcilws==1) {pom=EstimateLws(cont)};
    outsig[i,]=pom$outsigma;
    outlimat[i,]=pom$outlier;
 }#for
print("outsig"); print(outsig);
print("quantiles"); print(apply(outsig, 2, quantile));
print("outlier"); print(apply(outlimat, 2, mean)/n);