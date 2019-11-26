### creating a multivariate dataset
library(robustbase);

### parameters chosen by the user:
  n=30;
  PocetOut=3; #how many contaminated data points
  nr=10;
  beta=c(10,1,1,1);

### generate random datasets
outmat=outlimat=matrix(0, nrow=nr, ncol=5);
for (i in 1:nr)
 {print(i); 
  pom=CreateB();
    x=pom$x; y=pom$y; 
  cont=ContaminateB(x,y)$out;
  pom=estimate(cont);
    outmat[i,]=pom$outsigma;
    outlimat[i,]=pom$outlier;
 }#for
#print(outmat);
print("quantiles"); print(apply(outmat, 2, quantile));
print("outlier"); print(apply(outlimat, 2, mean)/n);