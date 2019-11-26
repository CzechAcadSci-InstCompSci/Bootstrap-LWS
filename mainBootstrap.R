### main function of the nonparametric bootstrap code, version 1.0
### the code automatically generates data (using various possibilities for univariate and multivariate data)
### the data are automatically contaminated (using various contamination schemes)
### the nonparametric bootstrap is performed, comparing various robust regression estimators
### including MM-estimators, S-estimators, least trimmed squares (LTS) and least weighted squares (LWS)

library(robustbase);

### parameters chosen by the user
  n=60;
  PocetOut=18; #how many contaminated data points  
  ndata=30; #number of datasets
  nboot=30; #number of bootstrap samples for each dataset
  beta=c(2,1);
  
OneBoot=function() #bootstrap for one dataset
{vytvor=CreateA();
  x=vytvor$x; y=vytvor$y;
  pom=ContaminateE(x,y); cont=pom$out; x=pom$regresor;
### bootstrap samples
odhadymat=matrix(-1, nrow=nboot, ncol=9);
for (i in 1:nboot)
  {indexy=sample(n,size=n,replace=T);
  #print(sort(indexy));
  regresor=x[indexy];  #to je univariate
  odezva=y[indexy];
  odhadymat[i,] = BodoveVse(regresor,odezva)$outvec;
  ###if (chcilws==0) {odhadymat[i,] = BodoveOdhady(regresor,odezva)$outvec}
  ###if (chcilws==1) {odhadymat[i,] = BodoveLws(regresor,odezva)$outvec}
  }#for
#print(odhadymat); #to je pro jeden datovy soubor
prumery=apply(odhadymat, 2, mean);
#barplot(prumery, main="bodove intercept", ylim=c(0,2.2));
list(prumery=prumery)
}#OneBoot

outpru=outroz=matrix(-1, nrow=ndata, ncol=9);
for (j in 1:ndata)
  {print(j);
  pom=OneBoot(); 
  outpru[j,]=pom$prumery; 
  }#for
print("end of bootstrap computations");
print("outpru,prumery"); print(outpru); 
rm=apply(outpru,2,var);
print("rozptyly smernice"); print(rm);
barplot(rm, main="var smernice"); 
