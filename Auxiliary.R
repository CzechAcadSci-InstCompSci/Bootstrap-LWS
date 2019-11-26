### auxiliary files for the computations

CreateA=function() #creating a univariate dataset
{mysd=1;
e=rnorm(n,mean=0,sd=mysd);
x=runif(n, 0, 10);
ode=rep(-1,n);
for (i in 1:n)
  ode[i]=beta[1]+beta[2]*x[i]+e[i];
list(x=x,y=ode)
}#CreateA

CreateB=function() #creating a multivariate dataset
{mysd=1;
e=rnorm(n,mean=0,sd=mysd);
x1=runif(n, 0, 10); x2=runif(n, 0, 10); x3=runif(n, 0, 10);
ode=rep(-1,n);
for (i in 1:n)
  ode[i]=beta[1]+beta[2]*x1[i]+beta[3]*x2[i]+beta[4]*x3[i]+e[i];
regr=cbind(x1,x2,x3); 
list(xmat=regr,y=ode)
}#CreateB

ContaminateA=function(x,ode) 
# contaminating a univariate dataset
# the bootstrap for contaminated data reveals the LWS to be more appropriate than the LTS
{indexy=sample(n,PocetOut);
#plot(x,ode,xlab=" ", ylab=" ", ylim=c(-1,16));
#print("indexy"); print(indexy); print("outliers");
if (PocetOut>0)
  for (k in 1:PocetOut)
    {ode[indexy[k]]=beta[1]+beta[2]*x[indexy[k]]+8*((runif(1)>0.5)-0.5);
   # points(x[indexy[k]], ode[indexy[k]], col="red", pch=19); 
   # print(c(x[indexy[k]], ode[indexy[k]]));
    }#for
list(regresor=x,out=ode);
} #ContaminateA

ContaminateC=function(x,ode) # contaminating a univariate dataset
{indexy=sample(n,PocetOut);
ee=rnorm(PocetOut,mean=0,sd=5);
#plot(x,ode,xlab=" ", ylab=" ", ylim=c(-1,16));
#print("indexy"); print(indexy); print("outliers");
if (PocetOut>0)
  for (k in 1:PocetOut)
    {ode[indexy[k]]=beta[1]+beta[2]*x[indexy[k]]+ee[k];
    #points(x[indexy[k]], ode[indexy[k]], col="red", pch=19); 
    #print(c(x[indexy[k]], ode[indexy[k]])); 
    }#for
list(regresor=x,out=ode);
} #ContaminateC

ContaminateD=function(xloc,ode) # contaminating a univariate dataset
#the regressor is contaminated only for small values of a given regressor
{ee=rnorm(PocetOut,mean=0,sd=5);
plot(xloc,ode,xlab=" ", ylab=" ", ylim=c(-1,16));
xnew=xloc;
print("PocetOut"); print(PocetOut);
if (PocetOut>0)
  for (k in 1:PocetOut)
    {xnew[k]=runif(1,min=0.5,max=2);
    ode[k]=beta[1]+beta[2]*xnew[k]+ee[k];
    points(xnew[k], ode[k], col="red", pch=19); 
    }#for
print("xloc"); print(xloc); print("xnew"); print(xnew);
list(regresor=xloc,out=ode);
} #ContaminateD

ContaminateE=function(xin,ode) # contaminating a univariate dataset
# regressor = m + noise (not severe), while m is taken from the whole range of the regressor 
{ee=runif(PocetOut,3,6);
xzaklad=runif(1,0,10);
xx=xin;
#par(mfrow=c(2,2));
if (PocetOut>0)
  {#plot(xx,ode,xlab=" ", ylab=" ", ylim=c(-1,16));
  xx[1:PocetOut]=xzaklad+rnorm(PocetOut,mean=0,sd=0.5);
  for (k in 1:PocetOut)
    {ode[k]=beta[1]+beta[2]*xx[k]+ee[k];
   #points(xx[k], ode[k], col="red", pch=19); ###print(c(xx[k], ode[k])); 
    }#for
  }#if
print("x"); print(x); print("xx"); print(xx);
#plot(xx,ode);
#readline()
list(regresor=xx,out=ode);
} #ContaminateE

ContaminateB=function(x,ode) # contaminating a multivariate dataset
{indexy=sample(n,PocetOut);
for (k in 1:PocetOut)
  ode[indexy[k]]=beta[1]+beta[2]*x[indexy[k],1]+beta[3]*x[indexy[k],2]+beta[2]*x[indexy[k],3]+4*(runif(1)>0.5);
plot(x,ode,xlab=" ", ylab=" "); readline()
list(regresor=x,out=ode);
} #ContaminateB; 2D obrazky jsou zde k nicemu

BodoveVse = function(x,y) ###### various robust estimators
{two=1; #slope vs. intercept
outvec=rep(-1,9);
mylm=lm(y~x); outvec[1]=mylm$coef[two];
### S-estimator for n>20; 30 uz dobre
  x1=cbind(1,x);
  mys=lmrob.S(x1, y, only.scale=F,control = lmrob.control(nResample = 500), trace.lev=0);
  outvec[2]=mys$coef[two];
ltsa=ltsReg(y~x,alpha=0.60); outvec[3]=ltsa$coef[two];
ltsb=ltsReg(y~x,alpha=0.80); outvec[4]=ltsb$coef[two];
mymm=lmrob(y~x); outvec[5]=mymm$coef[two];
myw=magnit(1)$t; outvec[6]=lwsreg(x,y,myw)$coef[two];
myw=magnit(2)$t; outvec[7]=lwsreg(x,y,myw)$coef[two];
myw=magnit(3)$t; outvec[8]=lwsreg(x,y,myw)$coef[two];
myw=magnit(4)$t; outvec[9]=lwsreg(x,y,myw)$coef[two];
list(outvec=outvec)
} #BodoveVse

BodoveOdhady = function(x,y) ###### various robust estimators
{outvec=rep(-1,10);
mylm=lm(y~x); 
  outvec[1:2]=mylm$coef;
ltsa=ltsReg(y~x,alpha=0.60);
  outvec[3:4]=ltsa$coef;
ltsb=ltsReg(y~x,alpha=0.80);
  outvec[5:6]=ltsb$coef;
mymm=lmrob(y~x);
  outvec[7:8]=mymm$coef;
### jeste LTS
 # ltsc=ltsReg(y~x,alpha=0.85);
 # outvec[9:10]=ltsb$coef;
### S-estimator for n>20; 30 uz dobre
  x1=cbind(1,x);
  mys=lmrob.S(x1, y, only.scale=F,control = lmrob.control(nResample = 500), trace.lev=0);
  outvec[9:10]=mys$coef;
list(outvec=outvec)
} #BodoveOdhady

BodoveLws = function(x,y) ###### various versions of the LWS estimator
{outvec=rep(-1,10);
myw=magnit(1)$t; outvec[1:2]=lwsreg(x,y,myw)$coef;
myw=magnit(2)$t; outvec[3:4]=lwsreg(x,y,myw)$coef;
myw=magnit(3)$t; outvec[5:6]=lwsreg(x,y,myw)$coef;
myw=magnit(4)$t; outvec[7:8]=lwsreg(x,y,myw)$coef;
mylm=lm(y~x); outvec[9:10]=mylm$coef;
list(outvec=outvec)
} #BodoveLws

estimate=function(y) ###### main function for computing various estimators (expect the LWS)
{outvec=outlier=rep(-1,5);
my=lm(y~x); 
  res=my$res;
  pom=sum(res^2)/(n-2);
  #print(c("ls", pom));
  outlier[1]=sum(abs(res)>(pom*2.5));
mylts=ltsReg(y~x,alpha=0.60);
  #print(c("lts", mylts$scale));
  outlier[2]=sum(abs(mylts$resid)>(mylts$scale*2.5));
mybig=ltsReg(y~x,alpha=0.80);
  outlier[3]=sum(abs(mybig$resid)>(mybig$scale*2.5));
mymm=lmrob(y~x);
  #print(c("mm", mymm$scale));
  outlier[4]=sum(abs(mymm$res)>(mymm$scale*2.5));
mys=lmrob.S(x, y, control = lmrob.control(nResample = 500), trace.lev=0);
  #print(c("s", mys$scale));
  outlier[5]=sum(abs(mys$res)>(mys$scale*2.5));
outsigma=c(pom, mylts$scale, mybig$scale, mymm$scale,mys$scale);
list(outsigma=outsigma,outlier=outlier)
} #estimate

EstimateLws=function(y) ###### main function for computing various versions of the LWS
{outsigma=outlier=rep(-1,5);
lwsiter=10;
myw=magnit(1)$t; pom=lwsreg(x,y,myw,lwsiter); outsigma[1]=pom$loss*11.01/n; outlier[1]=sum(abs(pom$res)>2.5); 
 # print("EstimateLws"); print("coef"); print(pom$coef); print("loss"); print(pom$loss); 
 # print("x,y,res,optw"); print(cbind(x,y,pom$res, pom$optw)); #print(cbind(rank(abs(pom$res)), n+1-rank(abs(pom$optw))));
myw=magnit(2)$t; pom=lwsreg(x,y,myw,lwsiter); outsigma[2]=pom$loss*37.75/n; outlier[2]=sum(abs(pom$res)>2.5);
myw=magnit(3)$t; pom=lwsreg(x,y,myw,lwsiter); outsigma[3]=pom$loss*173.3/n; outlier[3]=sum(abs(pom$res)>2.5);
myw=magnit(4)$t; pom=lwsreg(x,y,myw,lwsiter); outsigma[4]=pom$loss*1.250/n; outlier[4]=sum(abs(pom$res)>2.5);
my=lm(y~x); 
  res=my$res;
  pom=sum(res^2)/(n-2);
  outsigma[5]=pom;
  outlier[5]=sum(abs(res)>(pom*2.5));
list(outsigma=outsigma,outlier=outlier)
} #EstimateLws