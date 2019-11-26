### LWS estimator (in this version, only for p=1)

mflws=function(res,w)
{## 1. computes the loss function
### 2. another output are the weights in the correct order, so that
###    WLS can be computed
res2 = res^2; #squared residuals
myrank=rank(res2, ties.method="random"); #assign weights based on residuals
myw=rep(0,n);
myw=w[myrank]; #correct weights according to the rank of the residual
loss=0;
for (i in 1:n)
  loss = loss + myw[i]*res2[i];
list(loss=loss, outw=myw)
} #mflws

TakeTwoPoints=function(x,y) 
{## randomly selects two points, fit the least squares through them, computes residuals
stop=0;
while (stop == 0)
  {mysample=sample(n,2, replace=FALSE); 
   xx=c( x[mysample[1]], x[mysample[2]] );
   yy=c( y[mysample[1]], y[mysample[2]] );
   if (x[mysample[1]] - x[mysample[2]] > 0.01)
     {mylm=lm(yy~xx);
     slope= mylm$coefficients[2];
     inter= mylm$coefficients[1];
     stop= ( (slope<100)&(slope > -100) );}
   else
     stop=0; #the same values of the regressor, OLS cannot be computed  
  } #while
res = y - inter - slope*x;  
list(res=res) #output: residuals
} #TakeTwoPoints

lwsiterations=function(x,y,w,Iter)
{bignr=1000000;
optloss=bignr;
coef=c(0,0);
res=rep(0,n);
rmout=rep(0, Iter);
for (i in 1:Iter)
  {fnc1=TakeTwoPoints(x,y); 
     res=fnc1$res; #residuals
   fnc2=mflws(res,w);
     locw=fnc2$outw; #weights in the correct order
     loss=fnc2$loss; #loss for such weights
   localloss=bignr;
   while (loss<localloss)
     {localloss=loss;
      if ((loss<optloss)&(loss>0))
        {optloss=loss;
         optweights=locw;
         optres=res;
         optcoef=coef;
        } #if
      weightedregr=lm(y~x, weights=locw); #weighted regression
      res=weightedregr$residuals;
      coef=weightedregr$coef;
      loss=mflws(res,w)$loss;
     # print(c("in lwsiterations, loss", loss));
     } #while 
   } #for 
list(optw=optweights) #jine vystupy tem vaham nekoresponduji
} #lwsiterations

lwsreg=function(x,y,w,Iter=10)
{#print("lwsreg"); print(w); 
### kdyz chci i odhad rozptylu: nemohu si rucne zadat nejake libovolne vahy, musi byt podle weight function, nejvetsi vaha=1
smfnc=lwsiterations(x,y,w,Iter);
  optw=smfnc$optw;
my=lm(y~x, weights=optw);
  coef=my$coef;
  res=my$res;
  loss=mflws(res,w)$loss;
list(coef=coef,loss=loss,optw=optw,res=res)
}#lwsreg
### vahy nemuseji byt usporadany podle abs. hodnot rezidui

