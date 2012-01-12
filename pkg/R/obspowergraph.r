obspowergraph <-
function(gammavalues,numb.pairs,tauval){
  powervals <- matrix(0,length(numb.pairs),length(gammavalues))
  for (i in 1:length(gammavalues)){
   powervals[,i] <- sapply(numb.pairs,obspower,tauval=tauval,gamma.val=gammavalues[i]) 
  }
matplot(numb.pairs,powervals,
type="l",lty=1:length(gammavalues),lwd=2,
xlim=range(numb.pairs)*c(1,1.075),
xlab="Number of Matched Pairs",ylab="Power",col=1)
for (i in 1:length(gammavalues)){
  gam=gammavalues[i]
 text(max(numb.pairs)*1.05,powervals[length(numb.pairs),i],
 substitute(Gamma==t,list(t=gammavalues[i])))
}
}

