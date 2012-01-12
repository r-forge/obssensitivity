obssamplesize <-
function(gamma.val=1,power=0.8,tauval=0.5,
omegaval=1,alpha=0.05,lower=10,upper=1000)
{
if(gamma.val>obssensitivity(tauval,omegaval)) stop("Gamma Value must be less than the Sensitivity")

power.fun.power <- function(npairs,power,tauval=0.5,
omegaval=1,alpha=0.05,gamma.val=1, ...){
  obspower(npairs,tauval,
omegaval,alpha,gamma.val)-power}
ss <- uniroot(power.fun.power,lower=lower,upper=upper,power=power,gamma.val=gamma.val)$root
ss
}

