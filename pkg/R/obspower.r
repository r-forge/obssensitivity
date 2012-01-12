obspower <-
function(npairs,tauval=0.5,
omegaval=1,alpha=0.05,gamma.val=1)
{
critical <- qnorm(1-alpha,mean=0,sd=1,lower.tail=TRUE,log.p=FALSE)
p <- 1-pnorm(0,tauval,sqrt(omegaval))
p1prime <- 1-pnorm(0,2*tauval,sqrt(2*omegaval))
p2prime <- pmvnorm(lower=c(0,0),mean=rep(2*tauval,2),
sigma=matrix(c(2*omegaval,omegaval,omegaval,2*omegaval),2,2))
muf <- (npairs*(npairs-1)*p1prime/2)+npairs*p
sig2f <- (npairs*(npairs-1)*(npairs-2)*(p2prime-p1prime^2))+
         ((npairs*(npairs-1)/2)*(2*(p-p1prime)^2+
         3*p1prime*(1-p1prime)))+(npairs*p*(1-p))
eta.val <- (gamma.val/(1+gamma.val))*(npairs*
           (npairs+1)/2)+critical*sqrt((gamma.val/(1+gamma.val)^2)*
           ((npairs*(npairs+1)*(2*npairs+1))/6))
pow.value <- 1-pnorm((eta.val-muf)/sqrt(sig2f),
              mean=0,sd=1,log = FALSE)
pow.value <- pow.value[[1]][1]
pow.value
}

