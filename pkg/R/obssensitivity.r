obssensitivity <-
function(tauval=0.5,omegaval=1)
{
p1prime <- 1-pnorm(0,2*tauval,sqrt(2*omegaval))
p1prime/(1-p1prime)
}

