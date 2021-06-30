#'simulation d´une chaine de markov a espace des etats fini
#'@export
#'@param P matrix of transition
#'@param mu numeric vector of initial distrebution
#'@param n numeric number of repetition
SimCMk=function(P,mu,n){
  m=length(mu)
  y=c(seq(1,m,1))
  x=c(rep(0,n+1))
  t=c(0:n)
  x[1]=rdist(y,mu)
  for(i in 1:n){
    x[i+1]=rdist(y,P[x[i],]) }
  plot(t,x,pch=8,xlim=c(0,n),ylim=c(1,m+1),col=3)
}

