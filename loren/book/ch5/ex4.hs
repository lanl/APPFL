factors1 n = [x|x<-[1..n],n`mod`x==0]
perfects n = [x|x<-[1..n],sum(factors1 x)==2*x]
