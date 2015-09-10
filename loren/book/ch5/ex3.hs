pyths n = [(x,y,z)|x<-[1..n],y<-[1..n],z<-[1..n],x^2+y^2==z^2]
pyths1 n = [(x,y,z)|x<-[1..n],y<-[x..n],z<-[x..n],x^2+y^2==z^2]
