_?[]=0;i?(x:s)=i?s+x*sum[y|y<-s,y+x==i]
q[]=0;q(x:s)=q s+x*(2020-x)?s
l=map read.lines<$>readFile"Day1.dat"
r=(l>>=).(print.)
f=r(2020?)
g=r q