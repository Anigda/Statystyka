M=100
s=0.3
wektor=rnorm(M)
while(var(wektor)> 1.1 || var(wektor)< 0.9 || mean(wektor)> 0.1 ||
      mean(wektor)< -0.1 || shapiro.test(wektor)$p.value<0.85)
{wektor=rnorm(M)}
v1=var(wektor)+0.1
v2=var(wektor)-0.1
m1=mean(wektor)-0.2
m2=mean(wektor)+0.2
wektor1=NULL
wektor2=NULL
wektor3=NULL
wektor4=NULL
wektor5=NULL
wektor6=NULL
wektor7=NULL

for(i in 1:100)
{n=ceiling(M/2); wykladniczy=rexp(n);
while(var(c(wektor,wykladniczy))> v1 ||
      var(c(wektor,wykladniczy))< v2 ||
      mean(c(wektor,wykladniczy))> m2 ||
      shapiro.test(c(wektor,wykladniczy))$p.value<s)
{wykladniczy=rexp(n); n=n-1};
wektor1[i]=n+1}

for(i in 1:100)
{n=ceiling(M/2); parzystywykladniczy=c(rexp(n),-rexp(n));
while(var(c(wektor,parzystywykladniczy))> v1 ||
      var(c(wektor,parzystywykladniczy))< v2 ||
      mean(c(wektor,parzystywykladniczy))> m2 ||
      mean(c(wektor,parzystywykladniczy))< m1 ||
      shapiro.test(c(wektor,parzystywykladniczy))$p.value<s)
{parzystywykladniczy=c(rexp(n),-rexp(n)); n=n-1};
wektor2[i]=2*(n+1)}

for(i in 1:100)
{n=ceiling(M/3); jednostajny=runif(n,-3,3);
while(var(c(wektor,jednostajny))> v1 ||
      var(c(wektor,jednostajny))< v2 ||
      mean(c(wektor,jednostajny))> m2 ||
      mean(c(wektor,jednostajny))< m1 ||
      shapiro.test(c(wektor,jednostajny))$p.value<s)
{jednostajny=runif(n,-3,3); n=n-1};
wektor3[i]=n+1}

for(i in 1:100)
{n=ceiling(M/3); gam=rgamma(n,0.3);
while(var(c(wektor,gam))> v1 ||
      var(c(wektor,gam))< v2 ||
      mean(c(wektor,gam))> m2 ||
      shapiro.test(c(wektor,gam))$p.value<s)
{gam=rgamma(n,0.3); n=n-1};
wektor4[i]=n+1}

for(i in 1:100)
{n=ceiling(M/5); parzystygam=c(rgamma(n,0.3),-rgamma(n,0.3));
while(var(c(wektor,parzystygam))> v1 ||
      var(c(wektor,parzystygam))< v2 ||
      mean(c(wektor,parzystygam))> m2 ||
      mean(c(wektor,parzystygam))< m1 ||
      shapiro.test(c(wektor,parzystygam))$p.value<s)
{parzystygam=c(rgamma(n,0.3),-rgamma(n,0.3)); n=n-1};
wektor5[i]=2*(n+1)}

for(i in 1:100)
{n=ceiling(M/3); geometryczny=rgeom(n,0.85);
while(var(c(wektor,geometryczny))> v1 ||
      var(c(wektor,geometryczny))< v2 ||
      mean(c(wektor,geometryczny))> m2 ||
      shapiro.test(c(wektor,geometryczny))$p.value<s)
{geometryczny=rgeom(n,0.85); n=n-1};
wektor6[i]=n+1}

for(i in 1:100)
{n=ceiling(M/5); parzystygeometryczny=c(rgeom(n,0.85),-rgeom(n,0.85));
while(var(c(wektor,parzystygeometryczny))> v1 ||
      var(c(wektor,parzystygeometryczny))< v2 ||
      mean(c(wektor,parzystygeometryczny))> m2 ||
      mean(c(wektor,parzystygeometryczny))< m1 ||
      shapiro.test(c(wektor,parzystygeometryczny))$p.value<s)
{parzystygeometryczny=c(rgeom(n,0.85),-rgeom(n,0.85)); n=n-1};
wektor7[i]=2*(n+1)}

cat("rozklad wykladniczy:\t\t\t\t ?rednia =",ceiling(mean(wektor1))," min =",min(wektor1)," max =",max(wektor1),
    "\nrozklad parzysty wykladniczy:\t\t\t ?rednia =", ceiling(mean(wektor2))," min =", min(wektor2)," max =",max(wektor2),
    "\nrozklad jednostajny:\t\t\t\t ?rednia =", ceiling(mean(wektor3))," min =", min(wektor3)," max =",max(wektor3),
    "\nrozklad gamma:\t\t\t\t\t ?rednia =", ceiling(mean(wektor4))," min =", min(wektor4)," max =",max(wektor4),
    "\nrozklad parzysty gamma:\t\t\t\t ?rednia =", ceiling(mean(wektor5))," min =", min(wektor5)," max =",max(wektor5),
    "\nrozklad geometryczny:\t\t\t\t ?rednia =", ceiling(mean(wektor6))," min =", min(wektor6)," max =",max(wektor6),
    "\nrozklad parzysty geometryczny:\t\t\t ?rednia =", ceiling(mean(wektor7))," min =", min(wektor7)," max =",max(wektor7))