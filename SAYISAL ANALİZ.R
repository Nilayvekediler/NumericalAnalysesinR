#kesme fonksiyonu------------------
kesme <- function(x) {
  as.numeric(formatC(x, digits = 4, format = "f"))
}
#yuvarlama fonksiyonu-----------------
yuvarlama <- function(x) {
  signif(x,digits=2)
}
x<-12.53
yuvarlama(x)
#interpolasyon hesaplama kolaylýðý--------------------
y1<- 24
y0<- 3

x1<- 4
x0<- 1

f<- function(x0,x1) {(y1-y0)/(x1-x0)}

print(paste("(",y1,"-",y0,")/(",x1,"-",x0,") =",f(x0,x1)))

#sayýsal türev-----------------------------------------
h=0.1
x=0.4

f<- function(x) {((29/60)*(x^5))+((1/30)*(x^4))+((259/1200)*(x^3))+((1423/1500)*(x^2))+((221/37500)*(x))+(3999/4000)}

#ileri fark 
ftürevxileri<- function(x,h) {(f(x+h)-f(x))/h}
print(paste("ileri fark = (f(",x,"+",h,")-f(",x,"))/",h," =",ftürevxileri(x,h)))

#geri fark 
ftürevxgeri<- function(x,h) {(f(x)-f(x-h))/h}
print(paste("geri fark = (f(",x,")-f(",x,"-",h,"))/",h," =",ftürevxgeri(x,h)))

#merkezi fark
ftürevxmerkezi<- function(x,h) {(f(x+h)-f(x-h))/(2*h)}
print(paste("merkezi fark = (f(",x,"+",h,")-f(",x,"-",h,"))/ (",2*h,") =",
            ftürevxmerkezi(x,h)))

#2.türev için fark formülü
fikincitürev<- function(x,h) {(f(x+h)-2*f(x)+f(x-h))/(h*h)}
print(paste("ikinci türev = (f(",x,"+",h,")-2*f(",x,")+f(",x,"-",h,"))/",h*h,
            " =",fikincitürev(x,h)))

#sayýsal integral-------dikdörtgen,yamuk,simpson kuralý-----------
#nokta sayýsý hesaplama 
noktaSayisiHesaplama <- function(alt,üst,h) {
  noktasayisi=((üst-alt)/h)+1
  noktasayisi
}

#h=aralýk deðerleri hesaplama 
hHesapla<- function(alt,üst,noktasayisi) {
  h=(üst-alt)/(noktasayisi-1)
  h 
}


#dikdörtgen kuralý hesaplama kolaylýðý----------
x0=3
x1=4
f<-function(x) log(x)
(x1-x0)
f(x0)
print("alan:")
s<- function(x0,x1) (x1-x0)*f(x0)
s(x0,x1)

#yamuk kuralý hesaplama--------------------------- 
f<-function(x){(x+2)/(sqrt(2*x+3))}
h<-0.6666667
#soruya göre ekle sil
x0<- -1
x1<-x0+h
x2<-x1+h
x3<-x2+h
x4<-x3+h
x5<-x4+h
x6<-x5+h

print(paste(x0,"-",x1,"-",x2,"-",x3,"-",x4,"-",x5,"-",x6))
(h/2)
f(x0)
f(x1)
f(x2)
f(x3)
f(x4)
f(x5)
f(x6)
(f(x0)+2*f(x1)+2*f(x2)+2*f(x3)+2*f(x4)+2*f(x5)+1*f(x6))
s<-function(h,x0,x1,x2,x3,x4,x5,x6){(h/2)*(f(x0)+2*f(x1)+2*f(x2)+2*f(x3)+2*f(x4)+2*f(x5)+1*f(x6))}

sonuc<-s(h,x0,x1,x2,x3,x4,x5,x6)
sonuc 

#Eger gerçek deger verilir hata istenirse
gercekDeger=5.33333
error=abs(gercekDeger-5.33774)
error
#simpson kuralý hesaplama ----------------------
#toplamda kullanýlan nokta sayýsý tekse SÝMPSON 1/3 KURALI 
# h/3 .(1.fx0 + 4.fx1 + 2.fx2 + 4.fx3 + 2.fx4 + 1.fx5)  *1 4 2 4 2 1*
f<-function(x){(x+2)/(sqrt(2*x+3))}

alt= -1
üst= 3
noktasayisi= 7

h=hHesapla(alt,üst,noktasayisi)
x0=alt
x1=x0+h
x2=x1+h
x3=x2+h
x4=x3+h
x5=x4+h
x6=x5+h

print(paste("-",x0,"-",x1,"-",x2,"-",x3,"-",x4,"-",x5,"-",x6))

h/3
f(x0)
f(x1)
f(x2)
f(x3)
f(x4)
f(x5)
f(x6)

(1*f(x0)) + (4*f(x1)) + (2*f(x2)) + (4*f(x3)) + (2*f(x4)) + (4*f(x5)) + (1*f(x6))
s<-function(h,x0,x1,x2,x3,x4,x5,x6) {(h/3)*((1*f(x0)) + (4*f(x1)) + (2*f(x2)) + (4*f(x3)) + (2*f(x4)) + (4*f(x5)) + (1*f(x6)))}
s(h,x0,x1,x2,x3,x4,x5,x6)
h
#toplamda kullanýlan nokta sayýsý 4 ise SÝMPSON 3/8 KURALI 
# 3h/8 .(1.fx0 + 3.fx1 + 3.fx2 + 3.fx3 + 1.fx4) *1 3 3 3 1*
f<-function(x){exp((-1*(x^2)))}

alt=0.2
üst=2.6
noktasayisi= 5

h=hHesapla(alt,üst,noktasayisi)
x0=alt
x1=x0+h
x2=x1+h
x3=x2+h
print(paste("-",x0,"-",x1,"-",x2,"-",x3))

f(x0)
f(x1)
f(x2)
f(x3)

(1*f(x0)) + (3*f(x1)) + (3*f(x2)) + (1*f(x3))
s<-function(h,x0,x1,x2,x3) {(3*h/8)*(1*f(x0)) + (3*f(x1)) + (3*f(x2))  + (1*f(x3))}
s(h,x0,x1,x2,x3)

#FONKSÝYON HESAPLAMA KOLAYLIÐI-----------------
s<- function(x) (x1-x0)*f(x0)
s(x0) 
s

#Euler metodu------------------------------------

euler <- function(f, h = 0.1, x0, y0, xfinal) {
  N = (xfinal - x0) / h
  x = y = numeric(N + 1)
  x[1] = x0; y[1] = y0
  
  a = numeric(6)
  a[0]=as.numeric(x0)
  a[1]=x0+h
  a[2]=a[1]+h
  a[3]=a[2]+h
  a[4]=a[3]+h
  a[5]=a[4]+h
  a[6]=a[5]+h
  
  
  i = 1
  while (i <= N) {
    x[i + 1] = x[i] + h
    y[i + 1] = y[i] + (h * f(x[i], y[i]))
    
    
    print(paste("y[",a[i],"] = y[",a[i-1],"] + ",h,"*f(",a[i-1],",",y[i],")"))
    print(paste(y[i]," + ",h,"*",f(x[i],y[i]),"=",y[i + 1]))
    
    i = i + 1
  }
  return (data.frame(X = x, Y = y))
}

f=function(y,t) {(y*(log(y)))/t}
e1= euler(f, h=0.1, x0=2, y0=exp(1), xfinal=2.3)
e2=euler(f,x0=2,y0=exp(1), xfinal=2.3)
e1; e2[nrow(e2),]

#Düzeltilmiþ Euler(Heun) Metodu--------------------
heun <- function(f, h = 0.5, x0, y0, xfinal) {
  N = (xfinal - x0) / h
  x1 = y1 = numeric(N + 1)
  x1[1] = x0; y1[1] = y0
  x2 = y2 = numeric(N + 1)
  x2[1] = x0; y2[1] = y0
  
  a = numeric(6)
  a[0]=as.numeric(x0)
  a[1]=x0+h
  a[2]=a[1]+h
  a[3]=a[2]+h
  a[4]=a[3]+h
  a[5]=a[4]+h
  
  i = 1
  while (i <= N) {
    x1[i + 1] = x1[i] + h
    x2[i + 1] = x2[i] + h
    y1[i + 1] <- y1[i] + h * f(x1[i], y1[i])
    y2[i + 1] <- y2[i] + h * (f(x2[i], y2[i]) + f(x2[i + 1],y2[i + 1]))/2
  
    
    print(paste("y1[",a[i],"] = y[",a[i-1],"] + ",h,"*f(",a[i-1],",",y1[i],")"))
    print(paste(y1[i]," + ",h,"*",f(x1[i],y1[i]),"=",y1[i + 1]))

    y2[i + 1] = y2[i] + h * ((f(x2[i], y2[i])+f(x2[i+1], y1[i+1]))/2)
    print(paste("y2[",a[i],"] = y[",a[i-1],"] + ",h,"*[(f(",a[i-1],",",y1[i],")+ f(",a[i],",",y1[i+1],"))/2]"))
    print(paste(y2[i] ,"+", h ,"*", (f(x2[i], y2[i]) + f(x2[i + 1],y1[i + 1]))/2,"=",y2[i + 1]))
    i = i + 1
  }
  
  return (data.frame(X = x1, Y1 = y1, Y2=y2))

}


f=function(t,y) {(t*t*y)-(1.2*y)}
e1= heun(f, h=0.5, x0=0, y0=1, xfinal=1)
e2=heun(f,x0=0,y0=1, xfinal=1)
#SONUÇ Y2 NÝN ALTINDAKÝ UNUTMA!
e1; e2[nrow(e2),]


#Düzeltilmiþ Euler hazýr kütüphane kullanarak--------------------------
install.packages("pracma")
library(pracma)
#euler_heun(f, a, b, y0, n = 100, improved = TRUE, ...)
f <- function(t, y) (t-y)/2
s1 <- euler_heun(f, a=0, b=3, y0=1, n=12, improved=TRUE)
s1
s2 <- euler_heun(f, a=0, b=0.5, y0=1, n=2, improved=TRUE)
s2

#Runga Kutta 2. mertebe------------------------
#runge.kutta(f, initial, x)
rungakutta <-function (f, h = 0.2, x0, y0, xfinal) 
{
  N = (xfinal - x0) / h
  x = y = numeric(N + 1)
  x[1] = x0; y[1] = y0
  x = y = numeric(N + 1)
  x[1] = x0; y[1] = y0
  
  a = numeric(6)
  a[0]=as.numeric(x0)
  a[1]=x0+h
  a[2]=a[1]+h
  a[3]=a[2]+h
  a[4]=a[3]+h
  a[5]=a[4]+h
  
  i = 1
  while (i <= N)  {
    x[i + 1] = x[i] + h
    f1 <- f(x[i],y[i])
    f2 <- f(x[i]+h,y[i]+(h*f1))
    y[i+1] <- y[i] + ((h/2)*(f1+f2))
    print(paste("y[",a[i],"] = y[",a[i-1],"]+(h/2).(f1+f2)"))
    print(paste("=",y[i],"+",(h/2),".(",f1,"+",f2,")"))
    print(paste("=",y[i],"+",(h/2),".(",f1+f2,")"))
    print(paste("=", y[i+1] <- y[i] + ((h/2)*(f1+f2))))
    i = i + 1
  }
  #return (y[i+1])
  return (data.frame(X = x, Y = y))
}

f=function(t,y) {t+y^2}
s1=rungakutta(f, h=0.2, x0=0, y0=1, xfinal=0.4)
s2=rungakutta(f,x0=0,y0=1, xfinal=0.4)

s1; s2[nrow(s2),]
#yatay çizelge h artýþýna göre-------------
x0=2
h=0.2
  
x1=x0+h
x2=x1+h
x3=x2+h
x4=x3+h
x5=x4+h
x6=x5+h
x7=x6+h
x1
x2
x3
x4
x5
x6
x7
  

  
  
  
  