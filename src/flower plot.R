install.packages("randomcoloR")
library("randomcoloR")
data <- read.csv("Data_set.csv")
data<-data.frame(data)
cells<-length(data)
num=1
col_list<-c();
name_list<-c();

for(i in seq(2,cells-1,by=2)){
  x<-c(0,unlist(data[i]))
  print(typeof(x))
  y<-c(0,unlist(data[i+1]))
  print(typeof(y))
  print(data)
  color <- randomColor(count=1, hue="random", luminosity = "bright")
  col_list[[num]]<- color
  name_list[[num]]<- paste("Cell ",num)
  if(i==2)
    plot(x,y, type="l", col=color, lwd=2)
  else
    lines(x,y, type="l", col=color, lwd=2)
  num<-num+1
}
#legend(2,20,legend= name_list, col=col_list ,border = "black", lty=1) 

legend("topleft",unlist(name_list),col = unlist(col_list),lty = 1,lwd=2, cex=0.55)

