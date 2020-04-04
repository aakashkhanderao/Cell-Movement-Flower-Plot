
data <- read.csv("Data_set.csv")
data<-data.frame(data)
coln<-length(data[1,])
row<-length(data[,1])

num=1
step_distance<-data.frame()
distance<-c()
displacement<-c()
name_list<-c()

#distance
for(i in seq(1,row-1, by=1)){
  num=1
  rec1<-data[i,2:coln]
  rec2<-data[i+1,2:coln]
  for(j in seq(1,coln-1,by=2))
  {
    x1<-rec1[j]
    x2<-rec2[j]
    y1<-rec1[j+1]
    y2<-rec2[j+1]
    step_distance[i,num]=sqrt(((x2-x1)^2)+((y2-y1)^2))
    if(i==row-1)
      name_list[[num]]<- paste("Cell ",num)
    num=num+1
  }
}

for(x in seq(1,length(step_distance[1,])))
{
  distance[[x]]=sum(step_distance[,x])
}

#displacement
rec1<-data[1,2:coln]
rec2<-data[row,2:coln]
num=1
for(j in seq(1,coln-1,by=2))
{
  
  x1<-rec1[j]
  x2<-rec2[j]
  y1<-rec1[j+1]
  y2<-rec2[j+1]
  displacement[[num]]=sqrt(((x2-x1)^2)+((y2-y1)^2))
  num=num+1
  
}

#Avg Speed
speed=distance/100

#persistence
persis<-c()
persis<-distance/unname(unlist(displacement))

#plot double y plot
plot(x = speed,col = "blue",type="l",xlab = "Cell Numbers", ylab = "Speed",lwd = 2)
par(new = T)
plot(x = persis,col = "red", type = "l", xaxt = "n", yaxt = "n",
     xlab = " ", ylab = "", lwd = 2,lty = 2)
axis(side = 4)
mtext("Persistence", side = 4, line = 0)
legend("topleft",
       c("Speed","Persistence"),
       col = c("blue", "red"), lty = c(1, 2), cex=0.55
       )


