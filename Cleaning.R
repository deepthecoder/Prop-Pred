#-----------------Console----------------------------------
options(warn=-1)
#prop<-function()
#{
k<-read.csv("rawdata.csv")
#------------------------Cleans the Price Column--------------------
vectorised<- function(vector) 
{
  vector <- as.character(vector)
  vector <- gsub("^.? ","", vector)
  result <- as.numeric(vector)
  lac_pos <- grep(" Lac", vector)
  result[lac_pos] <- as.numeric(gsub(" Lac","", vector[lac_pos])) * 100000
  Cr_pos<- grep(" Cr", vector)
  result[Cr_pos] <- as.numeric(gsub(" Cr","", vector[Cr_pos])) * 10000000
  return(result)
}
vector<-k$Price

price<-vectorised(k$Price)
area<-gsub(" Sq-.*","",k$Area)
g<-data.frame(price,area,k$Project,k$Locality,k$COVERED_PARKING,
              k$FLOOR_NUMBER,k$ABOUT_FURNISHING)
write.csv(g,file="cleandata.csv",row.names = FALSE)
f<-read.csv("cleandata.csv")
matches <- f[grepl("Furnished|Unfurnished|Semi-Furnished",f$k.ABOUT_FURNISHING), ]
new<-data.frame(matches)
write.csv(new,"cleandata.csv",row.names = FALSE)
f<-read.csv("cleandata.csv")
g<-read.csv("cleandata.csv")
g<-g[complete.cases(g$k.COVERED_PARKING),]
g<-g[complete.cases(g$k.FLOOR_NUMBER),]
write.csv(g,file="cleandata.csv",row.names = FALSE)
g<-read.csv("cleandata.csv")

g$k.FLOOR_NUMBER<-gsub("Ground", 0 ,g[,6]) 
g$k.FLOOR_NUMBER<-gsub("Upper Basement", -1 ,g[,6]) 
g$k.FLOOR_NUMBER<-gsub("Lower Basement", -2 ,g[,6]) 

g$k.FLOOR_NUMBER<-as.numeric(g$k.FLOOR_NUMBER)

g$k.COVERED_PARKING[g$k.COVERED_PARKING>2]<-"More than 2"
g$k.COVERED_PARKING[g$k.COVERED_PARKING==0]<-"0"
g$k.COVERED_PARKING[g$k.COVERED_PARKING==1]<-"1"
g$k.COVERED_PARKING[g$k.COVERED_PARKING==2]<-"2"

price<-g$price
area<-g$area
k.Project<-g$k.Project
k.Locality<-g$k.Locality
k.COVERED_PARKING<-g$k.COVERED_PARKING
k.FLOOR_NUMBER<-g$k.FLOOR_NUMBER
k.ABOUT_FURNISHING<-g$k.ABOUT_FURNISHING

d<-data.frame(price,area,k.Project,k.Locality,k.COVERED_PARKING,k.FLOOR_NUMBER,k.ABOUT_FURNISHING)
write.csv(d,file="checkit.csv",row.names = FALSE)
g<-read.csv("checkit.csv")

g<-g[complete.cases(g$k.COVERED_PARKING),]
g<-g[complete.cases(g$k.FLOOR_NUMBER),]
g<-g[complete.cases(g$k.ABOUT_FURNISHING),]

h=lm(g$price~area+k.Project+k.Locality+k.FLOOR_NUMBER+k.COVERED_PARKING+k.ABOUT_FURNISHING,data=g)
txt<-coefficients(h)

capture.output(txt,file = "coeffInfo.txt")

predicted<-predict.lm(h) 
actual<-g$price
n<-data.frame(actual,predicted)
n$Diff<-(n$actual-n$predicted)
n$error<-(n$Diff/n$actual)
n$error<-(n$error*100)
a<-(n[n$error<=10 & n$error>=-10,])
n$count<-nrow(a)/nrow(g)
n<-data.frame(n$actual,n$predicted,n$error,n$count)
n<-lapply(n,abs)
write.csv(n,file="error.csv",row.names = FALSE)
w<-read.csv("error.csv")
act<-w$n.actual
pre<-w$n.predicted
err_per<-w$n.count
mdape<-median(w$n.error)
x<-data.frame(w$n.actual,w$n.predicted,w$n.error,err_per,mdape)
write.csv(x,file="ErrorMDape.csv",row.names = FALSE)
prop<-function()
{
   {
      ar<-readline(prompt = "Enter the approximate area in (Sqft): ")
      area<-as.integer(ar)
      loc<-readline(prompt = "Enter the location you want to search: ")
      proj<-readline(prompt = "Enter the project name in that particular location: ")
      k.Locality<-as.factor(loc)
      k.Project<-as.factor(proj)
      over<-readline(prompt="Enter the number of covered parking(1/2/More than 2) :")
      floor<-readline(prompt = "Enter the floor number of choice:")
      furnish<-readline(prompt="Enter the choice of the state of furnishing(Furnished/Unfurnished/Semi-Furnished):")
      k.COVERED_PARKING<-as.factor(over)
      k.FLOOR_NUMBER<-as.integer(floor)
      k.ABOUT_FURNISHING<-as.factor(furnish)
      locknn<-g[grep(loc,g$k.Locality,ignore.case = T),]
      d<-data.frame(locknn)
      write.csv(d,file="Knn.csv",row.names = FALSE)
      l<-read.csv("Knn.csv")
      projknn<-l[grep(proj,l$k.Project,ignore.case = T),]
      n2<-data.frame(projknn)
      write.csv(n2,file="Knn.csv",row.names = FALSE)
      s<-read.csv("Knn.csv")
      
      b<-s[s$area-area<=50 & s$area-area>=-50,]
      if(nrow(b)>0)
      { 
          print("Using K-Nearest Neighbour Approach")
          print(mean(b[["price"]]))
          print("Using Global Model For Multiple Regression")
          newframe=data.frame(area,k.Project,k.Locality,k.FLOOR_NUMBER,k.COVERED_PARKING,k.ABOUT_FURNISHING)
          testset<-write.csv(newframe,file = "predict.csv",row.names = F)
          good<-predict.lm(h,newframe,interval = "confidence")
          print(good)
      }
      else
        {
          print("No information gained from KNN...")
          print("Using Global Model For Multiple Regression")
          newframe=data.frame(area,k.Project,k.Locality,k.FLOOR_NUMBER,k.COVERED_PARKING,k.ABOUT_FURNISHING)
          testset<-write.csv(newframe,file = "predict.csv",row.names = F)
          good<-predict.lm(h,newframe,interval = "confidence")
          print(good)
        }
   }
j<-readline(prompt="Enter 1 to continue, 0 to exit: ")
n<-as.integer(j)
if(n!=1)break
else
  prop()
}
prop()