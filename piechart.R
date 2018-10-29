df<-read.csv('survey.csv',stringsAsFactors = FALSE)
#to check if country has NA
any(is.na(df$Country))
#plotting time series data

age1<-c()
ct<-1
#below for loop finds all the age values which are within 15 and 60
for(i in 1:nrow(df)){
  if(df$Age[i]>=15 || df$Age[i]<=60){
    #age1 contains these age values b/w 15 & 60
    age1[ct]<-df$Age[i]
    ct<-ct+1
  }
}
median_age1<-median(age1)
#the below for loop replaces the age values <15 & >60 with the median value
for(i in 1:nrow(df)){
  if(df$Age[i]<15 || df$Age[i]>60){
    df$Age[i]<-median_age1
  }
}

unique(df$Gender)
# dealing with gender
#the below replaces all the inconsistent values of gender with theconsistent values
g<-c()
for(i in 1:nrow(df)){
  x<-df$Gender[i]
  if(x=="fluid"){
    g[i]<-"Other"
  }
  else if("Trans"==substr(x,1,5) || "trans"==substr(x,1,5) || regexpr("trans",x)[1]!=-1 || regexpr("Trans",x)[1]!=-1){
    if(substr(x,7,7)=="f" || substr(x,7,7)=="w"){
      g[i]<-"Trans-Female"
    }
    else if(substr(x,1,1)=="F" || substr(x,1,1)=="f"){
      g[i]<-"Trans-Female"
    }
  }
  else if("M"==substr(x,1,1) || "m"==substr(x,1,1) || "Guy"==substr(x,1,3) || regexpr("male",x)[1]!=-1){
    g[i]<-"Male"
  }
  else if("F"==substr(x,1,1) || "f"==substr(x,1,1) || "w"==substr(x,1,1) || "W"==substr(x,1,1)){
    g[i]<-"Female"
  }
  else if("Cis"==substr(x,1,3) || "cis"==substr(x,1,3)){
    if(substr(x,5,5)=="f" || substr(x,5,5)=="F"){
      g[i]<-"Female"
    }
    else if(substr(x,5,5)=="m" || substr(x,5,5)=="M"){
      g[i]<-"Male"
    }
  }
  else
    g[i]<-"Other"
}
#replacing gender
df$Gender<-g

#drop column comments
df<-subset(df,select=-comments)

#drop column no_employees
df<-subset(df,select=-no_employees)

#drop rows with NA for self_employed
df<-df[-c(1:18),]

#NA here are replaced by a new category - Not Known
df[is.na(df$work_interfere),"work_interfere"] <- "Not Known"


#pre-analysis graphs

#pie chart for country
p<-as.data.frame(table(df$Country))$Freq
pie(p,labels=as.data.frame(table(df$Country))$Var1)

#pie chart for all the attributes
grid<-matrix(c(1:9),nrow=3,ncol=3,byrow=TRUE)
layout(grid,widths = rep.int(1, ncol(grid)),
       heights = rep.int(1, nrow(grid)))
#pie chart for the attributes 6 to 14
for(i in 6:14){
  slices<-as.data.frame(table(df[,i]))$Freq
  lbls<-as.data.frame(table(df[,i]))$Var1
  pct <- round(slices/sum(slices)*100,2)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep=":") # ad % to labels 
  pie(slices,labels=lbls,col = rainbow(length(lbls)),main=names(df)[i],radius = 1, cex = 0.7)
  Sys.sleep(0.5)
}
grid<-matrix(c(1:9),nrow=3,ncol=3,byrow=TRUE)
layout(grid,widths = rep.int(1, ncol(grid)),
       heights = rep.int(1, nrow(grid)))
#pie chart for the attributes 15 to 23
for(i in 15:23){
  slices<-as.data.frame(table(df[,i]))$Freq
  lbls<-as.data.frame(table(df[,i]))$Var1
  pct <- round(slices/sum(slices)*100,2)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep=":") # ad % to labels 
  pie(slices,labels=lbls,col = rainbow(length(lbls)),main=names(df)[i],radius = 1, cex = 0.7)
  Sys.sleep(0.5)
}
grid<-matrix(c(1:4),nrow=2,ncol=2,byrow=TRUE)
layout(grid,widths = rep.int(1, ncol(grid)),
       heights = rep.int(1, nrow(grid)))
#pie chart for the attributes 24 & 25
for(i in 24:25){
  slices<-as.data.frame(table(df[,i]))$Freq
  lbls<-as.data.frame(table(df[,i]))$Var1
  pct <- round(slices/sum(slices)*100,2)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep=":") # ad % to labels 
  pie(slices,labels=lbls,col = rainbow(length(lbls)),main=names(df)[i],radius = 1, cex = 0.7)
  Sys.sleep(0.5)
}

#pie chart for gender
slices<-as.data.frame(table(df$Gender))$Freq
slices<-slices/sum(slices)*100
lbls<-c("Female","Male","Other","Trans-F")
barplot(slices, main="Gender",names.arg=lbls)

#pie chart for age
slices<-c(0,0,0)
lbls<-c("Age(15-30)","Age(30-45)","Age(45-60)")
for(i in 1:nrow(df)){
  if(df$Age[i]<30 && df$Age[i]>=15){
    slices[1]=slices[1]+1
  }
  else if(df$Age[i]<45 && df$Age[i]>=30){
    slices[2]=slices[2]+1
  }
  else if(df$Age[i]<=60 && df$Age[i]>=45){
    slices[3]=slices[3]+1
  }
  else
    print("NI")
}

pct <- round(slices/sum(slices)*100,2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels=lbls,col = rainbow(length(lbls)),main="Age in a range",radius = 0.90, cex = 0.5)

par(mfrow=c(1,1))