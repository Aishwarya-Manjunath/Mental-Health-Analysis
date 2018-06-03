df<-read.csv('survey.csv',stringsAsFactors = FALSE)
#to check if country has NA
any(is.na(df$Country))
#plotting time series data
library(plotly)

#the below function is to convert the month number to the respective name of the month
find_m<-function(x){
  if(x=="1")
    m<-"Jan"
  if(x=="2")
    m<-"Feb"
  if(x=="3")
    m<-"mar"
  if(x=="4")
    m<-"Apr"
  if(x=="5")
    m<-"may"
  if(x=="6")
    m<- "Jun"
  if(x=="7")
    m<- "Jul"
  if(x=="8")
    m<-"Aug"
  if(x=="9")
    m<-"Sep"
  if(x=="10")
    m<-"Oct"
  if(x=="11")
    m<-"Nov"
  if(x=="12")
    m<-"Dec"
  return(m)
}
yes_list<-c() #this contains the response "yes"
no_list<-c() #this contains the response "no"
ct<-1
ct_y<-0
ct_n<-0
x_ax<-c()
x_y<-c()
x_axis<-c()
#below will obtain month number
prev<-as.numeric(unlist(strsplit(df$Timestamp[1],'/'))[1])

#the below for loop keeps a count of all the "yes" and "no" in a given month for the attribute treatment
for(i in 1:nrow(df)){
  curr<-as.numeric(unlist(strsplit(df$Timestamp[i],'/'))[1])
  if(curr!=prev){
    yes_list[ct]<-ct_y
    no_list[ct]<-ct_n
    m<-find_m(prev)
    x_ax[ct]<-m
    x_y[ct]<-as.numeric(unlist(strsplit(unlist(strsplit(df$Timestamp[i-1],'/'))[3],' '))[1])
    x_axis[ct]<-paste(x_ax[ct],x_y[ct],sep='')
    ct<-ct+1
    ct_y<-0
    ct_n<-0
  }
  if(df$treatment[i]=="Yes"){
    ct_y<-ct_y+1
  }
  else{
    ct_n<-ct_n+1
  }
  prev<-curr
}
yes_list[ct]<-ct_y
no_list[ct]<-ct_n
x_ax[ct]<-find_m(prev)
x_y[ct]<-as.numeric(unlist(strsplit(unlist(strsplit(df$Timestamp[i],'/'))[3],' '))[1])
x_axis[ct]<-paste(x_ax[ct],x_y[ct],sep='')

#the below for loop is to keep a track of the proportion of each response for treatment for all months
for(i in 1:length(yes_list)){
  x=yes_list[i]
  yes_list[i]<-yes_list[i]/(yes_list[i]+no_list[i])
  no_list[i]<-no_list[i]/(x+no_list[i])
  }

data_bar <- data.frame(x_axis, yes_list, no_list)
#using the plotly package to plot a grouped bar chart for the responses in each month for attibute treatment
p <- plot_ly(data_bar, x = ~x_axis, y = ~yes_list, type = 'bar', name = 'Yes') %>%
  add_trace(y = ~no_list, name = 'No') %>%
  layout(title = "Peoples reaction to having taken treatment",
         xaxis = list(rangeslider = list(type = "date"),categoryorder = "array",categoryarray = x_axis,title='MM/YY'),
         yaxis = list(title = 'Proportion of each respones'), barmode = 'group')

p


yes_list<-c() #this contains the response "yes"
no_list<-c() #this contains the response "no"
ct<-1
ct_y<-0
ct_n<-0
#below will obtain month number
prev<-as.numeric(unlist(strsplit(df$Timestamp[1],'/'))[1])
#the below for loop keeps a count of all the "yes" and "no" in a given month for the attribute mental_health_interview
for(i in 1:nrow(df)){
  curr<-as.numeric(unlist(strsplit(df$Timestamp[i],'/'))[1])
  if(curr!=prev){
    yes_list[ct]<-ct_y
    no_list[ct]<-ct_n
    ct<-ct+1
    ct_y<-0
    ct_n<-0
  }
  if(df$mental_health_interview[i]=="Yes"){
    ct_y<-ct_y+1
  }
  else{
    ct_n<-ct_n+1
  }
  prev<-curr
}
yes_list[ct]<-ct_y
no_list[ct]<-ct_n
#the below for loop is to keep a track of the proportion of each response for mental_health_interview for all months
for(i in 1:length(yes_list)){
  x=yes_list[i]
  yes_list[i]<-yes_list[i]/(yes_list[i]+no_list[i])
  no_list[i]<-no_list[i]/(x+no_list[i])
}

data_bar <- data.frame(x_axis, yes_list, no_list)
#using the plotly package to plot a grouped bar chart for the responses in each month for attibute mental_health interview
p <- plot_ly(data_bar, x = ~x_axis, y = ~yes_list, type = 'bar', name = 'Yes') %>%
  add_trace(y = ~no_list, name = 'No') %>%
  layout(title = "Peoples reaction for speaking about mental health",
         xaxis = list(rangeslider = list(type = "date"),categoryorder = "array",categoryarray = x_axis,title='MM/YY'),
         yaxis = list(rangeslider = list(type = "number"),title = 'Proportion of each respones'), barmode = 'group')
p
