library(dplyr)
library(ggplot2)
library(GGally)
library(here)

std <- function(x) sd(x[!is.na(x)])/sqrt(length(x[!is.na(x)]))

data<- read.table("WBS2_cleaned.csv",header=T,sep=",")

#Two experiment factors
data$NConc<-as.factor(data$NConc)
data$Seasol<-as.factor(data$Seasol)

#Time variable
data$DAS<-as.numeric(data$DAS)

#Response variable must be positive
response_var<-c("Height_manual","height_2d_mm","height_mm","proj_area_2d_mm2","area_mm2","volume_mm3","tGmRmB","n_tGmRmB","GmR","nGmR","G","nG")
data[response_var<=0]<-NA

###############
##visualization
#plot individual curve, facet by Nitrogen, color by Seasol
ggplot(data=data, aes(x=DAS, y=height_2d_mm, group=as.factor(Pot), na.rm = TRUE)) +
  geom_line(aes(color=Seasol))+
  geom_point(aes(color=Seasol), show.legend=FALSE)+
  scale_x_continuous(breaks =unique(data$DAS),labels=as.character(unique(data$DAS)))+
  facet_wrap(~NConc)

#plot individual curve, facet by Seasol, combine Nitrogen levels
ggplot(data=data, aes(x=DAS, y=height_2d_mm, group=as.factor(Pot), na.rm = TRUE)) +
  geom_line()+
  geom_point(show.legend=FALSE)+
  scale_x_continuous(breaks =unique(data$DAS),labels=as.character(unique(data$DAS)))+
  facet_wrap(~Seasol)

#plot individual curve, facet by Nitrogen, combine Seasol levels
ggplot(data=data, aes(x=DAS, y=height_2d_mm, group=as.factor(Pot), na.rm = TRUE)) +
  geom_line()+
  geom_point(show.legend=FALSE)+
  scale_x_continuous(breaks =unique(data$DAS),labels=as.character(unique(data$DAS)))+
  facet_wrap(~NConc)

#aggregate data, plot mean curve,facet by Nitrogen
data_agg<-data %>%
  group_by(DAS,Seasol,NConc) %>%
  summarize(mean.height_2d_mm = mean(height_2d_mm, na.rm = TRUE),
            sd.height_2d_mm = std(height_2d_mm))

ggplot(data=data_agg, aes(x=DAS, y=mean.height_2d_mm, group=Seasol, na.rm = TRUE)) +
  geom_line(aes(color=Seasol))+
  geom_point(aes(color=Seasol),show.legend=FALSE)+
  geom_errorbar(aes(ymin=mean.height_2d_mm-sd.height_2d_mm, ymax=mean.height_2d_mm+sd.height_2d_mm,color=Seasol), width=.1)+
  scale_x_continuous(breaks =unique(data$DAS),labels=as.character(unique(data$DAS)))+
  facet_wrap(~NConc)

#aggregate data, plot mean curve,facet by Seasol
ggplot(data=data_agg, aes(x=DAS, y=mean.height_2d_mm, group=NConc, na.rm = TRUE)) +
  geom_line(aes(color=NConc))+
  geom_point(aes(color=NConc),show.legend=FALSE)+
  geom_errorbar(aes(ymin=mean.height_2d_mm-sd.height_2d_mm, ymax=mean.height_2d_mm+sd.height_2d_mm,color=NConc), width=.1)+
  scale_x_continuous(breaks =unique(data$DAS),labels=as.character(unique(data$DAS)))+
  facet_wrap(~Seasol)

##Correlation
ggpairs(data[c("tGmRmB","n_tGmRmB","GmR","nGmR","G","nG")])
ggpairs(data[c("Height_manual","height_2d_mm","height_mm","proj_area_2d_mm2","area_mm2","volume_mm3")])