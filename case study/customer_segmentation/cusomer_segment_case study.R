getwd()
install.packages("dplyr")
install.packages("ggplot2")
library("ggplot2")
library("dplyr")
df=read.csv("Copy of Cx_seg.csv")
df
str(df)
names(df)
head(df)
tail(df)
summary(df)
summary(df$Age)
p=ggplot(df,aes(x=Gender,fill = Gender))+geom_bar()+
labs(x="female/male")
x=table(df$Gender)
x
d=0
count_per=c()
for (j in x){
    d=d+j
}
for (k in x){
  count_per=c(count_per,(k/d)*100)
}
df1=df %>% group_by(Gender)%>%summarise(count=n())
df1=df1%>% mutate(gender_dist=count_per)

ggplot(df1,aes(x="",y=gender_dist,fill=Gender))+geom_bar(width=1,stat="identity")+
  coord_polar("y")+geom_text(aes(label=paste(count_per,"%")),position=position_stack(vjust = 0.5)
  )+theme(axis.ticks  = element_blank())+theme(axis.ticks  = element_blank(),axis.text = element_blank(),
                                               axis.title = element_blank())+ggtitle("gender_dist")

ggplot(df,aes(x=Age))+geom_histogram(bins = 10,color="black",fill="skyblue")+ggtitle("age_distrubution")

ggplot(df,aes(y=Age))+geom_boxplot()
summary(df$Age)

ggplot(df,aes(x=Annual.Income..k..))+geom_histogram(bins = 10,color="black",fill="skyblue")+ggtitle("annual_income")
summary(df$Annual.Income..k..)

ggplot(df,aes(y=Spending.Score..1.100.))+geom_boxplot()
summary(df$Spending.Score..1.100.)
