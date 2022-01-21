#학술제 그래프
culture<-read.csv('Tculture.csv',header=T)
attach(culture)

#남녀 응답수 파이차트
library(dplyr)
tt<-culture %>% group_by(X1) %>% summarise(N=n())
tt

library(ggplot2)
library(scales)
pie<-ggplot(tt,aes(x="",y=N,fill=X1))+ 
  geom_bar(width=1,stat="identity") +
  coord_polar(theta="y",start=0) + scale_fill_brewer(palette="Blues")+
  theme_minimal()
pie
#모든 표시 없애기
blank_theme <- theme_minimal()+         
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold") )
pie + scale_fill_brewer("sex" , palette = "RdBu") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = N/2 + c(cumsum(N)[-length(N)],1), 
                label = percent(N/sum(N))), size=10)


###여기는 일반 파이차트###
#학년별 응답수
tt<-table(X2)
tt
pie(tt)
pct<-round(tt/sum(tt) * 100)
slabel<-paste(pct)
slabel<-paste(slabel,"%",sep="")
colors()
cc<-c('snow1','plum','tomato2','gold')
pie(tt,labels=slabel,col=cc,main='설문지 학년 별 응답수')
legend("topright",c('1학년','2학년','3학년','4학년'),cex=0.8,fill=cc)

#문화생활 종류 빈도 차트
culturef<-read.csv("cultureF.csv",header=T)
attach(culturef)
tt<-table(X20)
tt
pie(tt)
pct<-round(tt/sum(tt) * 100)
slabel<-paste(pct)
slabel<-paste(slabel,"%",sep="")
cc<-c('plum','lightgreen','lightblue2','gold','orange')
pie(tt,labels=slabel,col=cc,main='문화생활 종류 빈도 차트')
legend("topright",c('영화','연극 및 뮤지컬','스포츠 경기관람','전시회 및 박물관','기타'),cex=0.8,fill=cc)


##ggplots 을 이용하여 그래프 그리기
#학년과 성별
culture$X2<-as.factor(culture$X2)
culture$X1<-as.factor(culture$X1)
ss<-c('lightblue','pink')
ggplot(culture, aes(x=X1,fill=X1))+ ggtitle("학년과 성별") + facet_grid(~ X2) + geom_bar(position='dodge')  +
  scale_fill_brewer(palette = "Oranges") 
?scale_fill_brewer #색깔 찾아볼 수 있음

#알바 여부에 따른 문화생활
culturef$X7<-as.factor(culturef$X7)
culturef$X19<-as.factor(culturef$X19)
ggplot(culturef, aes(x=X19,fill=X19)) + ggtitle("아르바이트 근무에 따른 문화생활 소비 분포") +
  xlab("알바") + ylab("문화생활 소비") +   facet_grid(~ X7) + 
  geom_bar(position="dodge") + scale_fill_discrete(name='문화생활 소비', breaks=c("1","2","3","4","5"),
                                                   labels=c("5만원미만", "5-10만원", "10-15만원","15-20만원","20만원이상"))


#학년에 따른 소비 차이
culturef
head(culturef)

culturef$X2<-as.factor(culturef$X2)
culturef$X19<-as.factor(culturef$X19)
library(ggplot2)
#1번
ggplot(culturef, aes(x=X2,fill=X19)) + ggtitle("학년에 따른 문화생활 소비 분포") +
  xlab("학년") + ylab("문화생활 소비")  +
  geom_bar(position="stack") + scale_fill_discrete(name='문화생활 소비'
                                                   , breaks=c("1","2","3","4","5"),
                                                   labels=c("5만원미만", "5-10만원", "10-15만원","15-20만원","20만원이상"))  
#2번
ggplot(culturef, aes(x=X19,fill=X19)) + ggtitle("학년에 따른 문화생활 소비 분포") +
  xlab("학년") + ylab("문화생활 소비") +   facet_grid(~ X2) + 
  geom_bar(position="dodge") + scale_fill_discrete(name='문화생활 소비'
                                                   , breaks=c("1","2","3","4","5"),
                                                   labels=c("5만원미만", "5-10만원", "10-15만원","15-20만원","20만원이상"))




#corrplot
?preplot.locfit()
install.packages('corrplot')
library(corrplot)

culture_corr<-culture[,c(4:18)]
cor<-cor(culture_corr)
col2<-colorRampPalette(c("#67001F","#B2182B","#D6604D","#F4A582","#FDDBC7","#FFFFFF","#D1E5F0","#92C5DE","#4393C3","#2166AC","#053061"))
corrplot(cor,addrect = 2,col = col2(60))
####ggplot 파이차트#####
##남여 파이차트
library(dplyr)
tt<-culture %>% group_by(X1) %>% summarise(N=n()) #빈도수 구하기 함수
tt

library(ggplot2)
library(scales)
pie<-ggplot(tt,aes(x="",y=N,fill=X1))+ 
  geom_bar(width=1,stat="identity") +
  coord_polar(theta="y",start=0) + scale_fill_brewer(palette="Blues")+
  theme_minimal()
#모든 표시 없애기
blank_theme <- theme_minimal()+         
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold") )
pie + scale_fill_brewer("sex" , palette = "RdBu") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = N/2 + c(cumsum(N)[-length(N)],1), 
                label = percent(N/sum(N))), size=10)


##학년별 파이차트

tt<-culture %>% group_by(X2) %>% summarise(N=n())
tt

pie<-ggplot(tt,aes(x="",y=N,fill=X2))+ 
  geom_bar(width=1,stat="identity") +
  coord_polar(theta="y",start=0) + scale_fill_brewer(palette="Blues")+
  theme_minimal()
#모든 표시 없애기
blank_theme <- theme_minimal()+         
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold") )
pie + scale_fill_brewer("blues") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = N/4 + c(0,cumsum(N)[-length(N)]), 
                label = percent(N/sum(N))), size=4)

#plotly 방법 (https://plot.ly/r/pie-charts/)
##남녀 파이차트
install.packages("plotly")
library(plotly)
library(dplyr)
tt<-culture %>% group_by(X1) %>% summarise(N=n())
tt$sex<-as.factor(c("남","여"))
p <- plot_ly(tt, labels = ~sex, values = ~N, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste(sex),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = '남녀 분포',cex=1,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p

##학년 파이차트
tt<-culture %>% group_by(X2) %>% summarise(N=n())
tt
p <- plot_ly(tt, labels = ~X2, values = ~N, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste( X2, ' 학년'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = '학년 별 분포',cex=1,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p
#ROC 곡선 그래프
data<-read.csv("Tculture.csv",header = T)
install.packages("pROC")
library(pROC)
install.packages("Epi")
library(Epi)
ROC(form=data$Y~data$X9+data$X10,data=data,plot="ROC")
#문화생활 하는 사람 
data1<-read.csv("cultureF.csv")
str(data1)
data1$X21<-as.factor(data1$X21)
ROC(form=data1$Y1~data1$X4+data1$X6+data1$X7+data1$X20+data1$X21,data=data1,plot="ROC")
ROC(form=data1$Y2~data1$X1+data1$X5+data1$X10+data1$X11,data=data1,plot="ROC")
ROC(form=data1$Y3~data1$X2+data1$X5+data1$X13+data1$X20+data1$X21+data1$X23,data=data1,plot="ROC")
ROC(form=data1$Y3~data1$X5+data1$X13+data1$X20+data1$X21,data=data1,plot="ROC")
