options(repos='http://mirrors.tuna.tsinghua.edu.cn/CRAN/')  # 改成http， 而不是https
install.packages('arsenal')
library(arsenal)

install.packages('qwraps2')
library(qwraps2)

head(data)
table_one <- tableby(group ~ STAI, data)
summary(table_one)

tapply(data$STAI,data$group, summary)

library(psych)
describeBy(data$STAI,data$group, mat = TRUE)    #good

library(ggplot2)
head(data)
p <- ggplot(data, aes(x=group,y=STAI))
p + geom_boxplot(aes(colour=factor(group)),width=0.5)+
  theme(panel.background = element_blank(),axis.ticks=element_blank(),legend.position="none",axis.line = element_line(colour="black",arrow=arrow(length = unit(0.20, "npc"))))

p <- ggplot(data, aes(x=group,y=STAI))
p + stat_boxplot(geom = "errorbar",width=0.15,aes(color=factor(group)))+
  geom_boxplot(aes(colour=factor(group)),width=0.5)+
  theme(panel.background = element_blank(),axis.ticks=element_blank(),legend.position="none",axis.line = element_line(colour="black",size = 1),
        axis.ticks.y = element_line(size = 1), 
        axis.ticks.x = element_line(size = 1), 
        axis.title.x = element_text(size = 16, face = "bold",margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, face = "bold",margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x= element_text(size = 16, face = "bold"),
        axis.text.y= element_text(size = 16,face = "bold"),
        axis.ticks.length.y = unit(0.15, 'cm'),
        axis.ticks.length.x = unit(0.15,'cm'))+
  scale_color_manual(values=c("red", "gray45"))+
  labs(x="group", y = "Trait anxiety")+
  geom_point(data,mapping = aes(group,STAI,color = factor(group)),position = position_jitterdodge(),size = 3)+
  scale_y_continuous(breaks = c(20,30,40,50,60),limits = c(20,60))
  #scale_x_continuous(breaks = c(0,1),limits = c(-0.5,1.5))


ggplot(data, aes(x=group, y=STAI,color=factor(group))) + 
  geom_boxplot()+
  scale_color_manual(values=c("red", "gray45"))
