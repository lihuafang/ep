#善识计划

pre <- read.csv("shanshi20200901.csv")
post <- read.csv("shanshi20200902.csv")

pre.sub <- pre[c("outcomes_1", "outcomes_2","outcomes_3","outcomes_4","outcomes_5",
             "outcomes_6","outcomes_7","outcomes_8","outcomes_9","outcomes_10",
             "outcomes_11","outcomes_12","outcomes_13","outcomes_14","outcomes_15")]


post.sub <- post[c("outcomes_1", "outcomes_2","outcomes_3","outcomes_4","outcomes_5",
                 "outcomes_6","outcomes_7","outcomes_8","outcomes_9","outcomes_10",
                 "outcomes_11","outcomes_12","outcomes_13","outcomes_14","outcomes_15")]




pre.sub$pt <- 0
post.sub$pt <- 1
ep <- rbind(pre.sub, post.sub)

#main effects '

ep$aware <- (ep$outcomes_1+ep$outcomes_2)/2 
ep$edu <- (ep$outcomes_3+ep$outcomes_4+ep$outcomes_5+ep$outcomes_6)/4
ep$social <- (ep$outcomes_7+ep$outcomes_8+ep$outcomes_9+
                ep$outcomes_10+ep$outcomes_11)/5
ep$phil <- (ep$outcomes_12+ep$outcomes_13+ep$outcomes_14+ep$outcomes_15)/4



lm.aware <- lm(aware~pt, data = ep)
lm.edu <- lm(edu~pt, data = ep)
lm.social <- lm(social~pt, data = ep)
lm.phil <- lm(phil~pt, data = ep)

library(stargazer)
stargazer(lm.aware, lm.edu, lm.social, lm.phil,
          type="html", digits = 2,
          dep.var.labels =c("需求意识","学业兴趣","社会责任","公益慈善意向"),
          covariate.labels=c("善识课程"),
          single.row=F, out="maineffect20200924.htm")

#main figure / all effects 
#label mean values
library(ggplot2)
library(extrafont)

ep$pt=as.factor(ep$pt)

pre1 <- subset(ep, pt==0)
post1 <- subset(ep, pt==1)

summary(pre1$aware) #54.80
summary(post1$aware) #56.71

p.aware <- ggplot(ep, aes(x=pt, y=aware)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=54.80,vjust=-1, label="54.80") +
  annotate("text",x=2,y=56.71,vjust=-1, label="56.71") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(1)", y="需求意识") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p.aware

summary(pre1$edu) #60.52
summary(post1$edu) #65.41

p.edu <- ggplot(ep, aes(x=pt, y=edu)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=60.52,vjust=-1, label="60.52") +
  annotate("text",x=2,y=65.41,vjust=-1, label="65.41") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(2)", y="学业兴趣") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p.edu


summary(pre1$social) #72.45
summary(post1$social) #74.59

p.social <- ggplot(ep, aes(x=pt, y=social)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=72.45,vjust=-1, label="72.45") +
  annotate("text",x=2,y=74.59,vjust=-1, label="74.59") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(3)", y="社会责任") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p.social

summary(pre1$phil) #69.69
summary(post1$phil) #71.68

p.phil <- ggplot(ep, aes(x=pt, y=phil)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=69.69,vjust=-1, label="69.69") +
  annotate("text",x=2,y=71.68,vjust=-1, label="71.68") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(4)", y="公益慈善意向") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p.phil

#multiplot:  put figures in one page 
## function multiplot 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

library(Hmisc)

multiplot(p.aware, p.edu, p.social, p.phil, cols=4)


library(stargazer)
stargazer(pre1[c("aware", "edu", "social", "phil")], type = "html",
          title="Descriptive statistics/selected variables", digits=2, 
          out="pre1stats.html")

stargazer(post1[c("aware", "edu", "social", "phil")], type = "html",
          title="Descriptive statistics/selected variables", digits=2, 
          out="post1stats.html")


#detailed effects - 15 outcomes

lm1 <- lm(outcomes_1~pt, data=ep)
lm2 <- lm(outcomes_2~pt, data=ep)
lm3 <- lm(outcomes_3~pt, data=ep)
lm4 <- lm(outcomes_4~pt, data=ep)
lm5 <- lm(outcomes_5~pt, data=ep)
lm6 <- lm(outcomes_6~pt, data=ep)
lm7 <- lm(outcomes_7~pt, data=ep)
lm8 <- lm(outcomes_8~pt, data=ep)
lm9 <- lm(outcomes_9~pt, data=ep)
lm10 <- lm(outcomes_10~pt, data=ep)
lm11 <- lm(outcomes_11~pt, data=ep)
lm12 <- lm(outcomes_12~pt, data=ep)
lm13 <- lm(outcomes_13~pt, data=ep)
lm14 <- lm(outcomes_14~pt, data=ep)
lm15 <- lm(outcomes_15~pt, data=ep)

library(stargazer)
stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, 
          lm10, lm11, lm12, lm13, lm14, lm15,
          type="html", digits = 2,
          covariate.labels=c("课后"),
          single.row=F, out="shanshi20200923.htm")


#main figure / all effects 
#label mean values
library(ggplot2)
library(extrafont)

ep$pt=as.factor(ep$pt)

summary(pre$outcomes_1) #61.54
summary(post$outcomes_1) #63.97 

p1 <- ggplot(ep, aes(x=pt, y=outcomes_1)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=61.54,vjust=-1, label="61.54") +
  annotate("text",x=2,y=63.97,vjust=-1, label="63.97") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(1)", y="了解身边的人及需求") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p1


summary(pre$outcomes_2) #46.79
summary(post$outcomes_2) #49.33
p2 <- ggplot(ep, aes(x=pt, y=outcomes_2)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=46.79,vjust=-1, label="46.79") +
  annotate("text",x=2,y=49.33,vjust=-1, label="49.33") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(2)", y="了解身边的公益慈善组织") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p2

summary(pre$outcomes_3) #73.67
summary(post$outcomes_3) #77.23
p3 <- ggplot(ep, aes(x=pt, y=outcomes_3)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=73.67,vjust=-1, label="73.67") +
  annotate("text",x=2,y=77.23,vjust=-1, label="77.23") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(3)", y="对这门课感兴趣") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p3


summary(pre$outcomes_4) #70.14
summary(post$outcomes_4) #72.79
p4 <- ggplot(ep, aes(x=pt, y=outcomes_4)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=70.14,vjust=-1, label="70.14") +
  annotate("text",x=2,y=72.79,vjust=-1, label="72.79") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(4)", y="对公益慈善感兴趣") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p4

summary(pre$outcomes_5) #57.16
summary(post$outcomes_5) #64.62
p5 <- ggplot(ep, aes(x=pt, y=outcomes_5)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=57.16,vjust=-1, label="57.16") +
  annotate("text",x=2,y=64.62,vjust=-1, label="64.62") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(5)", y="多选公益慈善课程") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p5


summary(pre$outcomes_6) #38.53
summary(post$outcomes_6) #45.62
p6 <- ggplot(ep, aes(x=pt, y=outcomes_6)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=38.53,vjust=-1, label="38.53") +
  annotate("text",x=2,y=45.62,vjust=-1, label="45.62") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(6)", y="读公益慈善学位") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p6

summary(pre$outcomes_7) #70.70
summary(post$outcomes_7) #77.44
p7 <- ggplot(ep, aes(x=pt, y=outcomes_7)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=70.70,vjust=-1, label="70.70") +
  annotate("text",x=2,y=77.44,vjust=-1, label="77.44*") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(7)", y="有兴趣加入组织去帮人") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后*"))
p7

summary(pre$outcomes_8) #73.64
summary(post$outcomes_8) #75.80
p8 <- ggplot(ep, aes(x=pt, y=outcomes_8)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=73.64,vjust=-1, label="73.64") +
  annotate("text",x=2,y=75.80,vjust=-1, label="75.80") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(8)", y="和其他人一起努力解决问题") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p8

summary(pre$outcomes_9) #74.37
summary(post$outcomes_9) #74.96
p9 <- ggplot(ep, aes(x=pt, y=outcomes_9)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=74.37,vjust=-1, label="74.37") +
  annotate("text",x=2,y=74.96,vjust=-1, label="74.96") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(9)", y="有责任帮助有需要的人") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p9

summary(pre$outcomes_10) #64.23
summary(post$outcomes_10) #67.78
p10 <- ggplot(ep, aes(x=pt, y=outcomes_10)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=64.23,vjust=-1, label="64.23") +
  annotate("text",x=2,y=67.78,vjust=-1, label="67.78") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(10)", y="要对自己所在社区负责") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p10

summary(pre$outcomes_11) #79.26
summary(post$outcomes_11) #76.20
p11 <- ggplot(ep, aes(x=pt, y=outcomes_11)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=79.26,vjust=-1, label="79.26") +
  annotate("text",x=2,y=76.20,vjust=-1, label="76.20") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(11)", y="为让世界更美好出一份力") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p11


summary(pre$outcomes_12) #84.49
summary(post$outcomes_12) #86.96
p12 <- ggplot(ep, aes(x=pt, y=outcomes_12)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=84.49,vjust=-1, label="84.49") +
  annotate("text",x=2,y=86.96,vjust=-1, label="86.96") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(12)", y="将来会参与志愿活动") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p12



summary(pre$outcomes_13) #68.40
summary(post$outcomes_13) #71.26
p13 <- ggplot(ep, aes(x=pt, y=outcomes_13)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=68.40,vjust=-1, label="68.40") +
  annotate("text",x=2,y=71.26,vjust=-1, label="71.26") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(13)", y="将来会帮助公益组织募捐") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p13

summary(pre$outcomes_14) #82.20
summary(post$outcomes_14) #78.62
p14 <- ggplot(ep, aes(x=pt, y=outcomes_14)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=82.20,vjust=-1, label="82.20") +
  annotate("text",x=2,y=78.62,vjust=-1, label="78.62") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(14)", y="将来会给公益组织捐款") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p14

summary(pre$outcomes_15) #38.79
summary(post$outcomes_15) #46.69
p15 <- ggplot(ep, aes(x=pt, y=outcomes_15)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=38.79,vjust=-1, label="38.79") +
  annotate("text",x=2,y=46.69,vjust=-1, label="46.69") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="(15)", y="毕业打算找与公益慈善相关的工作") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p15





#descriptive statistics - mean over periods 
multiplot(p1, p6, p11, p2, p7, p12, p3, p8, p13, 
          p4, p9, p14, p5, p10, p15, cols=5)
