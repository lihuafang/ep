#善识计划
##北京师范大学

pre <- read.csv("shanshi20200901.csv")
post <- read.csv("shanshi20200902.csv")

pre <- subset(pre, school=="北京师范大学")
post <- subset(post, school=="北京师范大学")

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
          single.row=F, out="bnu20200924.htm")

#main figure / all effects 
#label mean values
library(ggplot2)
library(extrafont)

ep$pt=as.factor(ep$pt)

pre1 <- subset(ep, pt==0)
post1 <- subset(ep, pt==1)

summary(pre1$aware) #61.27
summary(post1$aware) #49.12

p.aware <- ggplot(ep, aes(x=pt, y=aware)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=61.27,vjust=-1, label="61.27") +
  annotate("text",x=2,y=49.12,vjust=-1, label="49.12") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="北师大", y="需求意识") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p.aware

summary(pre1$edu) #55.81
summary(post1$edu) #58.74

p.edu <- ggplot(ep, aes(x=pt, y=edu)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=55.81,vjust=-1, label="55.81") +
  annotate("text",x=2,y=58.74,vjust=-1, label="58.74") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="北师大", y="学业兴趣") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p.edu


summary(pre1$social) #76.60
summary(post1$social) #78.02

p.social <- ggplot(ep, aes(x=pt, y=social)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=76.60,vjust=-1, label="76.60") +
  annotate("text",x=2,y=78.02,vjust=-1, label="78.02") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="北师大", y="社会责任") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
p.social

summary(pre1$phil) #73.86
summary(post1$phil) #67.14

p.phil <- ggplot(ep, aes(x=pt, y=phil)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=73.86,vjust=-1, label="73.86") +
  annotate("text",x=2,y=67.14,vjust=-1, label="67.14") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="北师大", y="公益慈善意向") +
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




#南开大学
pre <- read.csv("shanshi20200901.csv")
post <- read.csv("shanshi20200902.csv")

pre <- subset(pre, school=="南开大学")
post <- subset(post, school=="南开大学")

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
          single.row=F, out="nku20200924.htm")

#main figure / all effects 
#label mean values

ep$pt=as.factor(ep$pt)

pre1 <- subset(ep, pt==0)
post1 <- subset(ep, pt==1)

summary(pre1$aware) #64.17
summary(post1$aware) #60.96

nk.aware <- ggplot(ep, aes(x=pt, y=aware)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=64.17,vjust=-1, label="64.17") +
  annotate("text",x=2,y=60.96,vjust=-1, label="60.96") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="南开大学", y="需求意识") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
nk.aware

summary(pre1$edu) #78.83
summary(post1$edu) #84.19

nk.edu <- ggplot(ep, aes(x=pt, y=edu)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=78.83,vjust=-1, label="78.83") +
  annotate("text",x=2,y=84.91,vjust=-1, label="84.91") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="南开大学", y="学业兴趣") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
nk.edu


summary(pre1$social) #80.77
summary(post1$social) #81.45

nk.social <- ggplot(ep, aes(x=pt, y=social)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=80.77,vjust=-1, label="80.77") +
  annotate("text",x=2,y=81.45,vjust=-1, label="81.45") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="南开大学", y="社会责任") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
nk.social

summary(pre1$phil) #82.00
summary(post1$phil) #81.08

nk.phil <- ggplot(ep, aes(x=pt, y=phil)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=82.00,vjust=-1, label="82.00") +
  annotate("text",x=2,y=81.08,vjust=-1, label="81.08") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="南开大学", y="公益慈善意向") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
nk.phil




##南京师范大学

pre <- read.csv("shanshi20200901.csv")
post <- read.csv("shanshi20200902.csv")

pre <- subset(pre, school=="南京师范大学")
post <- subset(post, school=="南京师范大学")

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
          single.row=F, out="nnu20200924.htm")

#main figure / all effects 
#label mean values

ep$pt=as.factor(ep$pt)

pre1 <- subset(ep, pt==0)
post1 <- subset(ep, pt==1)

summary(pre1$aware) #59.10
summary(post1$aware) #57.42

n.aware <- ggplot(ep, aes(x=pt, y=aware)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=59.10,vjust=-1, label="59.10") +
  annotate("text",x=2,y=57.42,vjust=-1, label="57.42") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="南师大", y="需求意识") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
n.aware

summary(pre1$edu) #62.66
summary(post1$edu) #62.86

n.edu <- ggplot(ep, aes(x=pt, y=edu)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=62.66,vjust=-1, label="62.66") +
  annotate("text",x=2,y=62.86,vjust=-1, label="62.86") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="南师大", y="学业兴趣") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
n.edu


summary(pre1$social) #73.28
summary(post1$social) #69.66

n.social <- ggplot(ep, aes(x=pt, y=social)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=73.28,vjust=-1, label="73.28") +
  annotate("text",x=2,y=69.66,vjust=-1, label="69.66") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="南师大", y="社会责任") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
n.social

summary(pre1$phil) #71.09
summary(post1$phil) #70.94

n.phil <- ggplot(ep, aes(x=pt, y=phil)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=71.09,vjust=-1, label="71.09") +
  annotate("text",x=2,y=70.94,vjust=-1, label="70.94") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="南师大", y="公益慈善意向") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
n.phil




##华东政法大学

pre <- read.csv("shanshi20200901.csv")
post <- read.csv("shanshi20200902.csv")

pre <- subset(pre, school=="华东政法大学")
post <- subset(post, school=="华东政法大学")

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
          single.row=F, out="eplu20200924.htm")

#main figure / all effects 
#label mean values

ep$pt=as.factor(ep$pt)

pre1 <- subset(ep, pt==0)
post1 <- subset(ep, pt==1)

summary(pre1$aware) #49.64
summary(post1$aware) #62.35

ep.aware <- ggplot(ep, aes(x=pt, y=aware)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=49.64,vjust=-1, label="49.64") +
  annotate("text",x=2,y=62.35,vjust=-1, label="62.35") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="华东政法大学", y="需求意识") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
ep.aware

summary(pre1$edu) #63.88
summary(post1$edu) #70.05

ep.edu <- ggplot(ep, aes(x=pt, y=edu)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=63.88,vjust=-1, label="63.88") +
  annotate("text",x=2,y=70.05,vjust=-1, label="70.05") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="华东政法大学", y="学业兴趣") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
ep.edu


summary(pre1$social) #69.59
summary(post1$social) #78.13

ep.social <- ggplot(ep, aes(x=pt, y=social)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=69.59,vjust=-1, label="69.59") +
  annotate("text",x=2,y=78.13,vjust=-1, label="78.13") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="华东政法大学", y="社会责任") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
ep.social

summary(pre1$phil) #71.02
summary(post1$phil) #75.19

ep.phil <- ggplot(ep, aes(x=pt, y=phil)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=71.02,vjust=-1, label="71.02") +
  annotate("text",x=2,y=75.19,vjust=-1, label="75.19") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="华东政法大学", y="公益慈善意向") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
ep.phil

multiplot(ep.aware, ep.edu, ep.social, ep.phil, cols=4)




##电子科技大学

pre <- read.csv("shanshi20200901.csv")
post <- read.csv("shanshi20200902.csv")

pre <- subset(pre, school=="电子科技大学")
post <- subset(post, school=="电子科技大学")

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
          single.row=F, out="etu20200924.htm")

#main figure / all effects 
#label mean values

ep$pt=as.factor(ep$pt)

pre1 <- subset(ep, pt==0)
post1 <- subset(ep, pt==1)

summary(pre1$aware) #49.64
summary(post1$aware) #62.35

et.aware <- ggplot(ep, aes(x=pt, y=aware)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=49.64,vjust=-1, label="49.64") +
  annotate("text",x=2,y=62.35,vjust=-1, label="62.35") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="电子科技大学", y="需求意识") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
et.aware

summary(pre1$edu) #63.88
summary(post1$edu) #70.05

et.edu <- ggplot(ep, aes(x=pt, y=edu)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=63.88,vjust=-1, label="63.88") +
  annotate("text",x=2,y=70.05,vjust=-1, label="70.05") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="电子科技大学", y="学业兴趣") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
ey.edu


summary(pre1$social) #69.59
summary(post1$social) #78.13

et.social <- ggplot(ep, aes(x=pt, y=social)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=69.59,vjust=-1, label="69.59") +
  annotate("text",x=2,y=78.13,vjust=-1, label="78.13") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="电子科技大学", y="社会责任") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
et.social

summary(pre1$phil) #71.02
summary(post1$phil) #75.19

et.phil <- ggplot(ep, aes(x=pt, y=phil)) + 
  stat_summary(fun.y=mean, geom="bar", color="black", fill="#FFFFFF") + 
  stat_summary(position=position_dodge(.9), fun.data=mean_sdl, fun.args = list(mult=1), 
               geom="errorbar", color="tomato", width=.2) +
  geom_jitter(color="grey", shape = 1) +
  annotate("text",x=1,y=71.02,vjust=-1, label="71.02") +
  annotate("text",x=2,y=75.19,vjust=-1, label="75.19") +
  theme_classic()+
  theme(text = element_text(family='FangSong')) +
  labs(x="电子科技大学", y="公益慈善意向") +
  scale_x_discrete(breaks=c(0, 1), labels=c("课前", "课后"))
et.phil


multiplot(p.aware, p.edu, p.social, p.phil, 
          nk.aware, nk.edu, nk.social, nk.phil,           
          n.aware, n.edu, n.social, n.phil, 
          ep.aware, ep.edu, ep.social, ep.phil, 
          et.aware, et.edu, et.social, et.phil, cols=5)
