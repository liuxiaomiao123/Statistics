## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}



#-------------------------------画Errobar-------------------------------------------------------
library(ggplot2)

#data<-read.csv("C:/Users/liuxiaomiao/Desktop/haiyang/RESULT/ANOVA_2x2/nodelete/new/result2/conjunction_0.005_45/2-0_s-c/2-0_s-c.csv")
#data<-read.csv("C:/Users/liuxiaomiao/Desktop/haiyang/RESULT/ANOVA_2x2/nodelete/new/result2/main effect_group/main_effect_group_0.005_45.csv")

data<-read.csv("C:/Users/liuxiaomiao/Desktop/haiyang/RESULT/2T_2vs0_activation/result/2b.csv")

data<-read.csv("C:/Users/liuxiaomiao/Desktop/haiyang/RESULT/Network2/anova.csv")
data <- read.csv("D:/brainbnu/haiyang/hddm/result/all/avt/result_a.csv")
head(data)
data2<-summarySE(data,measurevar = 'mean', groupvars=c('cond','group'))  #注意是锘缩
head(data2)


ggplot(data2,aes(group,mean ,fill=factor(cond)))+geom_bar(position = position_dodge(),stat='identity',color='black',size=.72)+
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se),size=.9,width=.2,position=position_dodge(.9))+
  #geom_jitter(position=position_jitter(w=0.1, h=0.1), size=1.5)+
  geom_point(data,mapping = aes(group,mean,color = factor(cond)),position = position_jitterdodge(),size = 3)+
  scale_color_manual(values=c("coral1","turquoise"),name = "",labels = c("stress", "control"))+
  scale_fill_manual(values=c("orangered","darkturquoise"),guide = FALSE)+
  xlab('')+
  #ylab('Activation estimates(a.u.)')+
  ylab("")+
  #labs(fill = "condition")+
  #labs(color = "condition")+
  #scale_fill_discrete(breaks = c(0,2),labels = c("0back", "2back"))+
  #scale_y_continuous(breaks = c(-1,0,1,2,3),limits = c(-1,3))+
  #scale_y_continuous(breaks = c(-0.5,0,0.5,1,1.5,2,2.5),limits = c(-0.55,2.5))+
  #scale_y_continuous(expand = c(0,0))+
  #scale_x_continuous(breaks = c(1,2,3,4,5),labels = c("acc","pcc","mpfc","precu_L","precu_R"))+
  #scale_x_continuous(breaks = c(4,5,6,7),labels = c("acc_L","acc_R","pcc_L","pcc_R"))+
  #scale_x_continuous(breaks = c(1,2),labels = c("insula_L","insula_R"))+
  theme_bw()+ 
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(color='black',size = 1),
        axis.text.x=element_text(size = 12, face = "bold"),
        axis.text.y=element_text(size = 12,face = "bold"),
        axis.ticks.y = element_line(size = 1), 
        axis.ticks.x = element_blank(), #删去刻度线
        # axis.ticks.x = element_line(size = 0), 
        axis.ticks.length.y = unit(0.15, 'cm'))
#移除背景颜色和网格



#------------------------ANOVA and contrasts---------------------------------
install.packages("afex")
install.packages("lsmeans")
install.packages("emmeans")

library(afex)
library(lsmeans)
library(emmeans)

data<-read.csv("C:/Users/liuxiaomiao/Desktop/haiyang/RESULT/ANOVA_2x2/nodelete/new/result2/main effect_group/main_effect_group_0.005_45.csv")
data<-read.csv("C:/Users/liuxiaomiao/Desktop/haiyang/RESULT/ANOVA_2x2/nodelete/new/result2/conjunction_0.005_45/0-2_s-c/0-2_s-c.csv")

head(data)
str(data)

#fit <- aov(data$dacc~data$cond * data$group + Error(id/data$cond),data)    #spm好像用的是这种模型?
#summary(fit)

fit_all <- aov_ez("id","pcc_true",data,between=c("group"),within=c("cond"))  # aov_ez is identical to glm in spss
#fit_all <- mixed(insula_L ~ group * cond, data=data)
summary(fit_all)

ref <- lsmeans(fit_all,specs = c("group","cond"))
ref   # s0,c0,s2,c2

#-----the difference between aov and aov_ez and other ways to do anova,see https://www.rdocumentation.org/packages/afex/versions/0.26-0/topics/aov_car



contrast1 = c(-1,-1,1,1)  #group_2-0
contrast1 = c(1,-1,1,-1)  #group_s-c

contrast1 = c(-1,1,1,-1)  #interaction_s-c
summary(contrast(ref,list(s_vs_c = contrast1)))

a <- emmeans(fit, ~group)
pairs(a)       #看主效应是一样的


#https://cran.r-project.org/web/packages/afex/vignettes/afex_anova_example.html
#https://tysonbarrett.com/jekyll/update/2018/03/14/afex_anova/


#https://www.psychologie.uni-heidelberg.de/ae/meth/team/mertens/blog/anova_in_r_made_easy.nb.html


data$group <- factor(data$group,levels = c(1,2),
                     labels = c("stress", "control"))
data$cond <- factor(data$cond,levels = c(0,2),
                    labels = c("0back", "2back"))



data2 <- split(data,data$group)
stress <- data2$`1`
control <- data2$`2`

stress_0back <-split(stress,stress$cond) $'0'
stress_2back <-split(stress,stress$cond) $'2'

control_0back <-split(control,control$cond) $'0'
control_2back <-split(control,control$cond) $'2'

m1 <- stress_2back$acc - stress_0back$acc
m2 <- control_2back$acc - control_0back$acc

t.test(m1,m2,var.equal = FALSE)




