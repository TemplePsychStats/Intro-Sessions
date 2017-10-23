# Setting options for displaying number, to minimize 
# use of scientific notation and to only show  a 
# limited number of decimals
options(scipen=100)
options(digits=3)

# SeriousStatsAllfunctions script - from Thom 
# Baguley.  We will use cm.ci.mixed function 
# to produce Cousineau-Morey confidence intervals 
# for a mixed design
source("http://www2.ntupsychology.net/seriousstats/SeriousStatsAllfunctions.txt")

# afex package       "aov_car" function to run our 
#                     mixed-design ANOVA
# effects package   "effect" function to calculate 
#                    predictions from a regression
# ggplot2 package   "ggplot" function to make graphs
# GGally package    "ggpairs" function to make 
#                    exploratory data analysis matrix
# jtools package    "sim_slopes" function to probe 
#                    interaction in regression
# pastecs package   "stat.desc" function to get summary
#                    statistics
# phia package      "testInteractions" function to do 
#                    contrast analyses in our ANOVA
# reshape2          "melt" to go from a person-level 
#                    dataframe to a person-period 
#                    dataframe--necessary for mixed-
#                    design ANOVA
# stargazer         "stargazer" to make nice regression 
#                    tables

install.packages("afex", dependencies=TRUE)
install.packages("effects", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("GGally", dependencies=TRUE)
install.packages("jtools", dependencies=TRUE)
install.packages("pastecs", dependencies=TRUE)
install.packages("phia", dependencies=TRUE)
install.packages("reshape2", dependencies=TRUE)
install.packages("stargazer", dependencies=TRUE)
library("afex")
library("effects")
library("ggplot2")
library("GGally")
library("jtools")
library("pastecs")
library("phia")
library("reshape2")
library("stargazer")



# Reading in Data

# tachistoscope experiment - subjects see either an "I"
# or a "T" and they have to correctly identify what 
# the letter is.
# outcome is response time in milliseconds
# between-group condition: age (young vs old)
# 1=young, 2=old
# (I prefer dummy coding (0,1) but the cm.ci.mixed 
# function will only work when categories are coded 
# starting at 1)
# within-group condition: angle at which the letter 
# appears off-center.
# seq function - we will ask R to make ID variable 
#                from 1 to 40
# rep function - we will ask R to assign the first 
#                20 cases to the "young" condition
#                (old=1) and the next 20 cases to 
#                the "old" condition (old=2)

tach<-data.frame(id=c(seq(from=1, to=40)),
                 old=c(rep(1,20), rep(2,20)),
                 angle0=c(450,390,570,450,510,360,
                          510,510,510,510,450,510,
                          480,470,460,490,570,450,
                          520,390,420,600,450,630,
                          420,600,630,480,690,510,
                          640,740,350,650,680,410,
                          570,570,410,590),
                 angle4=c(510,480,630,660,660,450,
                          600,660,660,540,550,650,
                          530,570,640,510,630,780,
                          440,550,570,720,540,660,
                          570,780,690,570,750,690,
                          640,750,620,670,700,560,
                          720,480,780,640),
                 angle8=c(630,540,660,720,630,450,
                          720,780,660,660,700,560,
                          670,670,580,490,440,760,
                          690,640,690,810,690,780,
                          780,870,870,720,900,810,
                          840,850,810,690,600,640,
                          920,830,820,730))

# creating factor variable for "old"
tach$oldf<-factor(tach$old, levels=c(1,2), 
                  labels=c("young", "old"))

# Looking at Data - note it is in "wide" format--
# one line per participant
tach

# Getting summary statistics by the old grouping 
# variable
by(tach[c("angle0", "angle4", "angle8")], 
   tach$old, stat.desc)

# Getting Cousineau-Morey confidence intervals 
# for within-subject designs
# Cousineau-Morey CIs essentially subtract the 
# subject mean from the raw scores and then 
# add the between-subject group mean, and 
# applies Morey adjustment
# see Baguley 2012 for details 
# http://dx.doi.org/10.3758/s13428-011-0123-7
# last variable listed has to be  grouping 
# variable

cm.ci.mixed(tach[c("angle0", "angle4", "angle8", 
                   "old")], difference=FALSE)

# Creating a new data frame containing sample 
# means and CIs
gtach<-data.frame(old=c(rep("Young",3), 
                        rep("Old", 3)),
                  angle=c(0,4,8,0,4,8),
                  meanvar=c(478,585,632,552,655,782),
                  lower=c(448,556,597,512,626,746),
                  upper=c(508,614,668,592,684,819))
gtach


# Graphing Means
# ggplot function is based on "grammar of graphics"
# the following is a combination of paraphrases and
# verbatim quotes from Hadley Wickham's book 
# "ggplot2: Elegant Graphis for Data Analysis: Second
# Edition", pp. 4-5
# plots are composed of:
#      DATA and AESthetic mappings linking data to 
#           aesthetic attributes
#      Layers made up of GEOMetric elements--
#           represent what we actually see on the plot
#      SCALEs mapping values in data space to values 
#           in an aesthetic space 
#      FACETs describe how to break up the data into 
#           subsets
#      themes controlling finer points of display, 
#           like LABels and TITLEs
#      Note that I "dodged" (aka "jittered") the error
#      bars a bit--just so they are distinguishable in
#      cases where the lines are too close to each 
#      other.
g<-ggplot(data=gtach, aes(x=angle, y=meanvar, 
                          linetype=old, color=old))+
  geom_line(size=1)+geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.25, 
                position=position_dodge(width=.2))+
  labs(linetype=" ", color=" ")+
  xlab("Angle")+
  ylab("Latency (ms)")+
  ggtitle("Latency By Subject Age and Letter Angle")+
  scale_x_continuous(breaks=c(0,4,8))+
  ylim(c(400,900))
g

# Reshaping tach dataframe into long format--one line
# per observation
tach_long<-melt(tach, id=c("id","old","oldf"), 
                Measured=c("angle0","angle4","angle8"))
# Looking at melted data
tach_long
# renaming "variable" to "angle" and "value" to 
# "latency"
names(tach_long)[names(tach_long)=="variable"]<-"angle"
names(tach_long)[names(tach_long)=="value"]<-"latency"

# doing omnibus test
m1<-aov_car(latency~oldf+Error(id/angle), 
            data=tach_long)
m1

# checking levels for angle and old factors to make 
# sure we have order correct
levels(tach_long$angle)
levels(tach_long$oldf)


# main effects contrasts, old vs young
# multiple comparisons not an issue
testInteractions(m1$lm, 
                 custom=list(oldf=c(-1,1)), 
                 idata=m1$data[["idata"]], 
                 adjustments="none")

# main effects contrasts, angle 0 vs angle 4
# note: sometimes phia will use alphabetical order 
# for your factors, I think this occurs for within-
# subject factors for some reason.
# not an issue here because angle already is in 
# alphanumerical order
testInteractions(m1$lm, 
                 custom=list(angle=c(-1,1,0)), 
                 idata=m1$data[["idata"]], 
                 adjustments="none")
# Tukey-adjusted F crit value:
((qtukey(1-(.05), 3, 38))^2)/(2)

# simple effects contrast: 
# old vs young at angle 0
testInteractions(m1$lm, 
                 custom=list(oldf=c(-1,1), 
                             angle=c(1,0,0)), 
                 idata=m1$data[["idata"]], 
                 adjustments="none")
# Tukey-adjusted F crit value:
((qtukey(1-(.05/3), 2, 38))^2)/(2)

# simple effects contrast: 
# angle4 vs angle 8 for young people
testInteractions(m1$lm, 
                 custom=list(oldf=c(1,0), 
                             angle=c(0,-1,1)), 
                 idata=m1$data[["idata"]], 
                 adjustments="none")
# Tukey-adjusted F crit value:
((qtukey(1-(.05/2), 3, 38))^2)/(2)


# interaction contrast: 
# angle 4 vs angle 8 for young vs old people
testInteractions(m1$lm, 
                 custom=list(oldf=c(-1,1), 
                             angle=c(0,-1,1)), 
                 idata=m1$data[["idata"]], 
                 adjustments="none")
# Scheffe-adjusted crit value:
2*qf(.95,2,38)


# regression example
# this is data on scientists
# timeout - years since PhD
# numpubs - # of publications
# numcites - citation ccount
# female - 1=female, 0=male

# reading in dataframe
sci<-data.frame(timeout=c(3,6,3,8,9,6,16,
                          10,2,5,5,6,7,11,
                          18,6,9,7,7,3,7,
                          5,7,13,5,8,8,7,
                          2,13,5,3,1,3,9,
                          3,9,3,4,10,1,11,
                          5,1,21,7,5,16,5,
                          4,5,11,16,3,4,4,
                          5,6,4,8,3,4),
                numpubs=c(18,3,2,17,11,6,38,
                          48,9,22,30,21,10,27,
                          37,8,13,6,12,29,29,
                          7,6,69,11,9,20,41,
                          3,27,14,23,1,7,19,
                          11,31,9,12,32,26,12,
                          9,6,39,16,12,50,18,
                          16,5,20,50,6,19,11,
                          13,3,8,11,25,4),
                female=c(1,1,1,0,1,0,0,0,0,0,
                         1,0,1,0,0,0,1,0,1,1,
                         1,0,0,0,0,1,1,1,1,0,
                         0,0,0,0,0,0,0,0,1,0,
                         0,0,0,0,0,1,1,0,0,1,
                         0,0,1,1,1,1,0,1,1,1,
                         1,1),
                numcite=c(50,26,50,34,41,37,48,
                          56,19,29,28,31,25,40,
                          61,32,36,69,47,29,35,
                          35,18,90,60,30,27,35,
                          14,56,50,25,35,1,69,
                          69,27,50,32,33,45,54,
                          47,29,69,47,43,55,33,
                          28,42,24,31,27,83,49,
                          14,36,34,70,27,28),
                salary=c(51876,54511,53425,61863,
                         52926,47034,66432,61100,
                         41934,47454,49832,47047,
                         39115,59677,61458,54528,
                         60327,56600,52542,50455,
                         51647,62895,53740,75822,
                         56596,55682,62091,42162,
                         52646,74199,50729,70011,
                         37939,39652,68987,55579,
                         54671,57704,44045,51122,
                         47082,60009,58632,38340,
                         71219,53712,54782,83503,
                         47212,52840,53650,50931,
                         66784,49751,74343,57710,
                         52676,41195,45662,47606,
                         44301,58582))

# creating factor for female
sci$femalef<-factor(sci$female, levels=c(0,1), 
                    labels=c("Male", "Female"))


# ggpairs - making exploratory data analysis
# note I have found it is best to specify
# factor variables LASTE in list of 
# variables.
# matrix
# lower argument - specifying we want loess 
# scatterplots below diagonal
# showstrips - will add "male" and "female"
# labels to bivariate histograms
ggpairs(data=sci[c("salary", "timeout", "numpubs", 
                   "numcite", "femalef")], 
        lower = list(continuous = "smooth_loess"),
        showStrips=TRUE)


# regressions - main effects only 
m2<-lm(salary~timeout+numpubs+numcite+femalef, 
       data=sci)

# now let's add three-way interaction
# note "*" operator adds interaction
# and all component parts (all component
# two-way interactions and main effects)
# ":" operator just adds interaction
m3<-lm(salary~timeout*femalef*numcite+numpubs+
         numpubs:femalef, data=sci)

# get nice table for these two models
stargazer(m2, m3, type="text")

# that's nice, but let's change the order so the
# interaction terms are at the bottom but above 
# the intercept
stargazer(m2, m3, type="text", 
          order=c(8,9,1,2,5,3,4,7,6,10))


# let us get summary stats for numcite and timeout
# pay attention to first and third quartiles for 
# those variables
summary(sci$numcite)
summary(sci$timeout)


# using effect function in effects package to 
# calculate predictions showing three-way
# interaction
# using Q1 and Q3 for timeout and numcite
# to get 

m3eff<-effect(term="timeout*femalef*numcite", 
              mod=m3, 
              xlevels=list(female=c(0,1), 
                           numcite=c(28.2,50),
                           timeout=c(4,8.75)))
m3eff

#convert eff object into dataframe so we can
# graph the effects
m3effdf<-data.frame(m3eff)


# making a timeoutf factor variable
m3effdf$timeoutf<-factor(m3effdf$timeout, 
                         levels=c(4,8.75), 
                         labels=c("4 (Q1)", "8.8 (Q3)"))
m3effdf

# using ggplot2 to graph predictions showing
# 3-way interaction
g3<-ggplot(data=m3effdf, 
           aes(x=numcite, y=fit, color=timeoutf, 
               linetype=timeoutf))+
  facet_wrap(~femalef)+
  geom_line(size=1)+
  ggtitle("Salary By Gender, Citations, and 
          Years Since PhD")+
  labs(color="Years Since PhD", 
       linetype="Years Since PhD")+
  xlab("Number of Citations")+
  ylab("Salary ($)")
g3

# let's probe the interaction
# examining effect of numcite contingent on 
# timeout and sex
sim_slopes(model=m3, pred=numcite, modx=timeout, 
           mod2=femalef, modxvals=c(4,8.75), 
           mod2vals=c(0,1))

