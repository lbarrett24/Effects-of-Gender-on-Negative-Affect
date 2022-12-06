library(psychTools)
library(tidyverse)
library(ggplot2)
library(dplyr)

dataFQ22 <- msqR[msqR$time == 1,]
str(dataFQ22)
#as.data.frame(dataFQ22)

#creating data frames
df = dataFQ22 %>% select(id, study, gender,afraid, ashamed, distressed, guilty,
                          hostile, irritable , jittery, nervous, scared, upset)
#cleaning data
df_clean = na.omit(df)

#separating female and male participants
df_female = df_clean[df_clean$gender == 2,]
df_male = df_clean[df_clean$gender == 1,]

#creating scale scores
df_clean$scale_score <- rowMeans(df_clean[,4:13], na.rm=TRUE)

#counting total participants across gender
nrow(df_female)
nrow(df_male)

#checking for normality in female pop
shapiro.test(df_clean$scale_score[df_clean$gender == 2])
shapiro.test(log(df_clean$scale_score[df_clean$gender == 2]+.1))   #adding 1 to stop inf values
shapiro.test(sqrt(df_clean$scale_score[df_clean$gender == 2]))
shapiro.test(1/(df_clean$scale_score[df_clean$gender == 2]+.1))   #adding 1 to stop division by zero

#checking for normality in male pop
shapiro.test(df_clean$scale_score[df_clean$gender == 1])
shapiro.test(log(df_clean$scale_score[df_clean$gender == 1]+.1))    #adding 1 to stop inf values
shapiro.test(sqrt(df_clean$scale_score[df_clean$gender == 1]))
shapiro.test(1/(df_clean$scale_score[df_clean$gender == 1]+.1))   #adding 1 to stop division by zero

#checking for homoscedasticity
var(df_clean$scale_score[df_clean$gender == 2]) #female
var(df_clean$scale_score[df_clean$gender == 1]) #male


#qq-plot for normality
qqnorm(df_clean$scale_score[df_clean$gender == 2], 
       #main = "Figure 1\n Q-Q Plot - Female Participants",
       main= mtext("Q-Q Plot - Female Participants", side=3, adj=0,
                   cex = 2.4,font=2),
       cex.lab = 1.4,
       cex.main = 2.4) 
qqline(df_clean$scale_score[df_clean$gender == 2], col = 'red')

qqnorm(df_clean$scale_score[df_clean$gender == 1], 
       #main = "Figure 2\n Q-Q PLot - Male Participants",
       main= mtext("Q-Q PLot - Male Participants", side=3, adj=0, cex = 2.4,font=2),
       cex.lab = 1.4,
       cex.main = 2.4,
       )
qqline(df_clean$scale_score[df_clean$gender == 1], col = 'red')

#plotting frequency of Male & Female scale score
plot_histogram_female <- function(df, feature) {
    plt <- ggplot(df, aes(x=eval(parse(text=feature)))) +
      geom_histogram(aes(y = ..count..), alpha=0.7, fill="#33AADE", color="black") +
      geom_density(alpha=0.3, fill="red") +
      geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
      labs(x='Negative Affect(scale score)', y = "Density") + 
      ggtitle("Distribution of Negative Affect - Female Participants") + 
      theme(plot.title = element_text(size=30)) +
      theme(axis.text=element_text(size=18),
            axis.title=element_text(size=20,face="bold"))
    print(plt)
}
plot_histogram_male <- function(df, feature) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)))) +
    geom_histogram(aes(y = ..count..), alpha=0.7, fill="#33AADE", color="black") +
    geom_density(alpha=0.3, fill="red") +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x='Negative Affect(scale score)', y = "Density") + 
    ggtitle("Distribution of Negative Affect - Male Participants") +
    theme(plot.title = element_text(size=30)) +
    theme(axis.text=element_text(size=18),
          axis.title=element_text(size=20,face="bold"))
  print(plt)
}

#separating scale scores by gender
df_clean_female = data.frame(df_clean$scale_score[df_clean$gender == 2]) #female
colnames(df_clean_female) = c('scale_score')

df_clean_male = data.frame(df_clean$scale_score[df_clean$gender == 1]) #male
colnames(df_clean_male) = c('scale_score')

#plotting frequency of female population
plot_histogram_female(df_clean_female, 'scale_score')
plot_histogram_male(df_clean_male, 'scale_score') 

#plotting frequency of scale score by gender
hist_male = hist(df_clean$scale_score[df_clean$gender == 1], plot = FALSE)
hist_female = hist(df_clean$scale_score[df_clean$gender == 2], plot = FALSE)

#layered bar plot of scale score by gender
plot(hist_male, col = rgb(0,0,1,1/4), xlim=c(0,3))
plot(hist_female, col = rgb(1,0,0,1/4),xlim=c(0,3), add = TRUE)

#running t-test
t.test(df_clean$scale_score ~ df_clean$gender, data = df_clean)

#standard deviation of male & female groups
sd(df_clean$scale_score[df_clean$gender == 1])
sd(df_clean$scale_score[df_clean$gender == 2])

#mean of male & female groups
mean(df_clean$scale_score[df_clean$gender == 1])
mean(df_clean$scale_score[df_clean$gender == 2])


