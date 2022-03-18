# Here the sample size is less than 50 so, Shapiro-Wilk test is perfomed. 
# For sample
# size more than 50 Kolmogorov-Smirnov is carried out.

#Q-Q PLOT

qqnorm(mtcar$mpg,frame="F",las=1,ylim = range(c(10,40)))
qqline(mtcar$mpg,col="steelblue",lwd=2)

#----normal distribution and creating segment
#of 67-95-97% in a normally distributed data---

library(ggplot2)
pop_mean <- 50
pop_sd <- 5
x <- seq(-4,4,length = 100) * pop_sd + pop_mean
y <- dnorm(x,pop_mean,pop_sd)

df <- data.frame(x,y)

ggplot(df,aes(x,y))+
  geom_line()+
  labs(x = "",y="")+
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  geom_vline(xintercept = pop_mean,col="red")+
  geom_vline(xintercept = pop_mean + pop_sd,col="blue")+
  geom_vline(xintercept = pop_mean - pop_sd,col="blue")+
  geom_vline(xintercept = pop_mean + 2 * pop_sd,col = "green")+
  geom_vline(xintercept = pop_mean - 2 * pop_sd,col="green")+
  geom_vline(xintercept = pop_mean +3 * pop_sd,col = "steelblue")+
  geom_vline(xintercept = pop_mean - 3 * pop_sd,col="steelblue")+
  
  geom_segment(x=45,y=.04,xend=55,yend=.04,arrow = arrow(ends = "both",angle = 20))+
  annotate("text",x=50,y=.043,label="67%",col="blue")+
  
  geom_segment(x=40,y=.053,xend=60,yend=.053,arrow = arrow(ends="both",angle = 20))+
  annotate("text",x=50,y=.056,label="95%",col="green")+
  
  geom_segment(x=35,y=.066,xend=65,yend=.066,arrow = arrow(ends = "both",angle = 20))+
  annotate("text",x=50,y=.069,label="97%",col="steelblue")