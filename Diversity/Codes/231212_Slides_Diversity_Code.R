#### Code for EPSA - two data sets - December 2023: 

# how to run the code: 
library(tidyverse)
library(stargazer)
library(kableExtra)
library(DCchoice)
library(here)


library("rstudioapi")     
##Set working directory
setwd(dirname(getActiveDocumentContext()$path))
getwd()
setwd("../")
getwd()

prepilot2 <- read_csv("data/EPSA_Diversity.csv")

# Graphs below: 
# Age: 
p20 = ggplot(data = prepilot2, aes(x = Q6.2)) +
  #geom_histogram() + 
  geom_vline(aes(xintercept = mean(Q6.2, na.rm = T)), linetype = "dashed") +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.1, by = 0.01)) +
  #geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
  #          stat = "count", vjust = -0.5, size = 6) +
  labs(x = "Year",
       y = "Proportion") + 
  ggtitle("What year were you born?")
p20

p20_2 = ggplot(data = prepilot2, aes(x = AgeNum)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.1, by = 0.01)) +
  geom_vline(aes(xintercept = mean(AgeNum, na.rm = T)), linetype = "dashed") +
  labs(x = "Age",
       y = "Proportion") + 
  ggtitle("What is your age?")
p20_2

p21 = ggplot(prepilot2, aes(x = Q6.3)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.9)) + 
  ggtitle("Are you hampered in your daily activities in any way \n by any longstanding illness, disability or mental \n health problem?")
p21

p22 = ggplot(prepilot2, aes(x = Q6.4)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 1.0)) +
  ggtitle("Do you identify as disabled / having a disability?")
p22


p23 = ggplot(prepilot2, aes(x = Q6.7)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.7)) +
  ggtitle("Do you consider yourself to be a first-generation scholar? We generally speak of first-generation scholars when none of the parents received a university degree.")
p23

p24 = ggplot(prepilot2, aes(x = Q6.5)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 1.0)) +
  ggtitle("Do you identify as a person of color?") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
p24

p24_2 = ggplot(prepilot2, aes(x = Q6.6)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.9)) +
  ggtitle("Which of the following best describes you?") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
p24_2

p25 = ggplot(prepilot2, aes(x = Q6.8_10_aggregated)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.7)) +
  ggtitle("Q6.8 Which of the following best describes you?") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
p25

p26 = ggplot(prepilot2, aes(x = Q6.9_agg)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.7))+
  ggtitle("What is your gender identity?") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
p26

p27 = ggplot(prepilot2, aes(x = Q6.10)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.8))+
  ggtitle("Do you identify as transgender?") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
p27  







