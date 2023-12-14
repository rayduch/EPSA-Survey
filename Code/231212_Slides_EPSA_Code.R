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

prepilot <- read_csv("data/EPSA_Main.csv")

# Graphs: 
p5 = ggplot(prepilot, aes(x = Q2.3)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", hjust = -0.5, size = 3) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.35)) +
  coord_flip() + 
  ggtitle("What country do you currently work in?")
p5

p5_2 = ggplot(prepilot, aes(x = Q2.4)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat ="count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", hjust = -0.5, size = 3) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.3)) +
  coord_flip() +
  ggtitle("What is your nationality?")
p5_2

p6 = ggplot(prepilot, aes(x = Q2.1)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.4)) +
  ggtitle("How would you describe your current position?") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
p6

p6_2 = ggplot(prepilot, aes(x = Q2.2)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 1.0)) + 
  ggtitle("How would you describe your institutional \n affiliation?")
p6_2

p7 = ggplot(prepilot, aes(x = Q2.5)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.3)) + 
  ggtitle("Could you give us an idea of how many EPSA conferences prior to 2023 have you attended?")
p7


i = 1
p9 = (ggplot(data = prepilot, aes_string(x = paste0("Q2.6_", i))) +
        geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
        scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
        geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                  stat = "count", vjust = -0.25, size = 6) +
        #geom_histogram() + 
        geom_vline(aes(xintercept = mean(.data[[paste0("Q2.6_", i)]], na.rm = T)), linetype = "dashed") +
        labs(x = "Agreement Degree",
             y = "Proportion",
             title = case_when(#i == 1 ~ paste(strwrap("The EPSA Annual meeting is the top ranked annual political science conference.", width = 68), collapse = "\n"),
               i == 1 ~ paste(("The EPSA Annual meeting is the top ranked annual political science conference.")),
               i == 2 ~ paste(("The EPSA governing institutions represent the interests of the European political science community.")),
               i == 3 ~ paste(("The members of the EPSA Council adequately represent the preferences of the EPSA membership.")),
               i == 4 ~ paste(("There is no need to change the procedures for selecting the President of EPSA.")),
               i == 5 ~ paste(("The ownership structure of EPSA has served the organization well.")))) +
        scale_x_continuous(breaks = seq(0, 100, by = 10)))
p9 

i = 2
p10 = (ggplot(data = prepilot, aes_string(x = paste0("Q2.6_", i))) +
         geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
         scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
         geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                   stat = "count", vjust = -0.25, size = 6) +
         #geom_histogram() + 
         geom_vline(aes(xintercept = mean(.data[[paste0("Q2.6_", i)]], na.rm = T)), linetype = "dashed") +
         labs(x = "Agreement Degree",
              y = "Proportion",
              title = case_when(#i == 1 ~ paste(strwrap("The EPSA Annual meeting is the top ranked annual political science conference.", width = 68), collapse = "\n"),
                i == 1 ~ paste(("The EPSA Annual meeting is the top ranked annual political science conference.")),
                i == 2 ~ paste(("The EPSA governing institutions represent the interests of the European political science community.")),
                i == 3 ~ paste(("The members of the EPSA Council adequately represent the preferences of the EPSA membership.")),
                i == 4 ~ paste(("There is no need to change the procedures for selecting the President of EPSA.")),
                i == 5 ~ paste(("The ownership structure of EPSA has served the organization well.")))) +
         scale_x_continuous(breaks = seq(0, 100, by = 10)))
p10

i = 3
p11 = (ggplot(data = prepilot, aes_string(x = paste0("Q2.6_", i))) +
         geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
         scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
         geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                   stat = "count", vjust = -0.25, size = 6) +
         #geom_histogram() + 
         geom_vline(aes(xintercept = mean(.data[[paste0("Q2.6_", i)]], na.rm = T)), linetype = "dashed") +
         labs(x = "Agreement Degree",
              y = "Proportion",
              title = case_when(#i == 1 ~ paste(strwrap("The EPSA Annual meeting is the top ranked annual political science conference.", width = 68), collapse = "\n"),
                i == 1 ~ paste(("The EPSA Annual meeting is the top ranked annual political science conference.")),
                i == 2 ~ paste(("The EPSA governing institutions represent the interests of the European political science community.")),
                i == 3 ~ paste(("The members of the EPSA Council adequately represent the preferences of the EPSA membership.")),
                i == 4 ~ paste(("There is no need to change the procedures for selecting the President of EPSA.")),
                i == 5 ~ paste(("The ownership structure of EPSA has served the organization well.")))) +
         scale_x_continuous(breaks = seq(0, 100, by = 10)))
p11

i = 4
p12 = (ggplot(data = prepilot, aes_string(x = paste0("Q2.6_", i))) +
         geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
         scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
         geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                   stat = "count", vjust = -0.25, size = 6) +
         #geom_histogram() + 
         geom_vline(aes(xintercept = mean(.data[[paste0("Q2.6_", i)]], na.rm = T)), linetype = "dashed") +
         labs(x = "Agreement Degree",
              y = "Proportion",
              title = case_when(#i == 1 ~ paste(strwrap("The EPSA Annual meeting is the top ranked annual political science conference.", width = 68), collapse = "\n"),
                i == 1 ~ paste(("The EPSA Annual meeting is the top ranked annual political science conference.")),
                i == 2 ~ paste(("The EPSA governing institutions represent the interests of the European political science community.")),
                i == 3 ~ paste(("The members of the EPSA Council adequately represent the preferences of the EPSA membership.")),
                i == 4 ~ paste(("There is no need to change the procedures for selecting the President of EPSA.")),
                i == 5 ~ paste(("The ownership structure of EPSA has served the organization well.")))) +
         scale_x_continuous(breaks = seq(0, 100, by = 10)))
p12

i = 5
p13 = (ggplot(data = prepilot, aes_string(x = paste0("Q2.6_", i))) +
         geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
         scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
         geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                   stat = "count", vjust = -0.25, size = 6) +
         #geom_histogram() + 
         geom_vline(aes(xintercept = mean(.data[[paste0("Q2.6_", i)]], na.rm = T)), linetype = "dashed") +
         labs(x = "Agreement Degree",
              y = "Proportion",
              title = case_when(#i == 1 ~ paste(strwrap("The EPSA Annual meeting is the top ranked annual political science conference.", width = 68), collapse = "\n"),
                i == 1 ~ paste(("The EPSA Annual meeting is the top ranked annual political science conference.")),
                i == 2 ~ paste(("The EPSA governing institutions represent the interests of the European political science community.")),
                i == 3 ~ paste(("The members of the EPSA Council adequately represent the preferences of the EPSA membership.")),
                i == 4 ~ paste(("There is no need to change the procedures for selecting the President of EPSA.")),
                i == 5 ~ paste(("The ownership structure of EPSA has served the organization well.")))) +
         scale_x_continuous(breaks = seq(0, 100, by = 10)))
p13

# Willingess/WTP: 
# Current style: 
wtp_p1 = ggplot(prepilot, aes(x = Q3.1)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion",
       title = "") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.7)) + 
  ggtitle("Q3.1 One funding model for professional associations is soliciting members to invest in the association – similar to a start-up fund in which your investment is associated with partial ownership. \n Would you be willing to invest in order to fund a European political science professional association?")
wtp_p1  

# pie chart: 
prepilot$Q3.1 = ifelse(is.na(prepilot$Q3.1)==TRUE | prepilot$Q3.1 == "Prefer not to say" , "N/A", prepilot$Q3.1)

# Create Data
wtp_table1 = prepilot %>% group_by(Q3.1) %>% summarise(n = n()) %>% mutate(Freq = n/sum(n))
wtp_table1$Q3.1 = factor(wtp_table1$Q3.1, levels = (c("Yes", "No", "Do not know", "N/A")) ) 

# Basic piechart
ggplot(wtp_table1, aes(x="", y=n, fill=Q3.1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(x = NULL,
       y = NULL) +
  ggtitle("One funding model for professional associations is soliciting members to invest in the association – similar to a start-up fund in which your investment is associated with partial ownership. \n Would you be willing to invest in order to fund a European political science professional association?")

ggplot(wtp_table1, aes(x="", y=Freq*100, fill=Q3.1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  labs(x = NULL,
       y = NULL) +
  ggtitle("One funding model for professional associations is soliciting members to invest in the association – similar to a start-up fund in which your investment is associated with partial ownership. \n Would you be willing to invest in order to fund a European political science professional association?")

ggplot(wtp_table1, aes(x = "", y = Freq*100, fill = Q3.1)) +
  geom_col(color = "black") +
  geom_text(aes(label = round(Freq*100,0)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(x = NULL,
       y = NULL) +
  ggtitle("One funding model for professional associations is soliciting members to invest in the association \n similar to a start-up fund in which your investment is associated with partial ownership. \n Would you be willing to invest in order to fund a European political science professional association?")

ggplot(wtp_table1, aes(x = "", y = Freq*100, fill = Q3.1)) +
  geom_col(color = "black") +
  geom_text(aes(label = round(n)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(x = NULL,
       y = NULL) +
  ggtitle("One funding model for professional associations is soliciting members to invest in the association \n similar to a start-up fund in which your investment is associated with partial ownership. \n Would you be willing to invest in order to fund a European political science professional association?")


# other version 
require(ggforce)
wtp_table1$focus = c(0,0,-1,0)
wtp_table2 = wtp_table1[c(4,3,2,1),]
wtp_table2$focus = c(0.25,0,0,0)

ggplot(wtp_table2) + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1, amount = Freq*100, 
                   fill = Q3.1, explode = focus), stat = 'pie') + 
  scale_fill_brewer('', palette = 'Set1') +
  coord_fixed() +
  labs(x = NULL,
       y = NULL) +
  ggtitle("One funding model for professional associations is soliciting members to invest in the association \n similar to a start-up fund in which your investment is associated with partial ownership. \n Would you be willing to invest in order to fund a European political science professional association?") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank())

#table(prepilot$Q3.2)
#table(prepilot$Q3.3)
#table(prepilot$Q3.4)

# Invest XX amount 
#Q3.1, Q3.2, Q3.3
results = list()
results_store = list()
for (i in 1 : length(unique(na.omit(prepilot$investExtra1)))){
  working_file = prepilot[which(prepilot$investExtra1 == unique(na.omit(prepilot$investExtra1))[i]),c("Q3.2","Q3.3","Q3.4")]
  base_amount = 0 
  higher_amount = 0 
  lower_amount = 0
  working_file$higher = NA
  working_file$base = NA
  working_file$lower = NA
  working_file$amount = NA
  for(j in 1:dim(working_file)[1]){
    if(working_file[j,1] %in% "Yes" & working_file[j,2] %in% "Yes"){
      higher_amount = higher_amount + 1
      working_file$amount[j] = as.character(2*as.numeric(unique(na.omit(prepilot$investExtra1))[i]))
    } 
    if(working_file[j,1] %in% "Yes" & working_file[j,2] %in% c(NA,"No", "Do not know", "Prefer not to say")  ) {
      base_amount = base_amount + 1
      working_file$amount[j] = as.character(1*as.numeric(unique(na.omit(prepilot$investExtra1))[i]))
    } 
    if(working_file[j,1] %in% "No" & working_file[j,2] %in% c(NA,"No", "Do not know", "Prefer not to say") & working_file[j,3] %in% "Yes"){
      lower_amount = lower_amount + 1
      working_file$amount[j] = as.character(0.5*as.numeric(unique(na.omit(prepilot$investExtra1))[i]))
    }
  }
  results[[i]] = as.data.frame(rbind(base_amount, higher_amount, lower_amount))
  results[[i]]$Money = c(unique(na.omit(prepilot$investExtra1))[i], 
                         as.character(2*as.numeric(unique(na.omit(prepilot$investExtra1))[i])), 
                         as.character(0.5*as.numeric(unique(na.omit(prepilot$investExtra1))[i])))
  colnames(results[[i]]) = c("N", "Money")
  results_store[[i]] = working_file
}# end of loop 
summary_results = do.call(rbind, results)
tab_invest = summary_results %>% group_by(Money) %>% summarise(n = sum(N))

tab_invest$Money = factor(tab_invest$Money, levels = c("25","50", "100", "200", "250", "400", "500", "1000", "2000"))

summary_results2 = do.call(rbind, results_store)
summary_results2$Money = factor(summary_results2$amount, levels = c("25","50", "100", "200", "250", "400", "500", "1000", "2000"))

# Final Graph for investing question: 
p29 = ggplot(summary_results2[-which(is.na(summary_results2$Money)==TRUE),], aes(x = Money)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, size = 6) +
  labs(x = NULL,
       y = "Proportion") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.3))+
  ggtitle("Would you be willing to invest?") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
p29  

### Membership  and Conference fees: 
# Q3.6_1
# Q3.5 Here is a range of MEMBERSHIP fees that are charged by professional associations in the social sciences. 
#Can you indicate whether these are acceptable membership fees by checking yes or no beside each amount
table(prepilot$Q3.6_1) # 200
table(prepilot$Q3.6_2) # 300
table(prepilot$Q3.6_3) # 400
table(prepilot$Q3.6_4) # 500

e_200 = as.data.frame(prop.table(table(prepilot$Q3.6_1)))[2,]
e_300 = as.data.frame(prop.table(table(prepilot$Q3.6_2)))[2,]
e_400 = as.data.frame(prop.table(table(prepilot$Q3.6_3)))[2,]
e_500 = as.data.frame(prop.table(table(prepilot$Q3.6_4)))[2,]

e_tab = rbind(e_200, e_300, e_400, e_500)
#e_tab$Amount = c("200", "200","300", "300", "400", "400", "500", "500")
e_tab$Amount = c("200", "300", "400", "500")

e_tab =  e_tab[,-1]
e_tab

p_conf<-ggplot(data=e_tab, aes(x=Amount, y=Freq)) +
  geom_bar(stat="identity") + 
  labs(x = NULL,
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.3))+
  ggtitle("Can you indicate whether these are acceptable conference fees?") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
p_conf


m_200 = as.data.frame(prop.table(table(prepilot$Q3.5_1)))[2,]
m_300 = as.data.frame(prop.table(table(prepilot$Q3.5_2)))[2,]
m_400 = as.data.frame(prop.table(table(prepilot$Q3.5_3)))[2,]
m_500 = as.data.frame(prop.table(table(prepilot$Q3.5_4)))[2,]

m_tab = rbind(m_200, m_300, m_400, m_500)
#e_tab$Amount = c("200", "200","300", "300", "400", "400", "500", "500")
m_tab$Amount = c("200", "300", "400", "500")

m_tab =  m_tab[,-1]
m_tab

p_memb<-ggplot(data=m_tab, aes(x=Amount, y=Freq)) +
  geom_bar(stat="identity") + 
  labs(x = NULL,
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.3))+
  ggtitle("Can you indicate whether these are acceptable membership conference fees?") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
p_memb



#Q7.1 We are interested in what you think are the most important features of professional conferences. Please, re-arrange the list below to rank your preferences. The top choice will be the most important feature and the bottom one will be the less important.
#______ Cost - registration fees (1) 
#______ The city in which the conference is held (2)
#______ The quality of the panels (3)
#______ The large number of attendees at the conference (4)
#______ The organized social activities (5)
#______ The quality of the conference organization (6)
#______ Paper submission and panel listing software (7)


# 
Q1.7_means = as.data.frame(
  c(mean(as.numeric(prepilot$Q7.1_1),na.rm = TRUE),
    mean(as.numeric(prepilot$Q7.1_2),na.rm = TRUE),
    mean(as.numeric(prepilot$Q7.1_3),na.rm = TRUE),
    mean(as.numeric(prepilot$Q7.1_4),na.rm = TRUE),
    mean(as.numeric(prepilot$Q7.1_5),na.rm = TRUE),
    mean(as.numeric(prepilot$Q7.1_6),na.rm = TRUE),
    mean(as.numeric(prepilot$Q7.1_7),na.rm = TRUE)
  ))
Q1.7_means$labels = c("Registration fee", "City", "Panel quality", 
                      "Number of delegates", "Social activities",
                      "Organization quality", "Submission software")
colnames(Q1.7_means)[1] = "Value"

p_mean<-ggplot(data = Q1.7_means, aes(x=labels, y=Value)) +
  geom_bar(stat="identity") + 
  labs(x = NULL,
       y = "Ranking") +
  scale_x_discrete(labels = scales::label_wrap(15)) +
  expand_limits(y=c(0, 0.3))+
  ggtitle("Conference Preferences") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))
p_mean


#### Conjoint plots: 
data <- prepilot %>%
  select(starts_with(c("profile")), ends_with("Q5.2")) %>%
  rowid_to_column("id") %>%
  rename(profile1_ans = "1_Q5.2",
         profile2_ans = "2_Q5.2",
         profile3_ans = "3_Q5.2",
         profile4_ans = "4_Q5.2",
         profile5_ans = "5_Q5.2")

# Install from the link: 
#install.packages("https://cran.r-project.org/src/contrib/Archive/dummies/dummies_1.5.6.tar.gz", repos=NULL)
library(dummies)

data <- data %>% 
  pivot_longer(-id, 
               names_to = c("person", ".value"), 
               names_pattern = "profile([1-5])(.*)",
               values_drop_na = TRUE) %>%
  filter(!is.na(a)) %>%
  pivot_longer(c(a,b), 
               names_to = "candidate", 
               values_to = "aux2") %>%
  separate(aux2, c("size", "procedures", "bodies", "ownership"),
           "\\|")

data <- data %>%
  mutate(candidate = if_else(candidate == "a", "Profile A", "Profile B"),
         select = if_else(`_ans` == candidate, 1, 0))

data$size <- data$size %>%
  factor(levels = c("500 conference participants", 
                    "1000 conference participants",
                    "1500 conference participants",
                    "2000 + conference participants"))

data$procedures <- data$procedures %>%
  factor(levels = c("Executive group that manages the association and annual meeting",
                    "Council and President that manage the association and annual meeting", 
                    "Council that manages the association and annual meeting",
                    "President that manages the association and annual meeting",
                    "Executive group that manages the annual meeting and a Council and president that manage the association"))

data$bodies <- data$bodies %>%
  factor(levels = c("Elected council that appoints a president", 
                    "Elected council and elected president", 
                    "Non-elected executive that appoints council and president", 
                    "Non-elected executive, elected Council and elected President", 
                    "Non-elected executive, elected Council and appointed President"))

data$ownership <- data$ownership %>%
  factor(levels = c("Not-for-profit ownership structure",
                    "Private-for-profit ownership structure", 
                    "Cooperative ownership structure – membership ownership"))

#### .Summary ####

conjoint <- data %>%
  select("size", "procedures", "bodies", "ownership")

conjoint <- as.data.frame(conjoint)
conjoint <- dummy.data.frame(conjoint, dummy.classes = "ALL")

dummyMeanscj <- round(as.double(colMeans(conjoint)),2)
dummyN <- as.numeric(colSums(conjoint))
dummySD <- round(as.double(apply(conjoint, 2, sd)),2)

var <- c("500 conference participants", 
         "1000 conference participants",
         "1500 conference participants",
         "2000 + conference participants",
         "Executive group that manages the association and annual meeting",
         "Council and President that manage the association and annual meeting", 
         "Council that manages the association and annual meeting",
         "President that manages the association and annual meeting",
         "Executive group that manages the annual meeting and a Council and president that manage the association",
         "Elected council that appoints a president", 
         "Elected council and elected president", 
         "Non-elected executive that appoints council and president", 
         "Non-elected executive, elected Council and elected President", 
         "Non-elected executive, elected Council and appointed President",
         "Not-for-profit ownership structure",
         "Private-for-profit ownership structure", 
         "Cooperative ownership structure – membership ownership")

sum_conjoint <- cbind(var,dummyMeanscj,dummySD,0,1,dummyN)

colnames(sum_conjoint) <- c("Conjoint attribute variable","Mean","SD","Min.","Max.","N")


##### Functions #####
# Include code to replace standard errors with robust S.Es
robustse.f <- function(model, cluster, df_correction) {
  ## Huber-White heteroskedasticity-robust standard error calculation and table generation code for lm and glm models in R.
  ##Written by Joshua Gubler ~  http://joshuagubler.com.  Note that the second half of this function is just a wrapper for the excellent "multiwaycov" package here: https://cran.r-project.org/web/packages/multiwayvcov/multiwayvcov.pdf .  Love the authors of that package...
  ##Last updated: 3 July 2017  
  
  #model = the model you estimated, now calculated with robust standard errors
  #cluster = the name of the variable on which you will cluster. Put a tilda in front of it (e.g. ~ state).  If you don't put in a cluster, you will simply get huber-white robust errors.
  #df_correction: If you do not want the number of levels in your cluster variable to count against your degrees of freedom (like the xt- options in Stata), then type "F".  Otherwise, type "T" and these levels will be counted against your degrees of freedom
  
  require(sandwich)
  require(lmtest)
  require(multiwayvcov)
  if(missing(cluster)) {
    name <- deparse(substitute(model))
    modelname <- paste(name,"rob",sep=".")
    model$se <- coeftest(model, vcov=vcovHC(model,"HC1"))[,2]
    model$vcovHC <- vcovHC(model,"HC1")
    assign(modelname,model,envir = .GlobalEnv)
    coeftest(model, vcov=vcovHC(model,"HC1"))
  } else {
    name <- deparse(substitute(model))
    vcovCL <- cluster.vcov(model, cluster, df_correction = df_correction)
    model$vcovCL <- vcovCL
    modelname <- paste(name,"clustrob",sep=".")
    model$se <- coeftest(model, vcovCL)[,2]
    assign(modelname,model,envir = .GlobalEnv)
    #coeftest(model, vcovCL)
  }
}


#### Regression models ####

model <- glm(select ~ size + procedures + bodies + ownership,
             family = binomial(link='logit'), data = data)
robustse.f(model, ~id, F) # Cluster-robust standard errors


#### .Fatigue Analysis Conjoints ####

model_p1 = glm(select ~ size + procedures + bodies + ownership,
               family = binomial(link='logit'), data[data$person == 1,])

model_p2 = glm(select ~ size + procedures + bodies + ownership,
               family = binomial(link='logit'), data[data$person == 2,])

model_p3 = glm(select ~ size + procedures + bodies + ownership,
               family = binomial(link='logit'), data[data$person == 3,])

model_p4 = glm(select ~ size + procedures + bodies + ownership,
               family = binomial(link='logit'), data[data$person == 4,])

model_p5 = glm(select ~ size + procedures + bodies + ownership,
               family = binomial(link='logit'), data[data$person == 5,])

robustse.f(model_p1, ~id, F) # Cluster-robust standard errors
robustse.f(model_p2, ~id, F)
robustse.f(model_p3, ~id, F)
robustse.f(model_p4, ~id, F)
robustse.f(model_p5, ~id, F)


#### Full conjoint figure prep ####
## Model 1
plotdf <- tibble(estimate = coeftest(model.clustrob, model.clustrob$vcovCL)[,1],
                 SE = coeftest(model.clustrob, model.clustrob$vcovCL)[,2])

plotdf <- plotdf[-1,]
plotdf <- rbind(c(0,0),
                plotdf[1:3,],
                c(0,0),
                plotdf[4:7,],
                c(0,0),
                plotdf[8:11,],
                c(0,0),
                plotdf[12:13,])

plotdf$coef <- var

plotdf$coef <- fct_rev(factor(plotdf$coef, levels = unique(var)))

plotdf$LCI <- plotdf$estimate-1.96*plotdf$SE
plotdf$UCI <- plotdf$estimate+1.96*plotdf$SE

plotdf <- plotdf %>%
  mutate(attribute = if_else(coef %in% c("500 conference participants", 
                                         "1000 conference participants",
                                         "1500 conference participants",
                                         "2000 + conference participants"), "Size",
                             if_else(coef %in% c("Executive group that manages the association and annual meeting",
                                                 "Council and President that manage the association and annual meeting", 
                                                 "Council that manages the association and annual meeting",
                                                 "President that manages the association and annual meeting",
                                                 "Executive group that manages the annual meeting and a Council and president that manage the association"), "Procedures",
                                     if_else(coef %in% c("Elected council that appoints a president", 
                                                         "Elected council and elected president", 
                                                         "Non-elected executive that appoints council and president", 
                                                         "Non-elected executive, elected Council and elected President", 
                                                         "Non-elected executive, elected Council and appointed President"), "Bodies",
                                             if_else(coef %in% c("Not-for-profit ownership structure",
                                                                 "Private-for-profit ownership structure", 
                                                                 "Cooperative ownership structure – membership ownership"), "Ownership", NA_character_)))))

plotdf$attribute <- plotdf$attribute %>%
  factor(levels = c("Size",
                    "Procedures",
                    "Bodies",
                    "Ownership"))

#### .Fatigue Preparation ####

## Model 1

for (i in 1:5) {
  assign(paste0("plotdf_p", i), tibble(estimate = coeftest(get(paste0("model_p", i, ".clustrob")), get(paste0("model_p", i, ".clustrob"))$vcovCL)[,1],
                                       SE = coeftest(get(paste0("model_p", i, ".clustrob")), get(paste0("model_p", i, ".clustrob"))$vcovCL)[,2]))
  
  assign(paste0("plotdf_p", i), get(paste0("plotdf_p", i))[-1,])
  
  assign(paste0("plotdf_p", i), rbind(c(0,0),
                                      get(paste0("plotdf_p", i))[1:3,],
                                      c(0,0),
                                      get(paste0("plotdf_p", i))[4:7,],
                                      c(0,0),
                                      get(paste0("plotdf_p", i))[8:11,],
                                      c(0,0),
                                      get(paste0("plotdf_p", i))[12:13,]))
  
  assign(paste0("plotdf_p", i), get(paste0("plotdf_p", i)) %>%
           mutate(conjoint = paste0("Conjoint ", i),
                  coef = var))
}

plotdf_fatigue <- do.call(rbind, mget(ls(pattern = "plotdf_p")))

plotdf_fatigue$coef <- fct_rev(factor(plotdf_fatigue$coef, levels = unique(var)))

plotdf_fatigue$LCI <- plotdf_fatigue$estimate-1.96*plotdf_fatigue$SE
plotdf_fatigue$UCI <- plotdf_fatigue$estimate+1.96*plotdf_fatigue$SE

plotdf_fatigue <- plotdf_fatigue %>%
  mutate(attribute = if_else(coef %in% c("500 conference participants", 
                                         "1000 conference participants",
                                         "1500 conference participants",
                                         "2000 + conference participants"), "Size",
                             if_else(coef %in% c("Executive group that manages the association and annual meeting",
                                                 "Council and President that manage the association and annual meeting", 
                                                 "Council that manages the association and annual meeting",
                                                 "President that manages the association and annual meeting",
                                                 "Executive group that manages the annual meeting and a Council and president that manage the association"), "Procedures",
                                     if_else(coef %in% c("Elected council that appoints a president", 
                                                         "Elected council and elected president", 
                                                         "Non-elected executive that appoints council and president", 
                                                         "Non-elected executive, elected Council and elected President", 
                                                         "Non-elected executive, elected Council and appointed President"), "Bodies",
                                             if_else(coef %in% c("Not-for-profit ownership structure",
                                                                 "Private-for-profit ownership structure", 
                                                                 "Cooperative ownership structure – membership ownership"), "Ownership", NA_character_)))))

plotdf_fatigue$attribute <- plotdf_fatigue$attribute %>%
  factor(levels = c("Size",
                    "Procedures",
                    "Bodies",
                    "Ownership"))




library(stargazer)
library(tidyverse)
library(broom)
library(texreg)
library(xtable)
library(pals)
library(dummies)
library(ggridges)
library(ggpubr)
library(cowplot)
set.seed(89)


# Plot no. 1: 
p1 = ggplot(data = plotdf[which(plotdf$attribute == "Size"),], aes(x=coef)) +
  #facet_grid(.~attribute, scales = "free_y") +
  facet_grid(rows = vars(attribute), cols = NULL) +
  geom_point(aes(y=estimate), position=position_dodge(width = 0.9), size = 2) +
  geom_linerange(aes(max=UCI, min=LCI), position=position_dodge(width = 0.9)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(x="",y="Logit Coefficient") +
  coord_flip() +
  theme(legend.position = "bottom",
        text = element_text(size=10)) +
  scale_x_discrete(labels = scales::label_wrap(35)) 
p1

# Plot no. 2: 
p2 = ggplot(data = plotdf[which(plotdf$attribute == "Procedures"),], aes(x=coef)) +
  #facet_grid(.~attribute, scales = "free_y") +
  facet_grid(rows = vars(attribute), cols = NULL) +
  geom_point(aes(y=estimate), position=position_dodge(width = 0.9), size = 2) +
  geom_linerange(aes(max=UCI, min=LCI), position=position_dodge(width = 0.9)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(x="",y="Logit Coefficient") +
  coord_flip() +
  theme(legend.position = "bottom",
        text = element_text(size=10)) +
  scale_x_discrete(labels = scales::label_wrap(35)) 
p2

# Plot no. 3: 
p3 = ggplot(data = plotdf[which(plotdf$attribute == "Bodies"),], aes(x=coef)) +
  #facet_grid(.~attribute, scales = "free_y") +
  facet_grid(rows = vars(attribute), cols = NULL) +
  geom_point(aes(y=estimate), position=position_dodge(width = 0.9), size = 2) +
  geom_linerange(aes(max=UCI, min=LCI), position=position_dodge(width = 0.9)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(x="",y="Logit Coefficient") +
  coord_flip() +
  theme(legend.position = "bottom",
        text = element_text(size=10)) +
  scale_x_discrete(labels = scales::label_wrap(35)) 
p3

# Plot no. 4: 
p4 = ggplot(data = plotdf[which(plotdf$attribute == "Ownership"),], aes(x=coef)) +
  #facet_grid(.~attribute, scales = "free_y") +
  facet_grid(rows = vars(attribute), cols = NULL) +
  geom_point(aes(y=estimate), position=position_dodge(width = 0.9), size = 2) +
  geom_linerange(aes(max=UCI, min=LCI), position=position_dodge(width = 0.9)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(x="",y="Logit Coefficient") +
  coord_flip() +
  theme(legend.position = "bottom",
        text = element_text(size=10)) +
  scale_x_discrete(labels = scales::label_wrap(35)) 
p4

# combined graph: 
plot_grid(p1 + theme(axis.title.x = element_blank()),
          p2 + theme(axis.title.x = element_blank()), 
          p3+ theme(axis.title.x = element_blank()),
          p4,
          align='v', vjust=1, scale = 1, ncol = 1)










