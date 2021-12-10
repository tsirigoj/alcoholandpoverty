#Code for boxplots and table data#
#Coded by Lingxuan Kong#
rm(list=ls())
library("NHANES")
DATASET <- NHANES
Variable_included <- c("Poverty","Gender","Age","Race1","AlcoholDay")
#DATA_selected <- subset(DATASET,select = Variable_included)
#DATA_selected <- na.omit(DATA_selected[which(DATA_selected$Age>=20&DATA_selected$Age<=69),])
DATA_selected <- subset(DATASET,select = Variable_included)
miss_num = apply(DATA_selected, 1, function(x) sum(is.na(x)))
DATA_selected$Incomplete = ifelse(miss_num>0, 1, 0)
DATA_selected$Incomplete = as.factor(DATA_selected$Incomplete)
index_complete <- which(DATA_selected$Incomplete == 0&DATA_selected$Age>=20&DATA_selected$Age<=69)
Variable_included2 <- c("Poverty","Gender","Age","Race1","Education","AlcoholDay")
DATA_selected2 <- subset(DATASET,select = Variable_included2)
DATA_included <- DATA_selected2[index_complete,]
DATA_notincluded <- DATA_selected2[-index_complete,]
library("GGally")
library("visreg")
GGally::ggpairs(DATA_included)
library("ggplot2")
library("ggsci")
##GEN X RACE##
#GRAPH#Gender X Race# Poverty
a <- ggplot(aes(x = Gender, y = Poverty, fill = Race1),data = DATA_included) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "Poverty")+
  scale_x_discrete(name = "Gender") +
  ggtitle("Boxplot of Poverty") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face =  "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_lancet()
ggsave("1.jpeg",plot = a,path = "C:/Users/Administrator/Desktop")
#GRAPH#Gender X Race# Alcohol
b <- ggplot(aes(x = Gender, y = AlcoholDay, fill = Race1),data = DATA_included) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "AlcoholDay")+
  scale_x_discrete(name = "Gender") +
  ggtitle("Boxplot of AlcoholDay") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face =  "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_lancet()
ggsave("2.jpeg",plot = b,path = "C:/Users/Administrator/Desktop")
#GRAPH#Gender X Race# Age
c <- ggplot(aes(x = Gender, y = Age, fill = Race1),data = DATA_included) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "Age")+
  scale_x_discrete(name = "Gender") +
  ggtitle("Boxplot of Age") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face =  "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_lancet()
ggsave("3.jpeg",plot = c,path = "C:/Users/Administrator/Desktop")
##GEN X EDU##
#GRAPH#Gender X EDU# Poverty
d <- ggplot(aes(x = Gender, y = Poverty, fill = Education),data = DATA_included) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "Poverty")+
  scale_x_discrete(name = "Gender") +
  ggtitle("Boxplot of Poverty") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face =  "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_lancet()
ggsave("4.jpeg",plot = d,path = "C:/Users/Administrator/Desktop")
#GRAPH#Gender X EDU# Alcohol
e <- ggplot(aes(x = Gender, y = AlcoholDay, fill = Education),data = DATA_included) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "AlcoholDay")+
  scale_x_discrete(name = "Gender") +
  ggtitle("Boxplot of AlcoholDay") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face =  "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_lancet()
ggsave("5.jpeg",plot = e,path = "C:/Users/Administrator/Desktop")
#GRAPH#Gender X EDU# Age
f <- ggplot(aes(x = Gender, y = Age, fill = Education),data = DATA_included) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "Age")+
  scale_x_discrete(name = "Gender") +
  ggtitle("Boxplot of Age") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face =  "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_lancet()
ggsave("6.jpeg",plot = f,path = "C:/Users/Administrator/Desktop")
##RACE X EDU##
#GRAPH#RACE X EDU# Poverty
g <- ggplot(aes(x = Race1, y = Poverty, fill = Education),data = DATA_included) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "Poverty")+
  scale_x_discrete(name = "Race") +
  ggtitle("Boxplot of Poverty") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face =  "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_lancet()
ggsave("7.jpeg",plot = g,path = "C:/Users/Administrator/Desktop")
#GRAPH#RACE X EDU# Alcohol
h <- ggplot(aes(x = Race1, y = AlcoholDay, fill = Education),data = DATA_included) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "AlcoholDay")+
  scale_x_discrete(name = "Race") +
  ggtitle("Boxplot of AlcoholDay") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face =  "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_lancet()
ggsave("8.jpeg",plot = h,path = "C:/Users/Administrator/Desktop")
#GRAPH#RACE X EDU# Age
i <- ggplot(aes(x = Race1, y = Age, fill = Education),data = DATA_included) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "Age")+
  scale_x_discrete(name = "Race") +
  ggtitle("Boxplot of Age") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face =  "bold", hjust = 0.5),
        text = element_text(size = 12),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_lancet()
ggsave("9.jpeg",plot = i,path = "C:/Users/Administrator/Desktop")
##BASELINE COMPARISON##
t.test(DATA_included$Poverty,DATA_notincluded$Poverty)
t.test(DATA_included$Age,DATA_notincluded$Age)
t.test(DATA_included$AlcoholDay,DATA_notincluded$AlcoholDay)
table(DATA_included$Gender)
table(DATA_notincluded$Gender)
table_Gender <- matrix(c(1911,3109,2214,2766),nrow = 2,ncol = 2)
chisq.test(table_Gender)
table(DATA_included$Education)
table(DATA_notincluded$Education)
table_Education <- matrix(c(127,324,420,468,793,724,1355,912,1425,673),nrow = 2,ncol = 5)
chisq.test(table_Education)
table(DATA_included$Race1)
table(DATA_notincluded$Race1)
table_Race1<- matrix(c(430,767,224,386,318,697,2890,3482,263,543),nrow = 2,ncol = 2)
chisq.test(table_Race1)