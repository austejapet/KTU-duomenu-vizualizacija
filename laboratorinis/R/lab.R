library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)

dataFile<- read_csv("laboratorinis/data/lab_sodra.csv")

summary(dataFile)

dataFile<- filter(dataFile, dataFile$ecoActCode==949900)

# 1uþduotis

hist(dataFile$avgWage, main = NULL, 
     xlab = "avgWage",
     ylab = "count",
     col = "gray28",
     breaks = 100,
     border = "gray28", 
    xlim = c(0, 5000))

#2uþduotis

filtered <- dataFile %>%
  group_by(code) %>%
  summarise(suma = sum(avgWage)) %>% 
  arrange(desc(suma)) %>% head(5)

merged <- merge(filtered, dataFile, by = "code")
ggplot(merged, aes(x=month, y=avgWage, group = name, color = name)) +
  geom_line() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
          
#3uþduotis


group_by(merged, name) %>%
summarise(maxNumInsured = max(numInsured)) %>%
  ggplot(aes(x=reorder(name, -maxNumInsured), y=maxNumInsured, group = name, 
                    fill=name )) +
  geom_bar(stat="identity") +
  xlab("name") +
  ylab("apdraustieji") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))








