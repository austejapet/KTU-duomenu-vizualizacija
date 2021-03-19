library(tidyverse)
library(ggplot2)

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

avg<- dataFile[order(-dataFile$avgWage), 1:10]  
print(avg)

a<-filter(dataFile, dataFile$name=="NVENT THERMAL NETHERLANDS B.V. ATSTOVYBË")
b<-filter(dataFile, dataFile$name=="LEGRAND SNC ATSTOVYBË")
c<-filter(dataFile, dataFile$name=="ALCON PHARMACEUTICALS LTD. ATSTOVYBË")
d<-filter(dataFile, dataFile$name=="LIETUVOS RESPUBLIKOS TRANSPORTO PRIEMONIØ DRAUDIKØ BIURAS")
e<-filter(dataFile, dataFile$name=="IVECO S.P.A. ATSTOVYBË")


data<-rbind(a, b, c, d, e)


ggplot(data, aes(x=month, y=avgWage, group = name, color = name)) +
  geom_line() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
          
#3uþduotis

A<-sum(a$numInsured)
B<-sum(b$numInsured)
C<-sum(c$numInsured)
D<-sum(d$numInsured)
E<-sum(e$numInsured)

insured<-rbind(A, B, C, D, E)

names<-c("NVENT THERMAL NETHERLANDS B.V. ATSTOVYBË", "LEGRAND SNC ATSTOVYBË", 
"ALCON PHARMACEUTICALS LTD. ATSTOVYBË", "LIETUVOS RESPUBLIKOS TRANSPORTO 
PRIEMONIØ DRAUDIKØ BIURAS", "IVECO S.P.A. ATSTOVYBË")

data2<-data.frame(insured, names)

ggplot(data2, aes(x=reorder(names, -insured), y=insured, group = names, 
                  fill=names )) +
  geom_bar(stat="identity") +
  xlab("name") +
  ylab("apdraustieji") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

