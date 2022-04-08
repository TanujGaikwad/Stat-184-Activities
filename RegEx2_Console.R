#Load Packages
library(rvest)
library(tidyverse)

#Read in Data
data <- read.delim("Data_RegEx2.txt", fileEncoding = "UTF-8", header = F, quote = "")

#View Data
head(data)


#Make new table with all filters and modifications
NewTable <- 
  as.data.frame(matrix(data$V1, ncol = 16, byrow = T)) %>%
  rename("Position" = "V1", "Player" = "V2", "FinalScore" = "V3", "THRU" = "V4", "TODAY" = "V5",
         "SGPutt" = "V6", "SCARG" = "V7", "SGAPP" = "V8", "SGOTT" = "V9", "SGT2G" = "V10", "SGTOT" = "V11",
         "DrDist" = "V12", "DrAcc" = "V13", "GIR" = "V14", "FairProx" = "V15", "Scrambling" = "V16") %>% #Rename Col Names
  extract(col = Player, into = c("Last", "First") , regex = "([\\w\\s]+)(\\s\\w.+)") %>% #Separate Last & First Names of Players
  unite("Player", c("First", "Last"), sep = " ") %>% #Unite them again in "First Last" order
  filter(Position != "WD") %>% #Filter out withdrawn players
  mutate(THRU = replace(THRU, THRU == "—", NA)) %>% #Make all hyphens into NA values
  mutate(TODAY = replace(TODAY, TODAY == "—", NA)) #Make all hyphens into NA values

#View Head & Tail of table for final checks before graphing
head(NewTable)

tail(NewTable)

#Change formatting for variables that need to be charted
NewTable$SGTOT = as.numeric(NewTable$SGTOT)
NewTable$GIR = as.numeric(gsub("[\\%]", "", NewTable$GIR))

#Make new column based on if Cut or Not
NewTable <- 
  mutate(NewTable, Cut = ifelse(Position == "CUT", "Missed Cut", "Made Cut"))

#First Plot
ggplot(NewTable, aes(x = Cut, y = SGTOT)) + 
  geom_boxplot(aes(color = Cut)) +
  labs(title = "Boxplot Shots Gained Total",
       y = "Shots Gained Total",
       x  = "CUT")

#INTERPRETIVE STATEMENT NEEDED

#Second Plot
ggplot(NewTable, aes(x = Cut, y = GIR)) + 
  geom_boxplot(aes(color = Cut)) +
  labs(title = "Boxplot Greens Hit in Regulation",
       y = "Greens in Regulation",
       x  = "CUT")

#INTERPRETIVE STATEMENT NEEDED