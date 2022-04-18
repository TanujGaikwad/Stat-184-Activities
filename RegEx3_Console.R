library(tidyverse)

Tennis_Data <- read.csv(file = "Data_RegEx3.csv")

NewTable <-
  Tennis_Data %>%
  mutate(year = str_extract(tourney_id, "^\\d+")) %>%
  mutate(score_edit = score)

  NewTable$score_edit <- gsub("\\(\\d+)", replacement = "", NewTable$score_edit)
  NewTable$score_edit <- gsub("RET", replacement = "", NewTable$score_edit)


NewTable <-
  NewTable %>% 
  separate(score_edit, c("Set.1", "Set.2", "Set.3", "Set.4", "Set.5"), sep = " ")

NewTable = 
  NewTable %>%
  mutate(setsPlayed = rowSums(!is.na(NewTable[,8:12])))

head(NewTable)

NewTable <-
  NewTable %>%
  select(tourney_id, tourney_name, winner_ioc, winner_age, score, minutes, year, setsPlayed)

NewTable$tourney_name <- gsub("Us Open", "US Open", NewTable$tourney_name)

ggplot(NewTable) +
  aes(x = minutes, group = setsPlayed, fill = as.factor(setsPlayed), color = as.factor(setsPlayed)) +
  geom_density() + 
  labs(title = "Density Plots of Minutes Played Per Number of Sets Played",
       x = "Minutes Played",
       y = "Density") +
  theme(legend.position = "top")


ggplot(NewTable, aes(x = tourney_name, y = winner_age, color = tourney_name)) + 
  geom_boxplot() +
  labs(title = "Boxplot Age of Winner in Grand Slam Events",
       x = "Tournament Name",
       y = "Age of Winner")

NewTable2 <-
  NewTable %>%
  filter(winner_ioc %in% c("ARG","AUS","ESP","FRA","GER","ITA","USA"))
  
ggplot(data = NewTable2) +
  aes(x = , y = winner_ioc ) +
  geom_point(stat = "count", size = 2)+ 
  coord_flip() + 
  facet_grid(tourney_name ~ .) +
  theme_bw()+
  labs(title = "Top 5 Olympic Memberships per Tournament", x = "Tournaments Won", y = "Winners Olympic Membership") + 
  xlim(100,290)


ioc_countries2 <-c("ESP","FRA","USA")

NewTable4 <- NewTable %>%
  filter(winner_ioc %in% ioc_countries2)


NewTable4_Vals <- NewTable4 %>%
  group_by(year,winner_ioc) %>%
  summarise(count = n())

YearCounts <- NewTable %>%
  group_by(year, winner_ioc) %>%
  summarise(count = n()) %>%
  group_by(year) %>%
  summarise(count = max(count)) 

WinnerBasedOnIOC_Year <- NewTable %>%
  group_by(year,winner_ioc) %>%
  summarise(count = n())


Toppers <- WinnerBasedOnIOC_Year %>%
  inner_join(YearCounts, by = c("year","count"))

NewTable4 <- anti_join(NewTable4_Vals, Toppers , by = c("year","winner_ioc","count"))

NewTable4_Vals2 <- NewTable4 %>%
  group_by(year) %>%
  summarise(count = max(count))

RunnerUpsBasedOnIOC_Year <- NewTable4 %>%
  inner_join(NewTable4_Vals2, by = c("year","count"))

RunnerUps <- Toppers %>%
  full_join(RunnerUpsBasedOnIOC_Year, by = c("year", "winner_ioc","count"))

ggplot(data = RunnerUps) +
  aes(x = year, group = winner_ioc, color = winner_ioc) +
  geom_point(stat = "count", size = 2) + 
  theme_bw() + 
  labs(title = "Top 2 Olympic Memberships per Year",
       x = "Year",
       y = "Tournaments Won") +
  scale_color_discrete("Olympic Membership")