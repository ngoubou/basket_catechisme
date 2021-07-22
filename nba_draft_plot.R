
library(tidyverse)


nba <- read_csv("./Data_Scientist_Path/Datasets/nba_dataset/draft_history.csv")

View(nba)

glimpse(nba)

nba$slugTeam <- as_factor(nba$slugTeam)

# filtering since Kobe last title
nba %>%
  filter(yearDraft >= 2010, numberRound == 1, numberRoundPick <= 14) %>%
  count(slugTeam, sort = TRUE) %>%
  mutate(slugTeam = fct_reorder(slugTeam, n)) %>%
  head(n = 10) %>%
  ggplot(aes(x = slugTeam, y = n, fill = slugTeam)) +
  geom_bar(stat = "identity", width = .4, show.legend = FALSE) +
  labs(x = "Team", y = "picks", title = "Total lottery picks par équipe 2010-2020") +
  ggthemes::scale_fill_tableau() +
  ggthemes::theme_economist()


  head() %>%
  ggplot(aes(slugTeam)) +
  geom_bar()
  summary()


  
  police_clean %>%
    count(raceethnicity, sort = T) %>%
    mutate(race = fct_reorder(raceethnicity, n)) %>%
    ggplot(aes(x = race, y = n, fill = race)) +
    geom_bar(stat = "identity", width = .4, show.legend = FALSE)
  
  
  

table(nba$numberRound)
summary(nba)





nba %>%
  filter(yearDraft >= 2010, numberRound == 1, numberRoundPick <= 14) %>%
  group_by(slugTeam) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  View()
