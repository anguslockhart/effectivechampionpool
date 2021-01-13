library(tidyverse)
library(wesanderson)
match_results <- read_csv("~/Dropbox/Coding Projects/eSport Analysis/2020_LoL_esports_match_data_from_OraclesElixir_20200817.csv") %>%
  filter(position!="team")

na_results <- filter(match_results, league=="LCS")

champion_pools <- match_results %>%
  group_by(team) %>%
  summarize(normal_pool=length(unique(champion)))

picks <- na_results$champion

ECP <- function(picks) {
  unique_champions <- unique(picks)
  games <- length(picks)/5
  results <- data.frame(
    champion=rep(NA, length(unique_champions)),
    percentage=rep(NA, length(unique_champions))
  )
  for (i in 1:length(unique_champions)) {
    pick <- unique_champions[i]
    times_picked <- length(picks[picks==pick])
    pick_percentage <- times_picked/games
    results$champion[i] <- unique_champions[i]
    results$percentage[i] <- pick_percentage
  }
  effective_champions <- 1/sum((results$percentage/5)^2)
  return(effective_champions)
}

ECP_player <- function(picks) {
  unique_champions <- unique(picks)
  games <- length(picks)
  results <- data.frame(
    champion=rep(NA, length(unique_champions)),
    percentage=rep(NA, length(unique_champions))
  )
  for (i in 1:length(unique_champions)) {
    pick <- unique_champions[i]
    times_picked <- length(picks[picks==pick])
    pick_percentage <- times_picked/games
    results$champion[i] <- unique_champions[i]
    results$percentage[i] <- pick_percentage
  }
  effective_champions <- 1/sum((results$percentage)^2)
  return(effective_champions)
}

results <- data.frame(
  team=rep(NA, 10),
  effective_pool=rep(NA, 10)
)
teams <- unique(na_results$team)
for (i in 1:length(teams)) {
  team_pool <- filter(na_results, team==teams[i])%>%
    select(champion) %>%
    pull(1) %>%
    ECP
  results$team[i] <- teams[i]
  results$effective_pool[i] <- team_pool
}

end <- left_join(results,champion_pools)

colour.list <- as.character(c("#7C7C7C","#0E4DAD","#63D3F1","#000000",
                              "#34CB2D","#A71B1B","#ECE91A","#2DA7B1","#BECB1D","#2B94EB"))
team <- as.character(c("Team SoloMid",
                       "Team Liquid",
                       "Cloud9",
                       "Evil Geniuses",
                       "FlyQuest",
                       "100 Thieves", 
                       "Dignitas", 
                       "Immortals",
                       "Golden Guardians",
                       "Counter Logic Gaming"))
colours <- data.frame(team, colour.list)
end <- left_join(results,champion_pools)%>%
  left_join(colours)

ggplot(end, aes(y=effective_pool, x=normal_pool, colour=team))+
  geom_point()+
  xlim(45, 80)+
  ylim(30, 45)+
  scale_colour_manual(values=setNames(colour.list, team))
  

results <- data.frame(
  team=rep(NA, 234),
  effective_pool=rep(NA, 234)
)
teams <- unique(match_results$team)
for (i in 1:length(teams)) {
  team_pool <- filter(match_results, team==teams[i])%>%
    select(champion) %>%
    pull(1) %>%
    ECP
  results$team[i] <- teams[i]
  results$effective_pool[i] <- team_pool
}

end <- left_join(results,champion_pools)%>%
  left_join(colours) %>%
  mutate(diff=normal_pool-effective_pool)

ggplot(end, aes(y=effective_pool, x=normal_pool))+
  geom_point()


champion_pools <- na_results %>%
  group_by(player) %>%
  summarize(normal_pool=length(unique(champion)))

na_results <- filter(match_results, league=="LCS")
picks <- na_results$champion
results <- data.frame(
  player=rep(NA, 73),
  effective_pool=rep(NA, 73)
)
players <- unique(na_results$player)
for (i in 1:length(players)) {
  player_pool <- filter(na_results, player==players[i])%>%
    select(champion) %>%
    pull(1) %>%
    ECP_player
  results$player[i] <- players[i]
  results$effective_pool[i] <- player_pool
}
end <- left_join(results,champion_pools)%>%
  mutate(diff=normal_pool-effective_pool)

