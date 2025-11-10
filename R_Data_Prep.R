library(tidyverse)
library(here)
library(tidyr)
data_dir <- here::here(".")

read_tbl <- function(name) {
  readr::read_csv(file.path(data_dir, paste0(name, ".csv")), show_col_types = FALSE)
}

#Read CSV files
championTbl <- read_tbl("ChampionTbl")
itemTbl<- read_tbl("ItemTbl")
matchStatsTbl <- read_tbl("MatchStatsTbl")
matchTbl <- read_tbl("MatchTbl")
RankTbl <- read_tbl("RankTbl")
SummonerMatchTbl <- read_tbl("SummonerMatchTbl")
TeamMatchTbl <- read_tbl("TeamMatchTbl")


# Create Tables

## MSI Table -TY
MSI_Tbl <- full_join(matchStatsTbl, itemTbl, 
                     by = join_by(item1 == ItemID),
                     suffix = c("", "_1"))

MSI_Tbl <- full_join(MSI_Tbl, itemTbl, 
                     by = join_by(item2 == ItemID), 
                     suffix = c("", "_2"))

MSI_Tbl <- full_join(MSI_Tbl, itemTbl, 
                     by = join_by(item3 == ItemID), 
                     suffix = c("", "_3"))

MSI_Tbl <- full_join(MSI_Tbl, itemTbl, 
                     by = join_by(item4 == ItemID), 
                     suffix = c("", "_4"))


MSI_Tbl  <- full_join(MSI_Tbl, itemTbl, 
                      by = join_by(item5 == ItemID), 
                      suffix = c("", "_5"))


MSI_Tbl <- full_join(MSI_Tbl, itemTbl, 
                     by = join_by(item6 == ItemID), 
                     suffix = c("", "_6"))

MSI_Tbl


#Match_rank_tbl
match_rank_tbl <- full_join(matchTbl,RankTbl,join_by(RankFk == RankId))

TMR_Tble <- full_join(TeamMatchTbl, match_rank_tbl, by = join_by(MatchFk == MatchId))

SMR_Tbl <- full_join(SummonerMatchTbl, championTbl, join_by(ChampionFk == ChampionId))

SMR_Tbl <- full_join(SMR_Tbl, match_rank_tbl, by = join_by(MatchFk == MatchId))

TMR_Tble <- TMR_Tble %>% 
  filter(QueueType != "CHERRY") %>% 
  mutate(Winning_Team = ifelse(RedKills != 0 , "Red", "Blue") )


#Reshaping table to only have information based on team  
Reshape_TMR_tbl <- TMR_Tble %>% 
  pivot_longer(
    cols = c(BlueBaronKills,RedBaronKills,BlueRiftHeraldKills,RedRiftHeraldKills,
             BlueDragonKills,RedDragonKills,RedTowerKills,BlueTowerKills,BlueKills,RedKills,RedWin,BlueWin),
    names_to = c("Side",".value"),     #"side" which side the column belongs to  (red or blue) and ".value" the type of variable
    names_pattern = "(Red|Blue)(.*)")  #(.*) means to fill after Red or Blue
 
Team_Winner_Tbl <- Reshape_TMR_tbl %>% 
  filter(Win == 1) %>% 
  select(TeamID,MatchFk,Side,GameDuration,QueueType,RankName,Kills,BaronKills,RiftHeraldKills,DragonKills,TowerKills)
    
