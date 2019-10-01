#optimal lineup modified from http://patrickclark.info/Lineup_Optimizer.html

library(devtools)
library(Rtools)
library(tidyverse)
library(ffanalytics)
library(lpSolveAPI)
library(lpSolve)

trim <- function (x) gsub("^\\s+|\\s+$", "", x) 

Season <- 2019
Week <- 4

hist_scrape <- scrape_data(src = c('FantasySharks','ESPN','FantasyPros','FFToday', 'NumberFire', 'Yahoo'),
                           pos = c("QB","RB","WR","TE","DST"),
                           season = Season, week = Week)

Projections <- projections_table(hist_scrape) %>% add_player_info() %>% filter(avg_type=='average')

#Modify Defense name to prepare for join 
Projections$Name <- ifelse(Projections$pos=='DST',Projections$last_name,paste0(Projections$first_name,' ',Projections$last_name,sep=''))

#Clean up suffixes to prepare for join
Projections$Name <- gsub(" Jr.","",Projections$Name)
Projections$Name <- gsub(" Sr.","",Projections$Name)
Projections$Name <- gsub(" II","",Projections$Name)
Projections$Name <- trim(Projections$Name)

#salaries

DKSalaries_Import <- read.csv("C:/Users/Patrick/Downloads/DKSalaries.csv",skip=8,header=FALSE)
DKSalaries <- DKSalaries_Import[11:18]
colnames(DKSalaries) <- c("Position","Name_ID","Name","ID","POS2","Salary","GameInfo","TeamAbbrev")
rm(DKSalaries_Import)
DKSalaries$Name <- gsub(" Jr.","",DKSalaries$Name)
DKSalaries$Name <- gsub(" Sr.","",DKSalaries$Name)
DKSalaries$Name <- gsub(" II","",DKSalaries$Name)
DKSalaries$Name <- trim(DKSalaries$Name)

#Join on the text column 'Name'
MyPool <- left_join(DKSalaries,select(Projections,Name,points),by="Name")

#The csv from DraftKings typically contains many more options than we have projections, but we only want to select from the players who have a projected score.
MyPool <- MyPool[complete.cases(MyPool),]

#Below, I manage one of the issues of joining on text - duplicate "keys". My pragmatic solution is to keep the player with the higher salary. A safe assumption that All-Pro Saints WR Michael Thomas is a more likely candidate for our lineup than Free-Agent WR Michael Thomas... There are more robust ways to solve this problem, but this simple solution gets the job done for this purpose.
PlayerPool <- MyPool
PlayerPool <- PlayerPool[order(-(PlayerPool$Salary),-(PlayerPool$points)), ] #sort by id and reverse of abs(value)
PlayerPool <- PlayerPool[ !duplicated(PlayerPool$Name), ] #take the first row within each id
PlayerPool <- PlayerPool[order(-(PlayerPool$Salary)), ]

#Creating some positional identifiers in the pool of players to simplify linear constraints
PlayerPool$QB_Check <- ifelse(PlayerPool$Position == "QB",1,0)
PlayerPool$RB_Check <- ifelse(PlayerPool$Position == "RB",1,0)
PlayerPool$WR_Check <- ifelse(PlayerPool$Position == "WR",1,0)
PlayerPool$TE_Check <- ifelse(PlayerPool$Position == "TE",1,0)
PlayerPool$DST_Check <- ifelse(PlayerPool$Position == "DST",1,0)
PlayerPool$One <- 1

PlayerPool <- PlayerPool[order(PlayerPool$QB_Check),]
PlayerPool <- PlayerPool[order(PlayerPool$RB_Check),]
PlayerPool <- PlayerPool[order(PlayerPool$WR_Check),]
PlayerPool <- PlayerPool[order(PlayerPool$TE_Check),]
PlayerPool <- PlayerPool[order(PlayerPool$DST_Check),]

#solver

Num_Players <- length(PlayerPool$One)

lp_model= make.lp(0, Num_Players)
set.objfn(lp_model, PlayerPool$points)

lp.control(lp_model, sense= "max")
set.type(lp_model, 1:Num_Players, "binary")

add.constraint(lp_model, PlayerPool$Salary, "<=",55000)

add.constraint(lp_model, PlayerPool$QB_Check, "=",1)

add.constraint(lp_model, PlayerPool$RB_Check, "<=",3)
add.constraint(lp_model, PlayerPool$RB_Check, ">=",2)

add.constraint(lp_model, PlayerPool$WR_Check, "<=",4)
add.constraint(lp_model, PlayerPool$WR_Check, ">=",3)

add.constraint(lp_model, PlayerPool$TE_Check, "<=",2)
add.constraint(lp_model, PlayerPool$TE_Check, ">=",1)

add.constraint(lp_model, PlayerPool$DST_Check, "=",1)

add.constraint(lp_model, PlayerPool$One, "=",9)

solve(lp_model)
Projected_Score <- crossprod(PlayerPool$points,get.variables(lp_model))

get.variables(lp_model)

optimal_lineup <- subset(data.frame(PlayerPool$Name_ID, PlayerPool$Position), get.variables(lp_model) == 1)
