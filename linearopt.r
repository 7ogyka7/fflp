#optimal lineup http://patrickclark.info/Lineup_Optimizer.html

library(devtools)
devtools::install_github("FantasyFootballAnalytics/ffanalytics")
library(Rtools)
library(tidyverse)
library(ffanalytics)
library(lpSolve)
library(lpSolveAPI)
library(XML)
library(tidyr)
library(dplyr)


trim <- function (x) gsub("^\\s+|\\s+$", "", x) 

Season <- 2019
Week <- 9

hist_scrape <- scrape_data(src = c('FantasySharks','ESPN','FantasyPros','FFToday', 'NumberFire', 'Yahoo'),
                           pos = c("QB","RB","WR","TE","DST"),
                           season = Season, week = Week)#removed espn from second in list because projections not posted on 10/9

Projections <- projections_table(hist_scrape) %>% add_player_info() %>% filter(avg_type=='average')

#Modify Defense name to prepare for join 
Projections$Name <- paste0(Projections$first_name,' ',Projections$last_name,sep='')

#Clean up suffixes to prepare for join
Projections$Name <- gsub(" Jr.","",Projections$Name)
Projections$Name <- gsub(" Sr.","",Projections$Name)
Projections$Name <- gsub(" II","",Projections$Name)
Projections$Name <- trim(Projections$Name)

#salaries
#salaries = "http://rotoguru1.com/cgi-bin/fyday.pl?game=fd"
#salaries.table = readHTMLTable(salaries, header=T, which=1,stringsAsFactors=F)
#as.data.frame.table(salaries.table)
#salaries.table <- salaries.table %>% drop_na(V1,V2,V3,V4,V5)
#salaries.table <- salaries.table[!grepl("/", salaries.table$V5),]
#salaries.table <- salaries.table[!grepl("Team", salaries.table$V2),]
#colnames(salaries.table) <- c("Name", "Team","Opponent","Points","Salary")

salaries = read.csv(file="wk_9.csv",sep=";")
salaries$Nickname <- gsub(" Jr.","",salaries$Nickname)
salaries$Nickname <- gsub(" Sr.","",salaries$Nickname)
salaries$Nickname <- gsub(" II","",salaries$Nickname)
salaries$Nickname <- trim(salaries$Nickname)
salaries$Name <-salaries$Nickname

#Join on the text column 'Name'
MyPool <- left_join(salaries,Projections,by="Name")

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
PlayerPool$DST_Check <- ifelse(PlayerPool$position == "DST",1,0)
PlayerPool$One <- 1

PlayerPool <- PlayerPool[order(PlayerPool$QB_Check),]
PlayerPool <- PlayerPool[order(PlayerPool$RB_Check),]
PlayerPool <- PlayerPool[order(PlayerPool$WR_Check),]
PlayerPool <- PlayerPool[order(PlayerPool$TE_Check),]
PlayerPool <- PlayerPool[order(PlayerPool$DST_Check),]

#solver

Num_Players <- length(PlayerPool$One)

lp_model= make.lp(0, Num_Players)
set.objfn(lp_model, PlayerPool$points)#change to ceiling or mean for risk tolerance

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

crossprod.replacena <- function(x, y, val=0) {
  crossprod(replace(x, is.na(x), val), 
            replace(y, is.na(y), val)
  )
}

crossprod.replacena(PlayerPool$points, get.variables(lp_model))#points
crossprod.replacena(PlayerPool$ceiling, get.variables(lp_model))#ceiling
crossprod.replacena(PlayerPool$floor, get.variables(lp_model))#floor
crossprod.replacena(PlayerPool$sd_pts, get.variables(lp_model))#sd

get.variables(lp_model)

optimal_lineup <- subset(data.frame(PlayerPool$Name, PlayerPool$position, PlayerPool$Salary), get.variables(lp_model) == 1)
optimal_lineup
