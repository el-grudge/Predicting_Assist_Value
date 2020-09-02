table(player_matches_stats[player_matches_stats$year==2017,'position'])

matchIds <- read.csv('matchIds.csv', stringsAsFactors = FALSE)
allTeamStats <- read.csv('allTeamStats.csv', stringsAsFactors = FALSE)
matchShots <- read.csv('matchShots.csv', stringsAsFactors = FALSE)

dim(matchIds)
dim(matchIds[is.na(matchIds),])
dim(matchIds[!is.na(matchIds),]) # weirdly increases the number of NA records!!!

matchIds[matchIds$match_id==12331,'league_name'] <- 'La liga'
matchIds[matchIds$match_id==12331,'year'] <- '2019'

matchIds[matchIds$match_id==13351,'league_name'] <- 'Serie A'
matchIds[matchIds$match_id==13351,'year'] <- '2019'

matchIds[matchIds$match_id==13354,'league_name'] <- 'Serie A'
matchIds[matchIds$match_id==13354,'year'] <- '2019'

View(matchIds[matchIds$league_name=='Bundesliga' & matchIds$year==2018,])
bundesliga2018 <- matchIds[matchIds$league_name=='Bundesliga' & matchIds$year==2018,]

# filling matchday column
bundesliga2018 <- bundesliga2018[order(bundesliga2018$date),]
bundesliga2018$matchday <- 0
for (i in bundesliga2018$h_team){
  n_matchdays <- nrow(bundesliga2018[bundesliga2018$h_team==i | bundesliga2018$a_team==i,])
  bundesliga2018[bundesliga2018$h_team==i | bundesliga2018$a_team==i,]$matchday <- seq(1:n_matchdays)
}

# matchStats
matchStats <- data.frame()
for (i in bundesliga2018[,'match_id']){
  matchStats <- rbind(matchStats, get_match_stats(i))
}

sapply(data.frame(matchStats, stringsAsFactors=FALSE),is.factor)

matchStats[,c('goals','own_goals','shots','xG','key_passes','assists','xA','xGChain','xGBuildup')]
sampleShots <- as.data.frame(matchStats[,c('goals','own_goals','shots','xG','key_passes','assists','xA','xGChain','xGBuildup')])
sampleShots <- as.data.frame(matchStats[,c('shots', 'xG', 'key_passes', 'assists', 'xA', 'xGChain', 'xGBuildup')])
sampleShots <- as.data.frame(matchStats[,c('player', 'shots', 'xG', 'key_passes', 'assists', 'xA', 'xGChain', 'xGBuildup')])
sampleShots <- as.data.frame(matchStats[,c('goals', 'shots', 'xG', 'key_passes', 'assists', 'xA', 'xGChain', 'xGBuildup')])
View(sampleShots)

sampleShots$player <- as.character(sampleShots$player)
#sampleShots$team_id <- as.numeric(as.character(sampleShots$team_id))
sampleShots$goals <- as.numeric(as.character(sampleShots$goals))
#sampleShots$own_goals <- as.numeric(as.character(sampleShots$own_goals))
sampleShots$shots <- as.numeric(as.character(sampleShots$shots))
sampleShots$xG <- as.numeric(as.character(sampleShots$xG))
sampleShots$key_passes <- as.numeric(as.character(sampleShots$key_passes))
sampleShots$assists <- as.numeric(as.character(sampleShots$assists))
sampleShots$xA <- as.numeric(as.character(sampleShots$xA))
sampleShots$xGChain <- as.numeric(as.character(sampleShots$xGChain))
sampleShots$xGBuildup <- as.numeric(as.character(sampleShots$xGBuildup))

#sampleShots <- sampleShots[,-1]
sapply(sampleShots,class)



sampleShots_T <- t(sampleShots)
colnames(sampleShots_T) <- sampleShots_T[1,]
sampleShots_T <- as.data.frame(sampleShots_T[-1,])

model_sampleShots <- hc(sampleShots)
graphviz.plot(model_sampleShots)

model_sampleShots <- hc(sampleShots_T)
graphviz.plot(model_sampleShots)


dplyr::filter(sampleShots_T, function(x) sd(x) != 0)

sampleShots_T <- as.matrix(sampleShots_T) 

sampleShots_T[,sapply(sampleShots_T,function(x) var(x)!=0)]
class(sampleShots_T[,"Manuel Neuer"])

sampleShots_T <- apply(sampleShots_T, 2, as.numeric)
all(duplicated(sampleShots_T[,"Manuel Neuer"])[-1L])

sampleShots_T

sampleShots_T[,1:5][,!apply(sampleShots_T[,1:5],2,function(x) all(x==0))]
sampleShots_T <- sampleShots_T[,!apply(sampleShots_T,2,function(x) all(x==0))] 


library(dplyr)
matchStatsStats <- matchStats[,c('player','team_id','goals','own_goals','shots','xG','key_passes','assists','xA','xGChain','xGBuildup')]
View(matchStatsStats)
sapply(matchStatsStats, is.factor)

#matchStatsStats <- lapply(matchStatsStats, function(x) as.numeric(as.character(x)))

matchStatsStats$player <- as.character(matchStatsStats$player)
matchStatsStats$team_id <- as.numeric(as.character(matchStatsStats$team_id))
matchStatsStats$goals <- as.numeric(as.character(matchStatsStats$goals))
matchStatsStats$own_goals <- as.numeric(as.character(matchStatsStats$own_goals))
matchStatsStats$shots <- as.numeric(as.character(matchStatsStats$shots))
matchStatsStats$xG <- as.numeric(as.character(matchStatsStats$xG))
matchStatsStats$key_passes <- as.numeric(as.character(matchStatsStats$key_passes))
matchStatsStats$assists <- as.numeric(as.character(matchStatsStats$assists))
matchStatsStats$xA <- as.numeric(as.character(matchStatsStats$xA))
matchStatsStats$xGChain <- as.numeric(as.character(matchStatsStats$xGChain))
matchStatsStats$xGBuildup <- as.numeric(as.character(matchStatsStats$xGBuildup))

# team_id
# goals
# own_goals
# shots
# xG
# key_passes
# assists
# xA
# xGChain
# xGBuildup

bayern <- matchStatsStats[matchStatsStats$team_id==117,] %>%
  group_by(player) %>%
  summarise(goals=sum(goals),
            shots=sum(shots),
            xG=sum(xG),
            key_passes=sum(key_passes),
            assists=sum(assists),
            xA=sum(xA),
            xGChain=sum(xGChain),
            xGBuildup=sum(xGBuildup))
bayern <- as.data.frame(bayern)
bayern$player <- gsub(" ", "", bayern$player, fixed = TRUE)
rownames(bayern) <- bayern$player
bayern <- bayern[,-1]

model_sampleShots <- hc(bayern)
graphviz.plot(model_sampleShots)

model_sampleShots <- hc(data.frame(t(bayern)))
graphviz.plot(model_sampleShots)


################################################################################################################

#read in your data
dat <- read.table(text="TrxID Items Quant
Trx1 A 3
Trx1 B 1
Trx1 C 1
Trx2 E 3
Trx2 B 1
Trx3 B 1
Trx3 C 4
Trx4 D 1
Trx4 E 1
Trx4 A 1
Trx5 F 5
Trx5 B 3
Trx5 C 2
Trx5 D 1", header=T)

#making the boolean matrix   
library(reshape2)
dat2 <- melt(dat)
w <- dcast(dat2, Items~TrxID)
x <- as.matrix(w[,-1])
x[is.na(x)] <- 0
x <- apply(x, 2,  function(x) as.numeric(x > 0))  #recode as 0/1
v <- x %*% t(x)                                   #the magic matrix 
diag(v) <- 0                                      #repalce diagonal
dimnames(v) <- list(w[, 1], w[,1])                #name the dimensions
v

########################################################

graphviz.plot(
  list_bnlearn[[1]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[2]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[3]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[4]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[5]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[6]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[6]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[7]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[8]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[9]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[10]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[11]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[12]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[13]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[14]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[15]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[[16]],
  shape='ellipse'
)

graphviz.plot(
  list_bnlearn[['hc']],
  shape='ellipse'
)

head(ALLEvents_HC)
whitelist = data.frame(
  from=colnames(ALLEvents_HC[-1]),
  to=rep('xA', ncol(ALLEvents_HC)-1)
)


list_bnlearn_hc <- do.call(
  what = 'hc',
  args = list(x = ALLEvents_HC, whitelist = whitelist)
  # args = list(x = ALLEvents_HCX)
)

# list_bnlearn_hc <- bnlearn::hc(
#   x = ALLEvents_HC, 
#   whitelist = whitelist
# )

graphviz.plot(
  list_bnlearn_hc,
  shape='ellipse'
)

HC_score <- matrix(
  data = NA,
  nrow = 1,
  ncol = length(scores_M),
)
rownames(HC_score) <- 'hc'
colnames(HC_score) <- scores_M
for(k in scores_M) try({
  HC_score[,k] <- score(
    x=list_bnlearn_hc,
    data=ALLEvents_HC,
    type=k
  )
}, silent = TRUE)
for(j in rownames(HC_score)) HC_score <- HC_score[,order(M_score[j,])]
for(j in colnames(HC_score)) HC_score <- HC_score[order(M_score[,j]),]
HC_score <- HC_score[!is.na(HC_score)]

fitted_hc = bn.fit(list_bnlearn_hc, ALLEvents_HC)
pred_hc = predict(
  object=fitted_hc,
  data=ALLEvents_HC[,-1],
  node='xA'
)
ALLEvents_HC[,-1]
