options(scipen=999)
library(StatsBombR)
library(dplyr)
library(bnlearn)

Comp <- FreeCompetitions()
ALLMATCHES <- FreeMatches(Comp)

# find common columns
col_counter <- data.frame(x=character(0), y=numeric(0), stringsAsFactors=FALSE)
colnames(col_counter) <- c('colname', 'colcount')
`%notin%` <- Negate(`%in%`)

for (i in ALLMATCHES$match_id){
  print(i)
  colz <- c(colnames(get.matchFree(filter(ALLMATCHES, match_id == i))))
  for (j in 1:length(colz)){
    if(colz[j] %notin% col_counter$colname){
      col_counter[nrow(col_counter) + 1,] = list(colz[j], 1)
    } else {
      col_counter$colcount[col_counter$colname == colz[j]] <- col_counter$colcount[col_counter$colname == colz[j]]+1
    }
  }
}
colkeys <- col_counter$colname[col_counter$colcount == max(col_counter$colcount)]
write.csv(colkeys, 'colkeys.csv', row.names = FALSE)

# get match events
ALLEvents <- data.frame()
for (i in c(1:length(ALLMATCHES$match_id))){
  print(i)
  event <- select(get.matchFree(filter(ALLMATCHES, match_id == ALLMATCHES$match_id[i])), all_of(colkeys))
  event$match_name <- paste(filter(ALLMATCHES, match_id == ALLMATCHES$match_id[i])$home_team.home_team_name,
                            'v',
                            filter(ALLMATCHES, match_id == ALLMATCHES$match_id[i])$away_team.away_team_name) 
  event$match_date <- filter(ALLMATCHES, match_id == ALLMATCHES$match_id[i])$match_date
  event$xA <- NA
  eventXG <- event[!is.na(event$shot.key_pass_id),c('shot.key_pass_id','shot.statsbomb_xg')]
  event[event$id %in% eventXG$shot.key_pass_id,'xA'] <- cbind(event[event$id %in% eventXG$shot.key_pass_id,c('xA','id')],eventXG)[,'shot.statsbomb_xg']
  xADataset_M <- event[!is.na(event$xA),]
  xADataset_M <- select(xADataset_M,
                        id,
                        player.name,
                        xA,
                        location,
                        play_pattern.name,
                        starts_with('pass'),
                        -pass.assisted_shot_id,
                        -pass.shot_assist,
                        -pass.recipient.id,
                        -pass.recipient.name,
                        -pass.height.id,
                        -pass.type.id,
                        -pass.body_part.id,
                        -pass.outcome.id,
                        -pass.cross,
                        -pass.switch,
                        -pass.type.name,
                        -pass.outcome.name
  )
  
  xADataset_M$start.X <- NA
  xADataset_M$start.Y <- NA
  xADataset_M$end.X <- NA
  xADataset_M$end.Y <- NA
  for (j in c(1:nrow(xADataset_M))){
    xADataset_M[j, 'start.X'] <- unlist(xADataset_M[j,'location'])[1]
    xADataset_M[j, 'start.Y'] <- unlist(xADataset_M[j,'location'])[2]
    xADataset_M[j, 'end.X'] <- unlist(xADataset_M[j,'pass.end_location'])[1]
    xADataset_M[j, 'end.Y'] <- unlist(xADataset_M[j,'pass.end_location'])[2]
  }
  xADataset_M <- select(xADataset_M, -location, -pass.end_location)
  
  # missing values
  apply(is.na(xADataset_M), 2, sum)
  xADataset_M[is.na(xADataset_M$pass.body_part.name),'pass.body_part.name'] <- 'Other'
  
  # handling categorical columns
  xADataset_M$play_pattern.name <- as.factor(xADataset_M$play_pattern.name)
  xADataset_M$pass.height.name <- as.factor(xADataset_M$pass.height.name)
  xADataset_M$pass.body_part.name <- as.factor(xADataset_M$pass.body_part.name)
  assistedShots <- event[!is.na(event$shot.outcome.name) & event$shot.outcome.name=='Goal' & !is.na(event$shot.key_pass_id),]
  assists <- event[event$id %in% assistedShots$shot.key_pass_id,]
  xADataset_M$pass.outcome <- ifelse(xADataset_M$id %in% assists$id, 'Goal', 'No goal')
  ALLEvents <- rbind(ALLEvents, xADataset_M)
}
write.csv(ALLEvents, 'ALLEvents.csv', row.names = FALSE)

#####################################################
# modelling
ALLEvents <- read.csv('ALLEvents.csv')
ALLEvents_HC <- ALLEvents[,c('xA','pass.length','pass.angle','start.X','start.Y','end.X','end.Y')]

v_algorithms <- c(
  "iamb.fdr","fast.iamb",
  "hc","tabu",
  "mmhc","rsmax2",
  "aracne","chow.liu"
)

list_bnlearn <- list()
whitelist = data.frame(
  from=colnames(ALLEvents_HC[-1]),
  to=rep('xA', ncol(ALLEvents_HC)-1)
)
for(j in v_algorithms) try({
  list_bnlearn[[j]] <- do.call(
    what = j,
    args = list(x = ALLEvents_HC, whitelist = whitelist)
  )
  M_arcs <- arcs(list_bnlearn[[j]])
  for(l in 1:nrow(M_arcs)){
    list_bnlearn[[j]] <- set.arc(
      x = list_bnlearn[[j]],
      from = M_arcs[l,1],
      to = M_arcs[l,2],
      check.cycles = FALSE,
      check.illegal = FALSE
    )
    list_bnlearn[[j]] <- choose.direction(
      x = list_bnlearn[[j]],
      arc = M_arcs[l,],
      data = ALLEvents_HC
    )
  }
},silent = TRUE)

################################################################################################################################
# scoring
scores_M <- c('loglik-g','aic-g','bic-g')
M_score <- matrix(
  data = NA,
  nrow = length(v_algorithms),
  ncol = length(scores_M),
)
rownames(M_score) <- v_algorithms
colnames(M_score) <- scores_M

for(j in v_algorithms) for(k in scores_M) try({
  M_score[j,k] <- score(
    x = list_bnlearn[[j]],
    data = ALLEvents_HC,
    type = k
  )
},silent=TRUE)
for(j in rownames(M_score)) M_score <- M_score[,order(M_score[j,])]
for(j in colnames(M_score)) M_score <- M_score[order(M_score[,j]),]
View(M_score)

max(M_score)
which.max(M_score)

################################################################################################################################
# plot
graphviz.plot(
  list_bnlearn[['hc']],
  shape='ellipse'
  )

list_bnlearn_modified <- reverse.arc(
  x = list_bnlearn[['hc']],
  from = "xA",
  to = "end.X",
  check.cycles = FALSE
)

score(
  x = list_bnlearn_modified,
  data = ALLEvents_HC,
  type = 'loglik-g'
)

graphviz.plot(
  list_bnlearn_modified,
  shape='ellipse'
)

################################################################################################################################
# predict
fitted_L <- list()
predictions <- list()
for (j in v_algorithms){
  fitted_L[[j]] = bn.fit(list_bnlearn[[j]], ALLEvents_HC)
  predictions[[j]] = predict(
    object=fitted_L[[j]],
    data=ALLEvents_HC[,-1],
    node='xA'
  )
}

fitted_L[['list_bnlearn_modified']] = bn.fit(list_bnlearn_modified, ALLEvents_HC)
predictions[['list_bnlearn_modified']] = predict(
  object=fitted_L[['list_bnlearn_modified']],
  data=ALLEvents_HC[,-1],
  node='xA'
)

# evaluate
hc_MSE <- ModelMetrics::mse(predictions[['hc']],ALLEvents_HC[,'xA'])
hc_Modified_MSE <- ModelMetrics::mse(predictions[['list_bnlearn_modified']],ALLEvents_HC[,'xA'])

predictedXA <- data.frame(data.frame(ALLEvents[,c('id','player.name','xA',"pass.outcome")],predictions=predictions[['list_bnlearn_modified']]) %>%
  group_by(player.name) %>%
  summarise(totalAssists = n_distinct(id[pass.outcome=='Goal']),
            totalXA = sum(xA),
            totalPredicted = sum(predictions)
  ) %>%
  arrange(desc(totalAssists)))
View(predictedXA)

# arc strength
arc.strength_xA <- arc.strength(
  x = list_bnlearn_modified,
  data = ALLEvents_HC
)

strength.plot(
  x = list_bnlearn_modified,
  strength = arc.strength_xA,
  shape='ellipse'
)

################################################################################################################################