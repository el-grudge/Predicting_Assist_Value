library(StatsBombR)
library(dplyr)
library(bnlearn)
options(scipen=999)

Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)
ALLMATCHES <- Matches

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
  assistedShots <- FAWSLEvents[!is.na(FAWSLEvents$shot.outcome.name) & FAWSLEvents$shot.outcome.name=='Goal' & !is.na(FAWSLEvents$shot.key_pass_id),]
  assists <- FAWSLEvents[FAWSLEvents$id %in% assistedShots$shot.key_pass_id,]
  xADataset_M$pass.outcome <- ifelse(xADataset_M$id %in% assists$id, 'Goal', 'No goal')
  ALLEvents <- rbind(ALLEvents, xADataset_M)
}
write.csv(ALLEvents, 'ALLEvents.csv')

#####################################################
# algorithms
v_algorithms <- c(
  "iamb.fdr","fast.iamb","inter.iamb","gs","iamb","hpc","si.hiton.pc","mmpc","pc.stable",
  "hc","tabu",
  "h2pc","mmhc","rsmax2",
  "aracne","chow.liu"
)

# modeling
ALLEvents_HC <- ALLEvents[,c('xA','pass.length','pass.angle','pass.height.name','start.X','start.Y','end.X','end.Y')]
for (j in ncol(ALLEvents_HC)){
  ALLEvents_HC[,j] <- factor(ALLEvents_HC[,j])
}
list_bnlearn <- list()
# for(j in v_algorithms) try({
#   list_bnlearn[[j]] <- do.call(
#     what = j,
#     args = list(x = ALLEvents_HC)
#   )
# },silent = TRUE)

for(j in v_algorithms) try({
  list_bnlearn[[j]] <- do.call(
    what = j,
    args = list(x = ALLEvents_HC)
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
scores_M <- c('aic','bic','loglik','pred-loglik','bde','loglik-g','aic-g','bic-g','bge','loglik-cg','aic-cg','bic-cg','pred-loglik-cg')
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
})
for(j in rownames(M_score)) M_score <- M_score[,order(M_score[j,])]
for(j in colnames(M_score)) M_score <- M_score[order(M_score[,j]),]
M_score

M_score1 <- M_score[,colSums(is.na(M_score))<nrow(M_score)]
M_score2 <- M_score1[rowSums(is.na(M_score1))<ncol(M_score1),]

rownames(M_score2)[which(M_score2 == get(functional)(M_score2), arr.ind = TRUE)[1]]
colnames(M_coef)[which(M_coef == get(functional)(M_coef), arr.ind = TRUE)[2]]


M_score2 <- data.frame(M_score2)

# h2pc, loglik-cg: -318897.1
# mmhc, loglik-cg: -318897.1
# rsmax2, loglik-cg: -318897.1
# hc, loglik-cg: -318550.4
# tabu, loglik-cg: -318543.7

################################################################################################################################
# plot
graphviz.plot(
  list_bnlearn[['tabu']],
  shape='ellipse'
  )

################################################################################################################################
# predict
fitted = bn.fit(list_bnlearn[['tabu']], ALLEvents_HC)
pred = predict(
  object=fitted,
  data=ALLEvents_HC[,-1],
  node='xA'
  )
ALLEvents_HC[,-1]



################################################################################################################################

