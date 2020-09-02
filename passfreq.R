# View(FAWSLEvents[FAWSLEvents$match_id==19800 & FAWSLEvents$team.id==968 & FAWSLEvents$possession_team.id==968 & FAWSLEvents$type.name=='Pass',c('player.name', 'pass.recipient.name')])

arsenal19800 <- FAWSLEvents[FAWSLEvents$match_id==19800 & FAWSLEvents$team.id==968 & FAWSLEvents$possession_team.id==968,]
arsenal19800$player.name <- make.names(arsenal19800$player.name)
arsenal19800$pass.recipient.name <- make.names(arsenal19800$pass.recipient.name)

starting11 <- arsenal19800$tactics.lineup[[1]]
starting11$player.name <- make.names(starting11$player.name)
# arsenal19800[arsenal19800$type.name=='Pass',]
# arsenal19800[arsenal19800$type.name=='Pass' & arsenal19800$player.name %in% starting11$player.name,]

# arsenal_Sample <- arsenal19800[arsenal19800$type.name=='Pass',]
# arsenal_Sample <- arsenal19800[arsenal19800$type.name=='Pass' & arsenal19800$player.name %in% starting11$player.name & arsenal_Sample$pass.recipient.name %in% starting11$player.name, c('player.name', 'pass.recipient.name')]
arsenal_Sample <- arsenal19800[arsenal19800$type.name=='Pass',]
arsenal_Sample <- arsenal_Sample[arsenal_Sample$player.name %in% starting11$player.name,]
arsenal_Sample <- arsenal_Sample[arsenal_Sample$pass.recipient.name %in% starting11$player.name,]
arsenal_Sample <- arsenal_Sample[,c('player.name', 'pass.recipient.name')]
# matchDetails <- FAWSLEvents[FAWSLEvents$match_id==19800 & FAWSLEvents$team.id==968 & FAWSLEvents$possession_team.id==968 & FAWSLEvents$type.name=='Pass',]
arsenal_Sample <- arsenal_Sample[stats::complete.cases(arsenal_Sample),]

arsenal_Sample$player.name <- as.factor(arsenal_Sample$player.name)
arsenal_Sample$pass.recipient.name <- as.factor(arsenal_Sample$pass.recipient.name)

# model_sampleShots <- bnlearn::hc(arsenal_Sample)
# graphviz.plot(model_sampleShots)

# model_sampleShots <- bnlearn::hc(data.frame(t(arsenal_Sample)))
# graphviz.plot(model_sampleShots)

# for (i in unique(arsenal_Sample$player.name)){
#   print(i)
# }
# 
# i1 <- arsenal_Sample$player.name[1]
# View(table(arsenal_Sample[arsenal_Sample$player.name==i1,]))
# arsenal_PassTable <- table(arsenal_Sample)
# arsenal_PassTable <- data.frame(arsenal_PassTable)
# passers <- unique(arsenal_PassTable$player.name)
# receipients <- unique(arsenal_PassTable$pass.recipient.name)

library(data.table)
# View(dcast(arsenal_Sample, player.name~pass.recipient.name))
# passFreq <- reshape2::dcast(arsenal_Sample, player.name~pass.recipient.name)
passFreq <- reshape2::dcast(arsenal_Sample, pass.recipient.name~player.name)
passFreq <- passFreq[,-1]
passFreq <- data.frame(sapply(passFreq, as.numeric))

# playerNames <- colnames(passFreq)
# lookup <- data.frame(cbind(colnames(passFreq),seq(1:length(colnames(passFreq)))))
# lookup$id <- c('23','9','7','20','4','12','15','2','10','6','17','16','18','11')
# colnames(lookup) <- c('player.name', 'id')
# matchDetails <- unique(matchDetails[,c('player.name','position.name')])
# matchDetails$player.name <- make.names(matchDetails$player.name)
# lookup <- dplyr::left_join(lookup,matchDetails,by='player.name')

colnames(passFreq) <- starting11[order(starting11$player.name),'jersey_number']
rownames(passFreq) <- colnames(passFreq)

library(bnlearn)
model_sampleShots <- hc(passFreq)
graphviz.plot(model_sampleShots)

model_sampleShots <- iamb.fdr(passFreq)
graphviz.plot(model_sampleShots)

model_sampleShots <- h2pc(passFreq)
graphviz.plot(model_sampleShots)

model_sampleShots <- aracne(passFreq)
graphviz.plot(model_sampleShots)



arc.strength_hailfinder <- arc.strength(
  x = model_sampleShots,
  data = passFreq
)

strength.plot(
  x = model_sampleShots,
  strength = arc.strength_hailfinder
)
