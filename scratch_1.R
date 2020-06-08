######################################################################################## ORIGINAL

# libraries
library(StatsBombR)
library(dplyr)
library(ggplot2)

################################################################################## RETRIEVING DATA
# 1- get free competitions
# 2- get free matches
# 3- get FA Women's Super League matches
Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)
FAWSL <- filter(Matches, competition.competition_name == 'FA Women\'s Super League')

# find common columns
col_counter <- data.frame(x=character(0), y=numeric(0), stringsAsFactors=FALSE)
colnames(col_counter) <- c('colname', 'colcount')
`%notin%` <- Negate(`%in%`)

for (i in FAWSL$match_id){
  print(i)
  colz <- c(colnames(get.matchFree(filter(FAWSL, match_id == i))))
  for (j in 1:length(colz)){
    if(colz[j] %notin% col_counter$colname){
      col_counter[nrow(col_counter) + 1,] = list(colz[j], 1)
    } else {
      col_counter$colcount[col_counter$colname == colz[j]] <- col_counter$colcount[col_counter$colname == colz[j]]+1
    }
  }
}

# get match events
colkeys <- col_counter$colname[col_counter$colcount == 194]

FAWSLEvents <- data.frame()
for (i in FAWSL$match_id){
  print(i)
  event <- select(get.matchFree(filter(FAWSL, match_id == i)), all_of(colkeys))
  event$match_name <- paste(filter(FAWSL, match_id == i)$home_team.home_team_name,
                            'v',
                            filter(FAWSL, match_id == i)$away_team.away_team_name) 
  event$match_date <- filter(FAWSL, match_id == i)$match_date
  FAWSLEvents <- rbind(FAWSLEvents, event)
}

# get assisted shots
FAWSLEvents$xA <- NA
FAWSLXG <- FAWSLEvents[!is.na(FAWSLEvents$shot.key_pass_id),c('shot.key_pass_id','shot.statsbomb_xg')]
FAWSLEvents[FAWSLEvents$id %in% FAWSLXG$shot.key_pass_id,'xA'] <- cbind(FAWSLEvents[FAWSLEvents$id %in% FAWSLXG$shot.key_pass_id,c('xA','id')],FAWSLXG)[,'shot.statsbomb_xg']
xADataset_M <- FAWSLEvents[!is.na(FAWSLEvents$xA),]
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
for (i in c(1:nrow(xADataset_M))){
  xADataset_M[i, 'start.X'] <- unlist(xADataset_M[i,'location'])[1]
  xADataset_M[i, 'start.Y'] <- unlist(xADataset_M[i,'location'])[2]
  xADataset_M[i, 'end.X'] <- unlist(xADataset_M[i,'pass.end_location'])[1]
  xADataset_M[i, 'end.Y'] <- unlist(xADataset_M[i,'pass.end_location'])[2]
}
xADataset_M$startDist <- sqrt(((xADataset_M$start.X-120)^2) + ((xADataset_M$start.Y-40)^2))
xADataset_M$endDist <- sqrt(((xADataset_M$end.X-120)^2) + ((xADataset_M$end.Y-40)^2))
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
write.csv(xADataset_M, 'xADataset_M.csv')

############################################################################################################

totalXA <- xADataset_M %>% 
  group_by(player.name) %>%
  summarise(
    assists=sum(pass.outcome=='Goal'),
    fromThrowIns=n_distinct(id[play_pattern.name=='From Throw In']),
    regularPlay=n_distinct(id[play_pattern.name=='Regular Play']),
    fromFreeKick=n_distinct(id[play_pattern.name=='From Free Kick']),
    fromKeeper=n_distinct(id[play_pattern.name=='From Keeper']),
    fromCounter=n_distinct(id[play_pattern.name=='From Counter']),
    fromCorner=n_distinct(id[play_pattern.name=='From Corner']),
    fromKickOff=n_distinct(id[play_pattern.name=='From Kick Off']),
    fromGoalKick=n_distinct(id[play_pattern.name=='From Goal Kick']),
    fromOther=n_distinct(id[play_pattern.name=='Other']),
    passLength=mean(pass.length),
    groundPass=n_distinct(id[pass.height.name=='Ground Pass']),
    highPass=n_distinct(id[pass.height.name=='High Pass']),
    lowPass=n_distinct(id[pass.height.name=='Low Pass']),
    rightFoot=n_distinct(id[pass.body_part.name=='Right Foot']),
    head=n_distinct(id[pass.body_part.name=='Head']),
    leftFoot=n_distinct(id[pass.body_part.name=='Left Foot']),
    other=n_distinct(id[pass.body_part.name=='Other']),
    noTouch=n_distinct(id[pass.body_part.name=='No Touch']),
    dropKick=n_distinct(id[pass.body_part.name=='Drop Kick']),
    startX=mean(start.X),
    startY=mean(start.Y),
    endX=mean(end.X),
    endY=mean(end.Y),
    startPosition=mean(startDist),
    endPosition=mean(endDist)
  ) %>% 
  arrange(desc(assists))
totalXA$group <- ifelse(totalXA$shotAssists<=10,'10',
                        ifelse(totalXA$shotAssists>10 & totalXA$shotAssists<=20,'20',
                               ifelse(totalXA$shotAssists>20 & totalXA$shotAssists<=30, '30',
                                      ifelse(totalXA$shotAssists>30 & totalXA$shotAssists<=40, '40',
                                             ifelse(totalXA$shotAssists>40 & totalXA$shotAssists<=50, '50','60')))))
totalXA$group <- factor(x=totalXA$group, levels=c('60','50','40','30','20','10'))
write.csv(totalXA, 'totalXA.csv')
############################################################################################################

totalXA %>%
  select(-player.name) %>%
  reshape2::melt(id.vars=c('assists','group')) %>%
  ggplot() +
  aes(x=value, y=assists, color=group) + 
  geom_point() + 
  scale_colour_viridis_d() +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Relationship between points and other factors",
       x ='assists')

############################################################################################################

totalXA <- xADataset_M %>% 
  group_by(player.name) %>%
  summarise(
    shotAssists=n(), 
    totalXA=sum(xA), 
    assists=sum(pass.outcome=='Goal'),
    fromThrowIns=n_distinct(id[play_pattern.name=='From Throw In']),
    regularPlay=n_distinct(id[play_pattern.name=='Regular Play']),
    fromFreeKick=n_distinct(id[play_pattern.name=='From Free Kick']),
    fromKeeper=n_distinct(id[play_pattern.name=='From Keeper']),
    fromCounter=n_distinct(id[play_pattern.name=='From Counter']),
    fromCorner=n_distinct(id[play_pattern.name=='From Corner']),
    fromKickOff=n_distinct(id[play_pattern.name=='From Kick Off']),
    fromGoalKick=n_distinct(id[play_pattern.name=='From Goal Kick']),
    fromOther=n_distinct(id[play_pattern.name=='Other']),
    passLength=mean(pass.length),
    passAngle=mean(pass.angle),
    groundPass=n_distinct(id[pass.height.name=='Ground Pass']),
    highPass=n_distinct(id[pass.height.name=='High Pass']),
    lowPass=n_distinct(id[pass.height.name=='Low Pass']),
    rightFoot=n_distinct(id[pass.body_part.name=='Right Foot']),
    head=n_distinct(id[pass.body_part.name=='Head']),
    leftFoot=n_distinct(id[pass.body_part.name=='Left Foot']),
    other=n_distinct(id[pass.body_part.name=='Other']),
    noTouch=n_distinct(id[pass.body_part.name=='No Touch']),
    dropKick=n_distinct(id[pass.body_part.name=='Drop Kick']),
    startX=mean(start.X),
    startY=mean(start.Y),
    endX=mean(end.X),
    endY=mean(end.Y),
    startPosition=mean(startDist),
    endPosition=mean(endDist),
    goal=n_distinct(id[pass.outcome=='Goal']),
    noGoal=n_distinct(id[pass.outcome=='No goal'])
    ) %>% 
  arrange(desc(assists))

totalXA$group <- ifelse(totalXA$shotAssists<=10,'Ten',
                        ifelse(totalXA$shotAssists>10 & totalXA$shotAssists<=20,'Twenty',
                               ifelse(totalXA$shotAssists>20 & totalXA$shotAssists<=30, 'Thirty',
                                      ifelse(totalXA$shotAssists>30 & totalXA$shotAssists<=40, 'Forty',
                                             ifelse(totalXA$shotAssists>40 & totalXA$shotAssists<=50, 'Fifty','FiftyPlus')))))
totalXA$group <- factor(x=totalXA$group, levels=c('FiftyPlus','Fifty','Forty','Thirty','Twenty','Ten'))

totalXA %>%
  filter(group!='Ten') %>%
  select(-player.name) %>%
  reshape2::melt(id.vars=c('assists','group')) %>%
  ggplot() +
  aes(x=value, y=assists, color=group) + 
  geom_point() + 
  scale_colour_viridis_d() +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Relationship between points and other factors",
       x ='assists')

########################################################################################################################################################################################################

mnist <- dslabs::read_mnist()
X <- as.data.frame(mnist$train$images[1:200,])
X <- X[,sapply(X = X,FUN = max) > 0]
y <- mnist$train$labels[1:1000]
dim(mnist$train$images)
## [1] 60000   784
head(X[,180:188])
##   V313 V314 V315 V316 V317 V318 V319 V320 V321
## 1    0    0    0    0    0    0    0  139  253
## 2    0    0    0    0   38  165  253  233  208
## 3  254  163    0    0    0    0    0    0    0
## 4    0    0    0    0    0    0    0    0    0
## 5    0    0    0    0  132  253  252  146   14
## 6    0    0    0    0    0   26  128   58   22
head(y)
## [1] 5 0 4 1 9 2

################################################## Original dataset

totalXA <- xADataset_M %>% 
  group_by(player.name) %>%
  summarise(
    shotAssists=n(), 
    totalXA=sum(xA), 
    assists=sum(pass.outcome=='Goal'),
    fromThrowIns=n_distinct(id[play_pattern.name=='From Throw In']),
    regularPlay=n_distinct(id[play_pattern.name=='Regular Play']),
    fromFreeKick=n_distinct(id[play_pattern.name=='From Free Kick']),
    fromKeeper=n_distinct(id[play_pattern.name=='From Keeper']),
    fromCounter=n_distinct(id[play_pattern.name=='From Counter']),
    fromCorner=n_distinct(id[play_pattern.name=='From Corner']),
    fromKickOff=n_distinct(id[play_pattern.name=='From Kick Off']),
    fromGoalKick=n_distinct(id[play_pattern.name=='From Goal Kick']),
    fromOther=n_distinct(id[play_pattern.name=='Other']),
    passLength=mean(pass.length),
    passAngle=mean(pass.angle),
    groundPass=n_distinct(id[pass.height.name=='Ground Pass']),
    highPass=n_distinct(id[pass.height.name=='High Pass']),
    lowPass=n_distinct(id[pass.height.name=='Low Pass']),
    rightFoot=n_distinct(id[pass.body_part.name=='Right Foot']),
    head=n_distinct(id[pass.body_part.name=='Head']),
    leftFoot=n_distinct(id[pass.body_part.name=='Left Foot']),
    other=n_distinct(id[pass.body_part.name=='Other']),
    noTouch=n_distinct(id[pass.body_part.name=='No Touch']),
    dropKick=n_distinct(id[pass.body_part.name=='Drop Kick']),
    startX=mean(start.X),
    startY=mean(start.Y),
    endX=mean(end.X),
    endY=mean(end.Y),
    startPosition=mean(startDist),
    endPosition=mean(endDist),
    goal=n_distinct(id[pass.outcome=='Goal']),
    noGoal=n_distinct(id[pass.outcome=='No goal'])
  ) %>% 
  arrange(desc(assists))
totalXA$group <- ifelse(totalXA$shotAssists<=10,'10',
                        ifelse(totalXA$shotAssists>10 & totalXA$shotAssists<=20,'20',
                               ifelse(totalXA$shotAssists>20 & totalXA$shotAssists<=30, '30',
                                      ifelse(totalXA$shotAssists>30 & totalXA$shotAssists<=40, '40',
                                             ifelse(totalXA$shotAssists>40 & totalXA$shotAssists<=50, '50','60')))))
totalXA$group <- factor(x=totalXA$group, levels=c('60','50','40','30','20','10'))


################################################# DIM REDUCE
# TNSE
# set.seed(823)
# Rtsne_1 <- Rtsne::Rtsne(
#   X = X
# )
# plot(
#   Rtsne_1$Y,
#   col = y,
#   pch = as.character(y),
#   main = "Scatterplot of MNIST T-SNE two dimensions"
# )

set.seed(823)
Rtsne_2 <- Rtsne::Rtsne(
  X=select(totalXA,-player.name,-group,-shotAssists,-totalXA,-assists,-goal,-noGoal)
)
plot(
  Rtsne_2$Y,
  col=totalXA$group,
  pch=as.character(totalXA$group)
)

RTSNEFeatures <- cbind(Rtsne_2$Y,totalXA$group)
colnames(RTSNEFeatures) <- c('feature1','feature2','group')
RTSNEFeatures <- data.frame(RTSNEFeatures)
ggplot(RTSNEFeatures) +
  aes(x=feature1, y=feature2, color=as.character(group)) +
  geom_point() +
  scale_color_viridis_d()

# PRCOMP
prcomp_mnist <- prcomp(
  x = select(totalXA,-player.name,-group,-shotAssists,-totalXA,-assists,-goal,-noGoal),
  center = TRUE,
  scale. = TRUE,
  rank = 2
)
plot(
  prcomp_mnist$x[,1:2],
  col = totalXA$group,
  pch = 
)

PRCCompFeatures <- cbind(prcomp_mnist$x[,1:2],totalXA$group)
colnames(PRCCompFeatures) <- c('feature1','feature2','group')
PRCCompFeatures <- data.frame(PRCCompFeatures)
ggplot(PRCCompFeatures) +
  aes(x=feature1, y=feature2, color=as.character(group)) +
  geom_point() +
  scale_color_viridis_d()

# NONNEGATIVE
nmf_acq <- NMF::nmf(
  x = select(totalXA,-player.name,-group,-shotAssists,-totalXA,-assists,-goal,-noGoal, -passAngle),
  rank = 2
)
nmf_acq
basis_acq <- NMF::basis(nmf_acq)
coef_acq <- NMF::coef(nmf_acq)

dim(select(totalXA,-player.name,-group,-shotAssists,-totalXA,-assists,-goal,-noGoal, -passAngle))
dim(basis_acq)
dim(coef_acq)

round(head(basis_acq),3)
round(head(coef_acq[,1:5]),3)

nonNegFeatures <- cbind(basis_acq, totalXA$group)
colnames(basis_acq) <- c('feature1','feature2','group')
nonNegFeatures <- data.frame(nonNegFeatures)

ggplot(nonNegFeatures) +
  aes(x=feature1, y=feature2, color=as.character(group)) +
  geom_point() +
  scale_color_viridis_d()


############################################################################################################################################################################################

# Correlation plots

totalXA.cor <- cor(select(totalXA, -player.name, -assists, -group))
ggcorrplot::ggcorrplot(totalXA.cor,
                       lab=TRUE, 
                       show.legend=FALSE,
                       hc.order=TRUE,
                       color=c('#FDE725FF', '#238A8DFF', '#440154FF'),
                       lab_size = 2,
                       outline.color='black')

dist_M <- dist(scale_M)
heatmap(
  x = as.matrix(dist_M),
  col = viridis::viridis(256)
)

heatmap(
  x = as.matrix(dist(select(totalXA, -player.name, -assists, -group))),
  col = viridis::viridis(256)
)


lin_model_xg <- lm(scored~xgb4, data=data)