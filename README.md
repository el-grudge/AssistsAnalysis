# AssistsAnalysis

In soccer, the most valuable players are those who score the most goals. However, goalscorers on their own can't do much, they need someone to provide them the ball so they can score. In this analysis I'll look at those providers, the playmakers. It could be argued that assisting is harder than scoring, since it involves picking out a player in space, deciding whether the receiver wants the ball at his feet or ahead of him (if he is making a run behind defense), and doing all of this while under pressure from the opponent's defenders.

I'll be using data from the English Women Super League (FAWSL), provided by statsbomb (https://statsbomb.com/), which provides event tracking data, including pass specific information such as start and end locations & pass height. Statsbomb has opensourced the data from both the 2018/19 & 2019/20 seasons. 

During this period, there have been 383 assisted goals, and the top five assist providers are:

Vivianne Miedema - Arsenal: 18

![alt text](./vivianne_miedema.jpg)

Bethany Mead - Arsenal: 14

![alt text](./bethany_maed.jpg)

Caroline Weir - Manchester City: 10

![alt text](./caroline_weir.jpg)

Danielle van de Donk - Arsenal: 10

![alt text](./danielle_van_de_donk.jpg)

Keira Walsh - Manchester City: 10

![alt text](./keira_walsh.jpg)

Incidently, these are the only players with double digits assists. In addition to assisted goals, I also include all assisted shots. I do this for two reasons: 1- To increase the size of dataset - there are 3426 assisted shots compared to only 383 assisted goals. 2- I want to analyze playmaking skill not goal scoring skill; a good playmaker can create a good chance to two different players, one can turn it into a goal while the other might shoot the ball wide - the playmaker ability shouldn't be judged by this miss.

Here is a snapshot of the original data:

![alt text](./data1.png)

As mentioned before, the data here describes events, I still need to aggregate it in order to get the totals. The final dataset looks like this:

![alt text](./data2.png)

The aggregated data doesn't include any categorical variables, since these have been turned into counts (for example, Vivianne Miedema has provided 10 assists from free kicks).The next step is to split the players into separate bins, determined by the number of assists they provide. In total, we have 6 groups, from those who have provided less than 10 assists (10-), up to those who have provided more than 50 assists (50+):

![alt text](./groups.png)

Now, let's have a look at the relationship between assists and the different features:

![alt text](./relationships.png)

Each figure describes the relationship between assists and a specific feature. One thing of note here is that there is no distinct relationship between any of the features and the target value. It should be mentioned here that the top left image shows the relationship between goal assists and shot assists, so it doesn't actually show a relationship between a predictor and a target, it rather shows the relationship between two aggregate values. What I'm trying to say is that shot assists are a good proxy of the goal assists:

![alt text](./shotAssists.png)

A good dimensionality reduction technique should be able to separate the distinct groups when plotted. This will be the main measure of how effective each technique is.

## Dimensionality Reduction

### 1- T-SNE

I start with the T-SNE technique. Here is the result:

![alt text](./tsne.png)

### 2- PCA

Next, I look at PCA. Here is the result:

![alt text](./pca.png)

### 3- Non-negative Matrix Factorization

Finally, non-negative matrix factorization. Here is the result:

![alt text](./nonnegative.png)

**From these figures, we can say confidently that non-negative matrix factorization is the best dimensionality reduction technique for our analysis.**

Appendix I - Code

```
# libraries
library(StatsBombR)
library(dplyr)
library(ggplot2)

################################################################################## RETRIEVING DATA
# 1- get free competitions
# 2- get free matches
# 3- get FA Women's Super League matches

getAssists <- function(fromSource=FALSE){
  if (fromSource){
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
    
    totalXA <- xADataset_M %>% 
      group_by(player.name) %>%
      summarise(
        shotAssists=n(), 
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
        endY=mean(end.Y)
      ) %>% 
      arrange(desc(assists))
    totalXA$group <- ifelse(totalXA$shotAssists<=10,'10-',
                            ifelse(totalXA$shotAssists>10 & totalXA$shotAssists<=20,'20-',
                                   ifelse(totalXA$shotAssists>20 & totalXA$shotAssists<=30, '30-',
                                          ifelse(totalXA$shotAssists>30 & totalXA$shotAssists<=40, '40-',
                                                 ifelse(totalXA$shotAssists>40 & totalXA$shotAssists<=50, '50-','50+')))))
    totalXA$group <- factor(x=totalXA$group, levels=c('50+','50-','40-','30-','20-','10-'))
    write.csv(totalXA, 'totalXA.csv', row.names = FALSE)
  } else {
    totalXA <- read.csv('totalXA.csv')
  }
  return (totalXA)
}

############################################################################################################

totalXA <- getAssists(fromSource=FALSE)

############################################################################################################

totalXA %>%
  select(-player.name) %>%
  reshape2::melt(id.vars=c('assists','group')) %>%
  ggplot() +
  aes(x=assists, y=value, color=group) + 
  geom_point() + 
  scale_colour_viridis_d() +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Relationship between assists and other factors",
       x ='assists')

ggplot(totalXA) +
  aes(x=assists, y=shotAssists, color=group) +
  geom_point() + 
  scale_colour_viridis_d() +
  theme_minimal() +
  labs(title = "Relationship between assists and shotAssists")

####################################################################################### DIMENSION REDUCTION

# TNSE
set.seed(823)
RtsneAssists <- Rtsne::Rtsne(
  X=select(totalXA,-player.name,-shotAssists,-assists,-group)
)

RTSNEFeatures <- data.frame(RtsneAssists$Y,totalXA$group)
colnames(RTSNEFeatures) <- c('feature1','feature2','group')
ggplot(RTSNEFeatures) +
  aes(x=feature1, y=feature2, color=group) +
  geom_point() +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "T-SNE")


# PRCOMP
prcompAssists <- prcomp(
  x = select(totalXA,-player.name,-shotAssists,-assists,-group),
  center = TRUE,
  scale. = TRUE,
  rank = 2
)

PRCCompFeatures <- data.frame(prcompAssists$x[,1:2],totalXA$group)
colnames(PRCCompFeatures) <- c('feature1','feature2','group')
ggplot(PRCCompFeatures) +
  aes(x=feature1, y=feature2, color=group) +
  geom_point() +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "PCA")


# NONNEGATIVE
nmfAssists <- NMF::nmf(
  x = select(totalXA,-player.name,-shotAssists,-assists,-group),
  rank = 2
)
basis_acq <- NMF::basis(nmfAssists)
coef_acq <- NMF::coef(nmfAssists)
t(round(head(coef_acq),3)) %>% View()

nonNegFeatures <- data.frame(basis_acq, totalXA$group)
colnames(nonNegFeatures) <- c('feature1','feature2','group')
ggplot(nonNegFeatures) +
  aes(x=feature1, y=feature2, color=group) +
  geom_point() +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Non-Negative Matrix Factorization")

```
