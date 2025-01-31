library(tidyverse)

# number of males
n.M <- 1650
# number of females
n.F <- 1350
# attractivity is normally distributed with ICC = 0.6
attr.mean <- 50
attr.sd <- 50/4
attr.cor <- 0.6
attr.ind.sd <- sqrt((1-attr.cor)/attr.cor)*attr.sd

# number of desired matches
des.matches <- 7
# maximum number of swipes
max.swipes <- 700

# generate population with random attractivity score
set.seed(6764234)
df.start <- data.frame(
  id = 1:(n.M+n.F),
  sex = c(
    rep("M", n.M),
    rep("F", n.F)
  ),
  des.matches = des.matches,
  n.matches = NA,
  attr.score = pmax(pmin(rnorm(n.M+n.F, mean = attr.mean, sd = attr.sd), 100), 0),
  cons.up.weeks = 0,
  cons.op.weeks = 0,
  n.received.likes = 0
) |> 
  mutate(
    attr.threshold = attr.score,        # start attractivity threshold is own attractivity
    n.swipes = 70                       # everyone starts with 70 swipes/week
  )

# IDs
id.F <- df.start$id[df.start$sex == "F"]
id.M <- df.start$id[df.start$sex == "M"]

# Initiate simulation
df <- df.start
res.list <- list()
week <- 1
n.weeks <- 101

# Simulate
while (week < n.weeks) {
  
  print(week)
  # allocation of likes
  # males give likes to most attractive women in a random subset
  lapply(
    id.M,
    function(i){
      # swiped profiles are chosen randomly
      sample(
        x = id.F,
        size = df$n.swipes[i],
        replace = F
      ) ->
        swipes
      
      # individual attractivity is given to the swipes by adding noise to their mean attractivity scores
      attr.score <- pmax(pmin(df.start$attr.score[swipes] + rnorm(length(swipes), mean = 0, sd = attr.ind.sd), 100), 0)
      
      # give likes to persons above attractivity threshold
      likes <- swipes[attr.score >= df$attr.threshold[i]]
      likes
    }
  ) ->
    likes.men.to.women
  
  # the same for females
  lapply(
    id.F,
    function(i){
      sample(
        x = id.M,
        size = df$n.swipes[i],
        replace = F
      ) ->
        swipes
      
      # individual attractivity
      attr.score <- pmax(pmin(df.start$attr.score[swipes] + rnorm(length(swipes), mean = 0, sd = attr.ind.sd), 100), 0)
      
      # give likes
      likes <- swipes[attr.score >= df$attr.threshold[i]]
      likes
    }
  )  ->
    likes.women.to.men
  
  # Calculate week stats
  # number of matches for individual males
  sapply(
    id.M,
    function(i){
      likes.women.to.men[unlist(likes.men.to.women[id.M == i])-n.M] |> 
        unlist() |> 
        (\(x) x == i)() |> 
        sum()
    }
  ) ->
    df$n.matches[id.M]
  
  
  # number of matches for individual females
  sapply(
    id.F,
    function(i){
      likes.men.to.women[unlist(likes.women.to.men[id.F == i])] |> 
        unlist() |> 
        (\(x) x == i)() |> 
        sum()
    }
  ) ->
    df$n.matches[id.F]
  
  # number of received likes
  df$n.received.likes <- 0
  # males
  likes.named <- table(unlist(likes.women.to.men))
  df$n.received.likes[as.numeric(names(likes.named))] <- unname(likes.named)
  # females
  likes.named <- table(unlist(likes.men.to.women))
  df$n.received.likes[as.numeric(names(likes.named))] <- unname(likes.named)
  
  # add stats to result list
  res.list[[week]] <- df
  week <- week+1
  
  # Determine strategy for next week
  df |> 
    mutate(
      # number of consecutive underperforming weeks with less than desired matches
      cons.up.weeks = ifelse(
        n.matches < des.matches,
        cons.up.weeks+1,
        0
      ),
      # number of consecutive overperforming weeks with more than desired matches
      cons.op.weeks = ifelse(
        n.matches > des.matches,
        cons.op.weeks+1,
        0
      ),
      # number of swipes for next week
      n.swipes = case_when(
        n.matches == des.matches ~ n.swipes,      # if desired number of matches was achieved, change nothing
        n.matches == 0 ~ 1.2*n.swipes,           # if no matches, increase swipes by 20%
        n.matches < des.matches | n.swipes > 70 ~ sqrt(des.matches/n.matches)*n.swipes,    # if too few matches or too much matches with more than 70 swipes, adapt number of matches in desired direction
        n.matches > des.matches & n.swipes <= 70 ~ 70       # minimum number of swipes is 70, even if too much matches
      ) |> 
        (\(x) pmin(pmax(round(x), 1), max.swipes))(),
      # attractivity threshold for next week
      attr.threshold = case_when(
        week <= 3 | (cons.op.weeks < 2 & cons.up.weeks < 2) ~ attr.threshold,     # in the beginning of the simulation or with less than 2 overperforming or underperforming weeks, change nothing
        cons.up.weeks >= 2 ~ attr.threshold - (cons.up.weeks-1),    # if at least 2 underperforming weeks, decrease attractivity threshold. Decrease it more after more underperforming weeks
        cons.op.weeks >= 2 ~ attr.threshold + (cons.op.weeks-1)    # if at least 2 overperforming weeks, increase attractivity threshold. Increase it more after more overperforming weeks
      ) |> 
        (\(x) pmin(pmax(x, 0), 100))(),
      # After 4 underperforming weeks, decrease number of desired matches
      des.matches = case_when(
        cons.up.weeks < 4 ~ des.matches,
        TRUE ~ pmax(1, des.matches-1)
      )
    ) ->
    df
}