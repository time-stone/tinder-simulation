library(tidyverse)

# number of males
n.M <- 1650
# number of females
n.F <- 1350
# mean number of desired matches per week
des.matches.mean <- 7
# standard deviation of desired matches to account for individual variability
des.matches.sd <- 2
# expected success rates of likes for the first week
expected.success.rate <- 0.1
# maximum number of likes per week
max.n.given.likes <- 700

# starting situation
df.start <- data.frame(
  id = 1:(n.M+n.F),          # user id
  sex = c(
    rep("M", n.M),
    rep("F", n.F)
  ),
  des.matches = pmax(rnorm(n.M+n.F, des.matches.mean, des.matches.sd), 1),     # randomly generate number of desired matches (at least one) per week
  n.matches = NA        # number of matches
) |> 
  mutate(
    n.given.likes = round(des.matches/expected.success.rate)     # number of given likes in next week. for the first week, this is calculated from the expected success rate and the number of desired matches
  )

# IDs of females and males
id.F <- df.start$id[df.start$sex == "F"]
id.M <- df.start$id[df.start$sex == "M"]

# initiate loop
df <- df.start
res.list <- list()
week <- 1
# number of weeks to simulate
n.weeks <- 24
while (week < n.weeks) {
  
  # allocation of likes
  # males give likes randomly to women
  lapply(
    id.M,
    function(i){
      sample(
        x = id.F,
        size = df$n.given.likes[i],
        replace = F
      )
    }
  ) ->
    likes.men.to.women
  
  # females give likes randomly to men
  lapply(
    id.F,
    function(i){
      sample(
        x = id.M,
        size = df$n.given.likes[i],
        replace = F
      )
    }
  )  ->
    likes.women.to.men
  
  # stats
  # stats
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
  
  # save stats for this week
  res.list[[week]] <- df
  # next week
  week <- week+1
  # strategy for next week
  df |> 
    mutate(
      # calculate the number of likes for next week
      n.given.likes = pmin(ifelse(
        n.matches == 0,           # if no matches were received, double the number of likes
        2*n.given.likes,
        # adjust number of likes proportional to matches but no more than double
        pmin(
          round(n.given.likes/n.matches*des.matches),
          2*n.given.likes
        )
      ), max.n.given.likes)
    ) ->
    df
}

# summarise stats in final data frame
lapply(
  1:length(res.list),
  function(week) res.list[[week]] |> mutate(week = week)
) |> 
  bind_rows() ->
  df.final

# plot number of given likes per week
df.final |> 
  ggplot() +
  geom_boxplot(
    aes(
      x = sex,
      y = n.given.likes
    )
  ) +
  facet_wrap(week~.) +
  theme_bw()