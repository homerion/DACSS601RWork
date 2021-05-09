library(tidyverse)
game_data_public_STX_Sealed <- read_csv("game_data_public.STX.Sealed.csv")
raw_stx <- game_data_public_STX_Sealed
head(select(raw_stx,1:17))
# opening hand 17-359, drawn 360-702, deck 703-1046, sideboard 1047-1389
glimpse(raw_stx[1:17])

sapply(select(raw_stx,"user_win_rate_bucket"), mean, na.rm=TRUE)

raw_stx %>% 
  select("user_win_rate_bucket","user_n_games_bucket","num_mulligans") %>% 
  mutate(experience_plus_skill = case_when(
    user_win_rate_bucket > .54 & user_n_games_bucket < 50 ~ "Inexperienced Above Average",
    user_win_rate_bucket > .54 & user_n_games_bucket >= 50 ~ "Experienced Above Average",
    user_win_rate_bucket <= .54 & user_n_games_bucket < 50 ~ "Inexperienced Below Average",
    user_win_rate_bucket <= .54 & user_n_games_bucket >= 50 ~ "Experienced Below Average",
    TRUE ~ "Not Available")
           ) %>%
  ggplot(aes(experience_plus_skill, fill = factor(num_mulligans))) + geom_bar(position = "fill") +
  theme_bw()

raw_stx %>%
  select("user_win_rate_bucket","num_mulligans","won") %>% 
  ggplot(aes(num_mulligans, fill = factor(won))) + geom_bar() +
  theme_bw()

raw_stx %>%
  select("user_win_rate_bucket","num_mulligans","won") %>% 
  ggplot(aes(user_win_rate_bucket, fill = factor(num_mulligans))) + geom_bar() +
  theme_bw()

#  labs(title = "ANES Survey of 2016 Presidential Vote", y = "Number of Respondents", x = "Presidential Vote")

count(raw_stx,num_mulligans)

ggplot(es(user_win_rate_bucket)) + geom_histogram()




turns <- select(raw_stx,"num_turns")
typeof(turns)
median(turns,na.rm = TRUE)
summary(select(raw_stx,"num_turns"))

raw_stx %>% 
  select("num_turns") %>% 
  summarize_all(sd)
colMeans(turns)
summary(turns)
sapply(turns, mean, na.rm=TRUE)

ggplot(raw_stx,aes(num_turns)) + geom_histogram()

blue_decks <- raw_stx %>% 
  filter(str_detect(opp_colors,"U")) %>% 
  select("opp_colors") %>% 
  nrow()
black_decks <- raw_stx %>% 
  filter(str_detect(opp_colors,"B")) %>% 
  select("opp_colors") %>% 
  nrow()
green_decks <- raw_stx %>% 
  filter(str_detect(opp_colors,"G")) %>% 
  select("opp_colors") %>% 
  nrow()
red_decks <- raw_stx %>% 
  filter(str_detect(opp_colors,"R")) %>% 
  select("opp_colors") %>% 
  nrow()
white_decks <- raw_stx %>% 
  filter(str_detect(opp_colors,"W")) %>% 
  select("opp_colors") %>% 
  nrow()

top_ten <- raw_stx %>% 
  count(opp_colors) %>% 
  arrange(desc(n)) %>% 
  slice(1:10)



raw_stx %>% 
  count(opp_colors) %>% 
  arrange(desc(n)) %>% 
  slice(1:10) %>%
  mutate(m=n/nrow(raw_stx)*100) %>% 
  ggplot(aes(x=opp_colors,y=m)) + geom_bar(stat='identity')

color <- raw_stx %>% 
  count(opp_colors) %>% 
  arrange(desc(n)) %>% 
  mutate(m=n/nrow(raw_stx)*100)
color
ggplot(color,aes(x=opp_colors,y=n)) + geom_bar(stat='identity')
tail(color,1)[,3]
color[26,2]

df <- tibble(name = c("Mara", "Hadley"), t=c(TRUE,FALSE), x = 1:2, y = 3:4, z = 5:6)
df %>% 
  rowwise() %>% 
  mutate(sum = sum(c_across(3:4))-z+ !t)

stx <- raw_stx %>%
  rowwise() %>% 
  mutate(drawn=ifelse(on_play,
         sum(c_across(360:702))- num_turns+1,
          sum(c_across(360:702))- num_turns),
         won_int= ifelse(won,1,0))
         


sapply(select(stx,"drawn"), mean, na.rm=TRUE)
select(stx,"on_play","num_turns","drawn","won","won_int")
stx[2,360:702]
theory_count <- stx %>%
  select("won_int","drawn") %>%
  group_by(drawn) %>% 
  tally()

theory <- stx %>%
  select("won_int","drawn") %>%
  group_by(drawn) %>%
  summarize(win_rate = mean(won_int, na.rm = TRUE)) %>% 
  slice(1:32)
theory_sliced <- stx %>%
  select("won_int","drawn") %>%
  group_by(drawn) %>%
  summarize(win_rate = mean(won_int, na.rm = TRUE)) %>% 
  slice(4:24)
ggplot(theory_sliced,aes(drawn,win_rate)) + geom_point() + 
    stat_smooth(method = "lm", col = "blue")
ggplot(theory,aes(drawn,win_rate)) + geom_point() + 
  stat_smooth(method = "lm", col = "blue")
summary(lm(theory$drawn~theory$win_rate))
summary(lm(theory_sliced$drawn~theory_sliced$win_rate))
cor(theory$win_rate,theory$drawn)
cor(theory_sliced$win_rate,theory_sliced$drawn)
ggplot(stx,aes(drawn)) + geom_bar()
stx %>%
  select("won","drawn") %>%
  ggplot(aes(drawn, fill=won)) + geom_bar(position="fill")
hi <- theory_count %>%
  slice(25:32) %>%
  select("n") %>% 
  colSums()
everything <- theory_count %>%
  select("n") %>% 
  colSums()
everything
hi[[1]]/nrow(stx)*100

