# Step 1: INSTALL PACKAGES
# install.packages("tidyverse")
# install.packages("fixest")
# install.packages("modelsummary")
# install.packages("lmtest")
# install.packages("sandwich")

# Step 2: LOAD PACKAGES: Every time you open RStudio
library(tidyverse)
library(fixest)
library(modelsummary)
library(lmtest)
library(sandwich)

# Step 3: LOAD THE DATASET
df <- read.csv("fedex_forum_tickets(in).csv")

# Step 4: INSPECT THE DATA
glimpse(df)
head(df)
summary(df)
dim(df)

#Step 5: CLEAN AND PREPARE THE DATA
df <- df %>%
  mutate(
    # Convert TRUE/FALSE to 1/0
    high_demand        = as.integer(high_demand),
    is_rivalry         = as.integer(is_rivalry),
    opp_star_injured   = as.integer(opp_star_injured),
    grizz_star_injured = as.integer(grizz_star_injured),
    
    # Factor variables for fixed effects
    day_of_week = factor(day_of_week),
    month       = factor(month),
    level       = factor(level, levels = c("Upper Bowl", "Lower Bowl",
                                           "Club", "Courtside")),
    
    # Log prices
    log_price_start = log(price_season_start),
    log_price_2wk   = log(price_2wk_before),
    log_price_tip   = log(price_tipoff),
    
    # Log price changes
    delta_2wk = log_price_2wk - log_price_start,
    delta_tip = log_price_tip - log_price_2wk
  )

# Step 6: VERIFY THE CLEANING WORKED
# Check 1 - should show new columns including delta_2wk, delta_tip
glimpse(df)

# Check 2 - should show small numbers roughly between -0.4 and +0.6
summary(df$delta_2wk)
summary(df$delta_tip)

# Check 3 - should show: "Upper Bowl" "Lower Bowl" "Club" "Courtisde"
levels(df$level)

# Check 4 - should show 0s and 1s
table(df$high_demand)
table(df$opp_star_injured)

# Step 7: COLLAPSE TO GAME LEVEL (41 ROWS)
game_df <- df %>%
  distinct(game_id, event_date, day_of_week, month, opponent,
           opp_preseason_ou, opp_win_pct_2wk, high_demand, is_rivalry,
           opp_star_injured, grizz_star_injured, attendance) %>%
  mutate(
    month = factor(month),
    day_of_week = factor(day_of_week)
  )

# Confirm it's exactly 41 rows
nrow(game_df)

# Step 8: Build game-level price changes
game_delta <- df %>%
  group_by(game_id) %>%
  summarise(
    mean_delta_2wk = mean(delta_2wk, na.rm = TRUE),
    mean_delta_tip = mean(delta_tip, na.rm = TRUE)
  ) %>%
  left_join(game_df, by = "game_id")

# Confirm
nrow(game_delta)  # should be 41
glimpse(game_delta)
class(game_delta$month)

# Step 1: What drives season-start price?

# Model 1A: Seat level with game fixed effects
step1_fe <-feols(
  log_price_start ~ level | game_id,
  data    = df,
  cluster = ~game_id
)

summary(step1_fe)

# Model 1B: Explicit game-level variables
step1_full <-feols(
  log_price_start ~ level + opp_preseason_ou + high_demand + is_rivalry + day_of_week + month,
  data    = df,
  cluster = ~game_id
)

summary(step1_full)

# Step 2: What causes prices to move from season-start to two weeks out?

# Fix factor variables in game_delta first
game_delta$month <- factor(game_delta$month)
game_delta$day_of_week <- factor(game_delta$day_of_week)

# Verify
class(game_delta$month)
class(game_delta$day_of_week)

# Main regression
step2 <- lm(mean_delta_2wk ~ opp_win_pct_2wk + opp_preseason_ou + high_demand + is_rivalry + opp_star_injured + grizz_star_injured + day_of_week + month, data = game_delta)

summary(step2)

coeftest(step2, vcov = vcovHC(step2, type = "HC3"))

# Step 3A: What causes prices to move from two weeks out to tipoff?
step3_game <- lm(mean_delta_tip ~ opp_win_pct_2wk + high_demand + is_rivalry + opp_star_injured + grizz_star_injured + day_of_week + month, data = game_delta)

summary(step3_game)

coeftest(step3_game, vcov = vcovHC(step3_game, type = "HC3"))

# Step 3B: Seat level with interaction terms
step3_interact <- feols(delta_tip ~ opp_win_pct_2wk + high_demand + is_rivalry + opp_star_injured + grizz_star_injured + level + high_demand:level + opp_star_injured:level, data = df, cluster = ~game_id)

summary(step3_interact)

# Step 4: What drives attendance?
step4 <- lm(attendance ~ opp_preseason_ou + opp_win_pct_2wk + high_demand + is_rivalry + opp_star_injured + grizz_star_injured + day_of_week, data = game_df)

summary(step4)

coeftest(step4, vcov = vcovHC(step4, type = "HC3"))

step4_simple <- lm(attendance ~ high_demand + opp_star_injured + grizz_star_injured + day_of_week, data = game_df)

summary(step4_simple)

# Final step: Clean output tables
# Step 1 comparison
modelsummary(
  list("Step 1A Game FE" = step1_fe,
       "Step 1B Full" = step1_full),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log|F$",
  title = "Step 1: What Drives Season-Start Price?"
)

# Steps 2 and 3 comparison
modelsummary(
  list("Step 2 Season to 2wk" = step2,
       "Step 3A 2wk to Tipoff" = step3_game,
       "Step 3B Interactions" = step3_interact),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log|F$",
  title = "Steps 2 and 3: Price Changes"
)

# Step 4 attendance
modelsummary(
  list("Step 4 Full" = step4,
       "Step 4 Simple" = step4_simple),
  stars = TRUE,
  gof_omit = "AIC|BIC|Log|F$",
  title = "Step 4: What Drives Attendance?"
)

modelsummary(list("1A Game FE" = step1_fe, "1B Full" = step1_full), stars = TRUE, output = "markdown")

modelsummary(list("Step 2" = step2, "Step 3A" = step3_game, "Step 3B" = step3_interact), stars = TRUE, output = "markdown")

modelsummary(list("Step 4 Full" = step4, "Step 4 Simple" = step4_simple), stars = TRUE, output = "markdown")



library(ggplot2)
ggplot(df, aes(x = level, y = price_tipoff, fill = level)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_discrete(limits = c("Upper Bowl", "Lower Bowl", "Club", "Courtside")) +
  labs(
    title = "Distribution of Tipoff Prices by Seating Level",
    x = "Seating Level",
    y = "Tipoff Price (USD)",
    caption = "2024-25 Memphis Grizzlies home games. All 41 games, 17,794 seats."
  ) +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("price_dist_by_level.pdf", width = 8, height = 5)
ggsave("price_dist_by_level.png", width = 8, height = 5)

