# PS6 - Data Cleaning: 2024 MLB Home Run Leaders
# Author: [Your Last Name]
# Description: Pull, clean, and explore 2024 MLB HR leader data from MLB Stats API

# ── 1. Install / load packages ────────────────────────────────────────────────
if (!require("rjson"))   install.packages("rjson",   repos = "https://cloud.r-project.org")
if (!require("dplyr"))   install.packages("dplyr",   repos = "https://cloud.r-project.org")
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require("stringr")) install.packages("stringr", repos = "https://cloud.r-project.org")

library(rjson)
library(dplyr)
library(ggplot2)
library(stringr)

# ── 2. Pull data from MLB Stats API (same as PS5) ─────────────────────────────
url <- "https://statsapi.mlb.com/api/v1/stats/leaders?leaderCategories=homeRuns&season=2024&limit=50"
data <- fromJSON(file = url)
leaders <- data$leagueLeaders[[1]]$leaders

hr_raw <- data.frame(
  Rank       = sapply(leaders, function(x) x$rank),
  Player     = sapply(leaders, function(x) x$person$fullName),
  PlayerID   = sapply(leaders, function(x) x$person$id),
  Team       = sapply(leaders, function(x) x$team$name),
  Home_Runs  = sapply(leaders, function(x) x$value),
  stringsAsFactors = FALSE
)

cat("Raw rows:", nrow(hr_raw), "\n")
cat("Columns:", paste(names(hr_raw), collapse = ", "), "\n\n")

# ── 3. Inspect the raw data ───────────────────────────────────────────────────
print(hr_raw)

# ── 4. Fix data types ─────────────────────────────────────────────────────────
# Rank and Home_Runs come in as character from the JSON parser
hr_clean <- hr_raw %>%
  mutate(
    Rank      = as.integer(Rank),
    Home_Runs = as.integer(Home_Runs)
  )

# ── 5. Check for missing values ───────────────────────────────────────────────
missing_check <- colSums(is.na(hr_clean))
cat("Missing values per column:\n")
print(missing_check)

# ── 6. Clean player and team names ───────────────────────────────────────────
# Trim any leading/trailing whitespace that may come through the API
hr_clean <- hr_clean %>%
  mutate(
    Player = str_trim(Player),
    Team   = str_trim(Team)
  )

# ── 7. Split full name into first and last name ───────────────────────────────
hr_clean <- hr_clean %>%
  mutate(
    First_Name = word(Player, 1),
    Last_Name  = word(Player, -1)
  )

# ── 8. Add derived variables ──────────────────────────────────────────────────
# HR tier groups for visualization
hr_clean <- hr_clean %>%
  mutate(
    HR_Tier = case_when(
      Home_Runs >= 45             ~ "Elite (45+)",
      Home_Runs >= 35             ~ "Great (35-44)",
      Home_Runs >= 25             ~ "Above Avg (25-34)",
      TRUE                        ~ "Average (<25)"
    ),
    HR_Tier = factor(HR_Tier,
                     levels = c("Elite (45+)", "Great (35-44)",
                                "Above Avg (25-34)", "Average (<25)"))
  )

# ── 9. Final ordered dataset ──────────────────────────────────────────────────
hr_final <- hr_clean %>%
  select(Rank, Player, First_Name, Last_Name, Team, Home_Runs, HR_Tier) %>%
  arrange(Rank)

cat("\nFinal clean dataset:", nrow(hr_final), "players x",
    ncol(hr_final), "variables\n\n")
print(hr_final)

# ── 10. Summary statistics ────────────────────────────────────────────────────
cat("\nSummary of Home Runs:\n")
cat("  Mean:   ", round(mean(hr_final$Home_Runs), 1), "\n")
cat("  Median: ", median(hr_final$Home_Runs), "\n")
cat("  Max:    ", max(hr_final$Home_Runs), "-", hr_final$Player[1], "\n")
cat("  Min:    ", min(hr_final$Home_Runs), "\n")

# ── 11. Save clean CSV ────────────────────────────────────────────────────────
write.csv(hr_final, "mlb_hr_leaders_2024_clean.csv", row.names = FALSE)
cat("\nSaved: mlb_hr_leaders_2024_clean.csv\n")

# ── 12. Plots ─────────────────────────────────────────────────────────────────

# Plot A: Bar chart of top 20 HR leaders
top20 <- hr_final %>% filter(Rank <= 20)

p_a <- ggplot(top20, aes(x = reorder(Last_Name, Home_Runs), y = Home_Runs,
                          fill = HR_Tier)) +
  geom_col(alpha = 0.9) +
  scale_fill_manual(values = c(
    "Elite (45+)"       = "#003366",
    "Great (35-44)"     = "#336699",
    "Above Avg (25-34)" = "#6699CC",
    "Average (<25)"     = "#99BBDD"
  )) +
  coord_flip() +
  labs(title = "2024 MLB Top 20 Home Run Leaders",
       x = NULL, y = "Home Runs", fill = "Tier") +
  theme_minimal(base_size = 12)

ggsave("PS6a_LastName.png", plot = p_a, width = 7, height = 6, dpi = 150)
cat("Saved: PS6a_LastName.png\n")

# Plot B: Distribution of HR counts across all 50 leaders
p_b <- ggplot(hr_final, aes(x = Home_Runs)) +
  geom_histogram(bins = 15, fill = "#003366", color = "white", alpha = 0.85) +
  geom_vline(xintercept = mean(hr_final$Home_Runs), color = "#CC0000",
             linetype = "dashed", linewidth = 0.9) +
  annotate("text", x = mean(hr_final$Home_Runs) + 1, y = Inf, vjust = 2,
           label = paste0("Mean: ", round(mean(hr_final$Home_Runs), 1)),
           color = "#CC0000", size = 3.5, hjust = 0) +
  labs(title = "Distribution of Home Runs — 2024 Top 50 Leaders",
       x = "Home Runs", y = "Count") +
  theme_minimal(base_size = 13)

ggsave("PS6b_LastName.png", plot = p_b, width = 7, height = 5, dpi = 150)
cat("Saved: PS6b_LastName.png\n")

# Plot C: HR count by tier (count of players in each tier)
p_c <- ggplot(hr_final, aes(x = HR_Tier, fill = HR_Tier)) +
  geom_bar(alpha = 0.9) +
  scale_fill_manual(values = c(
    "Elite (45+)"       = "#003366",
    "Great (35-44)"     = "#336699",
    "Above Avg (25-34)" = "#6699CC",
    "Average (<25)"     = "#99BBDD"
  )) +
  labs(title = "Number of Players by HR Tier — 2024 Top 50 Leaders",
       x = "HR Tier", y = "Number of Players", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave("PS6c_LastName.png", plot = p_c, width = 7, height = 5, dpi = 150)
cat("Saved: PS6c_LastName.png\n")
