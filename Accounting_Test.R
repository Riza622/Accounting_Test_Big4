# Load libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(gt)
library(patchwork)
library(fixest)

# Import data
df <- read.csv("/Users/riza/Documents/GitHub/BergerRPtaskRepo/bankauditors.csv", stringsAsFactors = FALSE)
getwd() # .TEX and .PDF files will be saved to working directory

##### Part1: Exploratory Data Analysis

### Dataset overview
head(df)
colnames(df)
str(df)
dim(df)
summary(df)
colSums(is.na(df))

# Key stats
n_banks <- n_distinct(df$bankId)
n_auditors <- n_distinct(df$auditorname)
n_obs <- nrow(df)

# Public/non-public split
df %>%
  group_by(public) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count / sum(count) * 100, 1))

# Banks with full data
n_banks_full_hist <- df %>%
  filter(year >= 2005 & year <= 2015) %>%
  group_by(bankId) %>%
  summarise(n_years = n_distinct(year)) %>%
  filter(n_years == 11) %>%
  nrow()

# Disappear after 2008
last_year_of_bank <- df %>%
  group_by(bankId) %>%
  summarise(last_year = max(year))

n_banks_last_year2007 <- last_year_of_bank %>% filter(last_year == 2007) %>% nrow() # 45
n_banks_last_year2008 <- last_year_of_bank %>% filter(last_year == 2008) %>% nrow() # 36
n_banks_last_year2009 <- last_year_of_bank %>% filter(last_year == 2009) %>% nrow() # 55

# Changed audit firm
n_banks_changed_auditor <- df %>%
  group_by(bankId) %>%
  summarise(n_aud = n_distinct(auditorname)) %>%
  filter(n_aud > 1) %>%
  nrow()

# Changed public status
n_banks_changed_public <- df %>%
  group_by(bankId) %>%
  summarise(n_status = n_distinct(public)) %>%
  filter(n_status > 1) %>%
  nrow()

# Assets summary
bankassets_stats <- df %>%
  group_by(public) %>%
  summarise(mean_assets = mean(totalassets, na.rm = TRUE),
            median_assets = median(totalassets, na.rm = TRUE))




### auditorname patterns

# all names start with a letter, except for ", LAIRSON, CPAS" and "#NAME?", these are Non-Big4
df |>
  filter(!str_starts(auditorname, regex("^[A-Za-z]")) | bankId==1103878 ) |>
  summarise(non_letter_count = n())
# 
# bankId 1103878 has ", LAIRSON, CPAS" and "#NAME?" values in auditorname in 2006-11
# in other also used Non-Big4
# https://www.ibanknet.com/scripts/callreports/getbank.aspx?ibnid=usa_1103878

df_baddata <- df |> filter(!str_starts(auditorname, regex("^[A-Za-z]")) | bankId==1103878 ) |> select(c('bankId','bankname','auditorname'))
# apply LaTeX formatting to the third column that contains bad characters

df_baddata[[3]] <- paste0("\\verb|", df_baddata[[3]], "|")

df_baddata_tex <- datasummary_df(df_baddata, 
               output = "df_baddata.tex",
               title = "Example of Incomplete Data on audutorname"
               )
# change table option
df_baddata_tex <- sub("\\\\begin\\{table\\}", "\\\\begin{table}[h]", df_baddata_tex)
# add a line to change LaTeX font size
df_baddata_tex <- sub(
  pattern = "(\\\\begin\\{table\\}.*?\n)",
  replacement = paste0("\\1", "\\\\footnotesize", "\n"),
  x = df_baddata_tex
)
writeLines(df_baddata_tex, "df_baddata.tex")


### Big4 name patterns

auditfirm_names <- df |> distinct(auditorname = toupper(auditorname)) # this returns only 1265 unique firms
auditfirm_names <- auditfirm_names |> arrange(auditfirm_names)
deloitte_patters <- auditfirm_names[309:342,]
ernst_patters <- auditfirm_names[c(392:393,421:451),]
kpmg_patters <- auditfirm_names[c(631,645:646,648:664,724),] 
pwc_patters <- auditfirm_names[c(954:983, 987),]

df <- df |> 
  mutate(
    big4 = case_when(
      toupper(auditorname) %in% deloitte_patters ~ 1,
      toupper(auditorname) %in% kpmg_patters ~ 1,
      toupper(auditorname) %in% pwc_patters ~ 1,
      toupper(auditorname) %in% ernst_patters ~ 1,
      TRUE ~ 0),
    big4_firm = case_when(
      toupper(auditorname) %in% deloitte_patters ~ "Deloitte",
      toupper(auditorname) %in% kpmg_patters ~ "KPMG",
      toupper(auditorname) %in% pwc_patters ~ "PwC",
      toupper(auditorname) %in% ernst_patters ~ "Ernst & Young",
      TRUE ~ "NonBig4"),
    isBig4 = ifelse(big4 == 1, "Big4", "NotBig4")
  )

big4patterns <- list('Deloitte' = deloitte_patters, 'Ernst & Young'=ernst_patters, 'KPMG'=kpmg_patters, 'PwC'=pwc_patters)
big4patterns_max_len <- max(lengths(big4patterns))

# Pad each vector to max length
big4patterns_padded <- lapply(big4patterns, function(x) {
  length(x) <- big4patterns_max_len  # adds NAs if needed
  return(x)
})
big4patterns_df <- as.data.frame(big4patterns_padded)

big4patterns_df[[1]] <- paste0("\\verb|", big4patterns_df[[1]], "|")
big4patterns_df[[2]] <- paste0("\\verb|", big4patterns_df[[2]], "|")
big4patterns_df[[3]] <- paste0("\\verb|", big4patterns_df[[3]], "|")
big4patterns_df[[4]] <- paste0("\\verb|", big4patterns_df[[4]], "|")


big4patterns_df_tex <- datasummary_df(big4patterns_df, output = "latex", title = "Big4 name patterns")


# add a line to change LaTeX font size
big4patterns_df_tex <- sub(
  pattern = "(\\\\begin\\{table\\}.*?\n)",
  replacement = paste0("\\1", "\\\\scriptsize", "\n"),
  x = big4patterns_df_tex
)

# change table option
big4patterns_df_tex <- sub("\\\\begin\\{table\\}", "\\\\begin{table\\}[h]", big4patterns_df_tex)

# save to disk, to compile into LaTeX
writeLines(big4patterns_df_tex, "include/big4patterns_df.tex")



# Big4 switches
n_big4_switches <- df %>%
  group_by(bankId) %>%
  summarise(big4_statuses = n_distinct(isBig4)) %>%
  filter(big4_statuses > 1) %>%
  nrow()


# Summary Table
eda_summary <- tibble(
  Metric = c(
    "Total Observations",
    "Unique bankIds",
    "Unique Audit Firms names (inlcuding variations)",
    "Banks with Full Data (2005–2015)",
    "Banks Disappeared After 2007",
    "Banks Disappeared After 2008",
    "Banks Disappeared After 2009",
    "Banks Changed Audit Firm",
    "Banks Changed Public Status",
    "Mean Assets (Public)",
    "Mean Assets (Non-Public)",
    "Median Assets (Public)",
    "Median Assets (Non-Public)",
    "Banks Switched Between Big4 and Non-Big4"
  ),
  Value = c(
    n_obs,
    n_banks,
    n_auditors,
    n_banks_full_hist,
    n_banks_last_year2007,
    n_banks_last_year2008,
    n_banks_last_year2009,
    n_banks_changed_auditor,
    n_banks_changed_public,
    bankassets_stats$mean_assets[bankassets_stats$public == 1],
    bankassets_stats$mean_assets[bankassets_stats$public == 0],
    bankassets_stats$median_assets[bankassets_stats$public == 1],
    bankassets_stats$median_assets[bankassets_stats$public == 0],
    n_big4_switches
  )
) 

eda_summary_tex <- datasummary_df(eda_summary, output = "include/eda_summary.tex", title = "Exploratory Data Analysis Summary")
# add a line to change LaTeX font size
eda_summary_tex <- sub(
  pattern = "(\\\\begin\\{table\\}.*?\n)",
  replacement = paste0("\\1", "\\\\footnotesize", "\n"),
  x = eda_summary_tex
)
# change table option
eda_summary_tex <- sub("\\\\begin\\{table\\}", "\\\\begin{table\\}[h]", eda_summary_tex)
# save to disk, to compile into LaTeX
writeLines(eda_summary_tex, "include/eda_summary.tex")



# Table for bank history lengths
bank_years_distribution <- df %>%
  group_by(bankId) %>%
  summarise(num_years = n_distinct(year)) %>%
  count(num_years) %>%
  rename(`Num Years` = num_years, `Bank Count` = n)

bank_years_distribution_tex <- datasummary_df(bank_years_distribution, output = "include/bank_years_distribution.tex", title = "Bank History Distribution")
# add a line to change LaTeX font size
bank_years_distribution_tex <- sub(
  pattern = "(\\\\begin\\{table\\}.*?\n)",
  replacement = paste0("\\1", "\\\\footnotesize", "\n"),
  x = bank_years_distribution_tex
)
# change table option
bank_years_distribution_tex <- sub("\\\\begin\\{table\\}", "\\\\begin{table\\}[h]", bank_years_distribution_tex)
# save to disk, to compile into LaTeX
writeLines(bank_years_distribution_tex, "include/bank_years_distribution.tex")






### Question 1: Publicly-listed status

# Calculate Big4 share by public status
big4_public_share <- df %>%
  group_by(public) %>%
  summarise(big4_share = mean(big4, na.rm = TRUE))

# Calculate Big4 share by public status and year
big4_public_year_share <- df %>%
  group_by(year, public) %>%
  summarise(big4_share = mean(big4, na.rm = TRUE), .groups = "drop")

# Plot 1: Share of Banks Hiring Big4
p1 <- ggplot(big4_public_share, aes(x = factor(public), y = big4_share, fill = factor(public))) +
  geom_col(width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels = c("0" = "NonPublic", "1" = "Public")) +
  scale_fill_manual(values = c("0" = "#F8766D", "1" = "#00BFC4")) +
  labs(title = "Share of Banks Hiring Big4", x = "Listing Status", y = "Share") +
  theme_minimal()

# Plot 2: Trend of Big4 Usage by Year
p2 <- ggplot(big4_public_year_share, aes(x = factor(year), y = big4_share, fill = factor(public))) +
  geom_col(position = position_dodge(), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("0" = "#F8766D", "1" = "#00BFC4")) +
  labs(title = "Trend of Big4 Usage by Year", x = "Year", y = "Share") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine both plots
combined_plot <- p1 + p2 + patchwork::plot_layout(ncol = 2)
print(combined_plot)

# Save combined_plot as PDF
ggsave("figure1_combined_big4_share.pdf", plot = combined_plot, width = 12, height = 5, units = "in")



















#================================= start



# Load required libraries
library(fixest)
library(broom)
library(modelsummary)
library(dplyr)

# # Rescale assets for readability
# df <- df %>%
#   mutate(totalassets_mil = totalassets / 1e6)
# 
# # Logistic Regressions
# logit_model1 <- glm(big4 ~ public + totalassets_mil, data = df, family = binomial)
# logit_model2 <- glm(big4 ~ public + totalassets_mil + factor(bankstate) + factor(year), data = df, family = binomial)
# logit_model3 <- glm(big4 ~ public + totalassets_mil + factor(bankstate) + factor(year) + public:factor(year), 
#                     data = df, family = binomial)
# 
# # Linear Regressions with clustered standard errors
# lin_model1 <- feols(big4 ~ public + totalassets_mil, data = df, cluster = ~bankId)
# lin_model2 <- feols(big4 ~ public + totalassets_mil + factor(bankstate) + factor(year), data = df, cluster = ~bankId)
# lin_model3 <- feols(big4 ~ public + totalassets_mil + public:factor(year) + factor(bankstate) + factor(year), 
#                     data = df, cluster = ~bankId)
# 
# # Print the detailed summary of logistic models
# summary(logit_model1)
# summary(logit_model2)
# summary(logit_model3)
# 
# # Print the detailed summary of linear models
# summary(lin_model1)
# summary(lin_model2)
# summary(lin_model3)
# 
# # Model summary
# logit_results <- list(logit_model1, logit_model2, logit_model3)
# lin_results <- list(lin_model1, lin_model2, lin_model3)
# 


# Use modelsummary to generate the tables
logit_table <- modelsummary(logit_results, output = "latex", stars = TRUE)
lin_table <- modelsummary(lin_results, output = "latex", stars = TRUE)

# Linear Regressions with clustered standard errors
lin_model1 <- feols(
  big4 ~ public + I(totalassets/1e6),
  data = df, cluster = ~bankId
)

lin_model2 <- feols(
  big4 ~ public + I(totalassets/1e6) + factor(bankstate),
  data = df, cluster = ~bankId
)

lin_model3 <- feols(
  big4 ~ public + I(totalassets/1e6) | bankId,
  data = df, cluster = ~bankId
)

logit_model1 <- feglm(
  big4 ~ public + I(totalassets/1e6),
  data = df, cluster = ~bankId,
  family = binomial("logit")
)

logit_model2 <- feglm(
  big4 ~ public + I(totalassets/1e6) + factor(bankstate),
  data = df, cluster = ~bankId,
  family = binomial("logit")
)

logit_model3 <- feglm(
  big4 ~ public + I(totalassets/1e6) | bankId,
  data = df, cluster = ~bankId,
  family = binomial("logit")
)
# NOTE: 971 fixed-effects (6,554 observations) removed because of only 0 (or only 1) outcomes.
# note that most banks do not switch in/out of Big4, so there is not much variation in the data to estimate the FE models


modelsummary(
  list(
    "-FE" = lin_model1,"-FE +State" = lin_model2, "+FE" = lin_model3,
    "-FE" = logit_model1,"-FE +State" = logit_model2, "+FE" = logit_model3
  ),
  coef_omit = "bankstate|factor",
  gof_omit = "AIC|BIC|R2 Within",
  output = "include/public_regressions.tex",
  stars = TRUE,
  title = "Propensity of hiring Big4"
)

#================ stop

# Proportion of Banks Using Each Big4 Auditor
df_summary_by_firm %>%
  filter(big4_firm != "Not Big4") %>%
  mutate(public_label = ifelse(public == 1, "Public", "NonPublic")) %>%
  ggplot(aes(x = factor(year), y = share, group = public_label, color = public_label)) +
  geom_line() +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(
    name = "Listing Status",
    values = c("NonPublic" = "#F8766D", "Public" = "#00BFC4"),
    labels = c("NonPublic", "Public")
  ) +
  labs(
    title = "Proportion of Banks Using Each Big4",
    x     = "Year",
    y     = "Proportion of Banks"
  ) +
  facet_wrap(~ big4_firm, nrow = 1) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) 


### Question 2:market share 

library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)

# Big4 market share
big4_market_share_by_year <- df %>%
  group_by(year) %>%
  summarise(
    total_assets_big4 = sum(totalassets[big4 == 1], na.rm = TRUE),
    total_assets_all  = sum(totalassets, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(market_share_big4 = total_assets_big4 / total_assets_all)

# Total assets by group
assets_trend <- df %>%
  group_by(year, big4) %>%
  summarise(total_assets = sum(totalassets, na.rm = TRUE), .groups = "drop") %>%
  mutate(auditor_group = if_else(big4 == 1, "Big4", "Non-Big4"))

# Annual total assets (used to unify the y axis in the left/middle figure)
total_assets_by_year <- df %>%
  group_by(year) %>%
  summarise(total_assets = sum(totalassets, na.rm = TRUE), .groups = "drop")
max_bar <- max(total_assets_by_year$total_assets)

# Flow data
flow_summary <- df %>%
  arrange(bankId, year) %>%
  group_by(bankId) %>%
  mutate(prev_big4 = lag(big4)) %>%
  ungroup() %>%
  filter(!is.na(prev_big4)) %>%
  group_by(year) %>%
  summarise(
    assets_entered = sum(totalassets[prev_big4 == 0 & big4 == 1], na.rm = TRUE),
    assets_exited  = sum(totalassets[prev_big4 == 1 & big4 == 0], na.rm = TRUE),
    .groups = "drop"
  )

# === Figure 1: Total assets + market share line ===
p1 <- ggplot() +
  geom_col(
    data = assets_trend,
    aes(x = factor(year), y = total_assets, fill = auditor_group),
    position = position_dodge(width = 0.7), width = 0.6
  ) +
  geom_line(
    data = big4_market_share_by_year,
    aes(x = factor(year), y = market_share_big4 * max_bar, group = 1),
    color = "#00BFC4", linewidth = 1.2, linetype = "dashed"
  ) +
  geom_point(
    data = big4_market_share_by_year,
    aes(x = factor(year), y = market_share_big4 * max_bar),
    color = "#00BFC4", size = 2
  ) +
  scale_fill_manual(values = c("Big4" = "#00BFC4", "Non-Big4" = "#F8766D")) +
  scale_y_continuous(
    name = "Total Assets (USD)",
    labels = comma_format(),
    limits = c(0, max_bar),
    sec.axis = sec_axis(
      trans = ~ . / max_bar,
      name = "Big4 Market Share",
      labels = percent_format()
    )
  ) +
  scale_x_discrete(breaks = as.character(seq(2005, 2015, 1))) +
  labs(
    title = "Total Assets by Auditor & Big4 Market Share",
    x     = "Year",
    fill  = "Auditor Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y.right = element_text(color = "#00BFC4"),
    axis.text.y.right = element_text(color = "#00BFC4"),
    legend.position = "bottom", 
    legend.title = element_blank()
  )

#Figure 2: Assets entering/exiting Big4 (uniform high Y-axis) ===
p2 <- ggplot(flow_summary, aes(x = factor(year))) +
  geom_col(aes(y = assets_entered, fill = "Enter"),
           width = 0.4, position = position_nudge(x = -0.2)) +
  geom_col(aes(y = assets_exited, fill = "Exit"),
           width = 0.4, position = position_nudge(x = 0.2)) +
  scale_fill_manual(
    name = "Flow Type",
    values = c("Enter" = "#00BFC4", "Exit" = "#F8766D")
  ) +
  scale_y_continuous(
    name = "Assets (USD)",
    labels = comma_format(),
    limits = c(0, max_bar)
  ) +
  scale_x_discrete(breaks = as.character(seq(2005, 2015, 1))) +
  labs(
    title = "Asset Flow Into and Out of Big4",
    x     = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# === Figure 3: Asset entering/exiting Big4 (scaling) ===
p3 <- ggplot(flow_summary, aes(x = factor(year))) +
  geom_col(aes(y = assets_entered, fill = "Enter"),
           width = 0.4, position = position_nudge(x = -0.2)) +
  geom_col(aes(y = assets_exited, fill = "Exit"),
           width = 0.4, position = position_nudge(x = 0.2)) +
  scale_fill_manual(
    name = "Flow Type",
    values = c("Enter" = "#00BFC4", "Exit" = "#F8766D")
  ) +
  scale_y_continuous(
    name = "Assets (USD)",
    labels = comma_format()
  ) +
  scale_x_discrete(breaks = as.character(seq(2005, 2015, 1))) +
  labs(
    title = "Asset Flow Into and Out of Big4",
    x     = "Year"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

p_combined <- p1 + p2 + p3 + plot_layout(ncol = 3)

print(p_combined)


#Total Asset Flow Between Auditor Groups

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(patchwork)

auditor_colors <- c(
  "Deloitte" = "#1b9e77",
  "PwC" = "#d95f02",
  "KPMG" = "#7570b3",
  "Ernst & Young" = "#e7298a",
  "NonBig4" = "#999999"
)

# Line chart: Changes in total assets of each audit group over the years
firm_total_assets_by_year <- df %>%
  group_by(year, big4_firm) %>%
  summarise(total_assets = sum(totalassets, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(firm_total_assets_by_year, aes(x = factor(year), y = total_assets / 1e6, color = big4_firm, group = big4_firm)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  scale_color_manual(values = auditor_colors) +
  scale_y_continuous(labels = comma_format(suffix = "M")) +
  labs(
    title = "Total Assets by Auditor Group (2005–2015)",
    x = "Year",
    y = "Total Assets (Million USD)",
    color = "Auditor Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Heat map: Asset flows between different audit groups (changes from year to year, combined for all years)
df_movement <- df %>%
  arrange(bankId, year) %>%
  group_by(bankId) %>%
  mutate(prev_firm = lag(big4_firm)) %>%
  ungroup() %>%
  filter(!is.na(prev_firm) & prev_firm != big4_firm) 

# Summary Conversion matrix (million USD)
transition_matrix <- df_movement %>%
  group_by(prev_firm, big4_firm) %>%
  summarise(total_asset_flow = sum(totalassets, na.rm = TRUE) / 1e6, .groups = "drop")

all_firms <- unique(df$big4_firm)
complete_matrix <- expand.grid(prev_firm = all_firms, big4_firm = all_firms) %>%
  left_join(transition_matrix, by = c("prev_firm", "big4_firm")) %>%
  mutate(total_asset_flow = replace_na(total_asset_flow, 0))

p2 <- ggplot(complete_matrix, aes(x = big4_firm, y = prev_firm, fill = total_asset_flow)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(total_asset_flow, 1)), size = 4) +
  scale_fill_gradient(low = "#F0F9E8", high = "#00BFC4") +
  labs(
    title = "Total Asset Flow Between Auditor Groups (2005–2015)",
    x = "New Auditor Group",
    y = "Previous Auditor Group",
    fill = "Assets Flowed (Million USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p1 + p2 + plot_layout(ncol = 2)


# Big4 Market Share Decomposition

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

# Construct a lag variable
df_sorted <- df %>%
  arrange(bankId, year) %>%
  group_by(bankId) %>%
  mutate(
    prev_assets = lag(totalassets),
    prev_big4   = lag(big4),
    prev_year   = lag(year)
  ) %>%
  ungroup()

# Keep only consecutive year pairs
df_year_pairs <- df_sorted %>%
  filter(!is.na(prev_assets), !is.na(prev_big4), year == prev_year + 1)

# All years broken down
years <- sort(unique(df$year))
years <- years[years != min(years)]  

decomp_list <- lapply(years, function(yr) {
  t <- yr
  t1 <- yr - 1
  
  big4_t1 <- df %>% filter(year == t1, big4 == 1)
  big4_t  <- df %>% filter(year == t , big4 == 1)
  
  # Line 1: Total asset change
  total_change <- sum(big4_t$totalassets, na.rm = TRUE) - sum(big4_t1$totalassets, na.rm = TRUE)
  
  # Status matching table
  merged <- full_join(
    big4_t1 %>% select(bankId, assets_t1 = totalassets),
    big4_t  %>% select(bankId, assets_t = totalassets),
    by = "bankId"
  ) %>%
    mutate(
      status = case_when(
        !is.na(assets_t1) & !is.na(assets_t) ~ "stay",
        is.na(assets_t1) & !is.na(assets_t) ~ "entry_new",
        !is.na(assets_t1) & is.na(assets_t) ~ "exit_dropout"
      ),
      asset_change = coalesce(assets_t, 0) - coalesce(assets_t1, 0)
    )
  
  within_change <- merged %>% filter(status == "stay") %>%
    summarise(val = sum(asset_change, na.rm = TRUE)) %>% pull()
  
  entry_switch <- df_year_pairs %>%
    filter(year == t, prev_big4 == 0, big4 == 1) %>%
    summarise(val = sum(totalassets, na.rm = TRUE)) %>% pull()
  
  exit_switch <- df_year_pairs %>%
    filter(year == t, prev_big4 == 1, big4 == 0) %>%
    summarise(val = sum(prev_assets, na.rm = TRUE)) %>% pull() * -1
  
  entry_new <- merged %>% filter(status == "entry_new") %>%
    summarise(val = sum(assets_t, na.rm = TRUE)) %>% pull()
  
  exit_dropout <- merged %>% filter(status == "exit_dropout") %>%
    summarise(val = sum(assets_t1, na.rm = TRUE)) %>% pull() * -1
  
  tibble(
    year = yr,
    total_change = total_change,
    within_change = within_change,
    entry_switch = entry_switch,
    exit_switch = exit_switch,
    entry_new = entry_new,
    exit_dropout = exit_dropout
  )
})

# Merge to summary table
decomp_df <- bind_rows(decomp_list) %>%
  mutate(
    sum_components = within_change + entry_switch + exit_switch + entry_new + exit_dropout,
    diff = total_change - sum_components
  )

#The long format is used to draw six component lines
decomp_long <- decomp_df %>%
  select(year, within_change, entry_switch, exit_switch, entry_new, exit_dropout) %>%
  pivot_longer(cols = -year, names_to = "Component", values_to = "Change")

p1 <- ggplot(decomp_long, aes(x = factor(year), y = Change / 1e6, color = Component, group = Component)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "within_change" = "#1b9e77",
      "entry_switch" = "#d95f02",
      "exit_switch" = "#7570b3",
      "entry_new" = "#e7298a",
      "exit_dropout" = "#66a61e"
    ),
    labels = c(
      "within_change" = "Growth of Existing Big4",
      "entry_switch" = "Entered from Non-Big4",
      "exit_switch" = "Left to Non-Big4",
      "entry_new" = "Newly Created Big4",
      "exit_dropout" = "Exited Big4 & Market"
    )
  ) +
  scale_y_continuous(labels = comma_format(suffix = "M")) +
  labs(
    title = "Decomposition of Big4 Asset Changes (6 Lines)",
    x = "Year", y = "Change in Big4 Assets (Million USD)",
    color = "Component"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

p1

stack_colors <- c(
  "within_change" = "#66c2a5",   # green
  "entry_switch" = "#fc8d62",    # orange
  "exit_switch"  = "#8da0cb",    # blue
  "entry_new"    = "#e78ac3",    # pink
  "exit_dropout" = "#a6d854"     # lime green
)

p1_stack <- ggplot(decomp_long, aes(x = factor(year), y = Change / 1e6, fill = Component)) +
  geom_col(position = "stack", width = 0.7) +
  geom_line(
    data = decomp_df,
    aes(x = factor(year), y = total_change / 1e6, group = 1),
    inherit.aes = FALSE,
    color = "#1f78b4",
    size = 1.4
  ) +
  geom_point(
    data = decomp_df,
    aes(x = factor(year), y = total_change / 1e6),
    inherit.aes = FALSE,
    color = "#1f78b4",
    size = 2.5
  ) +
  scale_fill_manual(
    values = stack_colors,
    labels = c(
      "within_change" = "Growth of Existing Big4",
      "entry_switch" = "Entered from Non-Big4",
      "exit_switch" = "Left to Non-Big4",
      "entry_new" = "Newly Created Big4",
      "exit_dropout" = "Exited Big4 & Market"
    )
  ) +
  scale_y_continuous(labels = comma_format(suffix = "M")) +
  labs(
    title = "Big4 Asset Change Decomposition (Stacked Bars + Total Line)",
    x = "Year", y = "Change in Big4 Assets (Million USD)",
    fill = "Component",
    caption = "Line = Total Change in Big4 Assets"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 10)
  )

p1_stack

#question3 : Spatial variation

#Total Bank Assets by State

library(dplyr)
library(ggplot2)
library(scales)
library(maps)
library(sf)
library(purrr)
library(tidyr)

us_map <- map_data("state")

# Build polygon for each state
us_map_polygons <- us_map %>%
  group_by(region, group) %>%
  nest() %>%
  mutate(
    geometry = map(data, ~ st_polygon(list(as.matrix(select(.x, long, lat)))))
  ) %>%
  ungroup()

# Building sf objects
us_map_sf <- st_sf(
  region = us_map_polygons$region,
  group = us_map_polygons$group,
  geometry = st_sfc(us_map_polygons$geometry),
  crs = 4326
)

# Merge with asset data
df_state_assets <- df %>%
  mutate(state_name = tolower(state.name[match(bankstate, state.abb)])) %>%
  filter(!is.na(state_name)) %>%
  group_by(state_name) %>%
  summarise(total_assets = sum(totalassets, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    total_assets_bil = total_assets / 1e9,
    region = state_name
  )

# Merge map and data
map_with_assets <- left_join(us_map, df_state_assets, by = "region")

# Compute label coordinates
state_labels <- map_with_assets %>%
  group_by(region) %>%
  summarise(
    long = mean(range(long), na.rm = TRUE),
    lat = mean(range(lat), na.rm = TRUE),
    assets = mean(total_assets_bil, na.rm = TRUE)
  ) %>%
  mutate(
    abbr = state.abb[match(str_to_title(region), state.name)],
    label = paste0(abbr, "\n$", round(assets, 1), "B")
  ) %>%
  filter(!is.na(abbr))

ggplot() +
  geom_polygon(
    data = map_with_assets,
    aes(x = long, y = lat, group = group, fill = total_assets_bil),
    color = "white"
  ) +
  geom_text(
    data = state_labels,
    aes(x = long, y = lat, label = label),
    color = "white", size = 2, fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#56B1F7", high = "#132B43",
    labels = dollar_format(suffix = "B"),
    name = "Total Assets"
  ) +
  coord_fixed(1.3) +
  labs(
    title = "Total Bank Assets by State",
    subtitle = "With State Abbreviation and Assets (in Billions)",
    caption = "Source: Bank Auditors Dataset"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )


#Big4 Market Share of Total Bank Assets by State

library(dplyr)
library(ggplot2)
library(scales)
library(maps)
library(stringr)

# Add state name (to match map)
df <- df %>%
  mutate(
    state_name = tolower(state.name[match(bankstate, state.abb)])
  )

# Calculate Big4 market share for each state (weighted by assets)
df_state <- df %>%
  filter(!is.na(state_name)) %>%
  group_by(state_name) %>%
  summarise(
    total_assets = sum(totalassets, na.rm = TRUE),
    big4_assets = sum(totalassets[big4 == 1], na.rm = TRUE),
    big4_share_state = big4_assets / total_assets,
    .groups = "drop"
  ) %>%
  rename(region = state_name)

# Get US map data and merge
us_states <- map_data("state")
df_map <- left_join(us_states, df_state, by = "region")

# Use the state center point data in the maps package as the label location
state_centers <- data.frame(
  region = tolower(state.name),
  long = state.center$x,
  lat = state.center$y,
  abbr = state.abb
)

# Merge tag information
state_labels <- df_state %>%
  left_join(state_centers, by = "region") %>%
  mutate(
    label = paste0(abbr, "\n", round(big4_share_state * 100, 1), "%")
  ) %>%
  filter(!is.na(label))

ggplot() +
  geom_polygon(
    data = df_map,
    aes(x = long, y = lat, group = group, fill = big4_share_state),
    color = "white"
  ) +
  geom_text(
    data = state_labels,
    aes(x = long, y = lat, label = label),
    color = "white", size = 2.7, fontface = "bold", lineheight = 0.9
  ) +
  scale_fill_gradient(
    low = "lightblue", high = "darkblue",
    labels = percent_format(accuracy = 1),
    name = "Big4 Share"
  ) +
  coord_fixed(1.3) +
  labs(
    title = "Big4 Market Share of Total Bank Assets by State",
    subtitle = "Labeled with State Abbreviations and Share (%)",
    caption = "Source: Bank Auditors Dataset"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )



#Big4 Market Share by State

# bar chart
library(dplyr)
library(ggplot2)
library(scales)

# Organize data
df_state <- df %>%
  mutate(state_name = tolower(state.name[match(bankstate, state.abb)])) %>%
  filter(!is.na(state_name)) %>%
  group_by(state_name) %>%
  summarise(
    total_assets = sum(totalassets, na.rm = TRUE),
    big4_assets = sum(totalassets[big4 == 1], na.rm = TRUE),
    big4_share_state = big4_assets / total_assets,
    .groups = "drop"
  ) %>%
  rename(region = state_name) %>%
  mutate(
    abbr = state.abb[match(str_to_title(region), state.name)]
  ) %>%
  filter(!is.na(abbr))

# Draw Bar Chart
ggplot(df_state, aes(x = reorder(abbr, big4_share_state), y = big4_share_state)) +
  geom_col(fill = "darkblue") +
  geom_text(
    aes(label = paste0(round(big4_share_state * 100, 1), "%")),
    vjust = 1.2,  
    color = "white",
    fontface = "bold",
    size = 2
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.05)  
  ) +
  labs(
    title = "Big4 Market Share by State",
    x = "State",
    y = "Big4 Share of Total Bank Assets"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#Distribution by Each Big4 Auditor

library(dplyr)
library(ggplot2)
library(maps)
library(stringr)
library(scales)

#Tag the Big4 company and state name
df <- df %>%
  mutate(
    auditor_lower = tolower(auditorname),
    big4_firm = case_when(
      str_detect(auditor_lower, "\\bkpmg\\b") ~ "KPMG",
      str_detect(auditor_lower, "ernst|\\bey\\b") ~ "Ernst & Young",
      str_detect(auditor_lower, "\\bdeloitte\\b") ~ "Deloitte",
      str_detect(auditor_lower, "price|pwc") ~ "PwC",
      TRUE ~ NA_character_
    ),
    state_name = tolower(state.name[match(bankstate, state.abb)])
  )

# Define the Big4 list
big4_firms <- c("Deloitte", "Ernst & Young", "KPMG", "PwC")

# Calculate the total assets of each Big4 in each state
firm_state_assets <- df %>%
  filter(big4_firm %in% big4_firms, !is.na(state_name)) %>% 
  group_by(big4_firm, state_name) %>%
  summarise(total_assets = sum(totalassets, na.rm = TRUE), .groups = "drop") %>%
  rename(region = state_name)

# Merge map data
us_states <- map_data("state")
map_data_firm <- left_join(us_states, firm_state_assets, by = "region") %>%
  filter(!is.na(big4_firm))  

ggplot(map_data_firm, aes(x = long, y = lat, group = group, fill = total_assets)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(
    low = "#cce5ff", high = "#003366",
    labels = label_number(scale_cut = cut_short_scale()),
    name = "Total Assets"
  ) +
  coord_fixed(1.3) +
  labs(
    title = "Total Bank Assets by State",
    subtitle = "Distribution by Each Big4 Auditor",
    caption = "Source: Bank Auditors Dataset"
  ) +
  facet_wrap(~ big4_firm, ncol = 2) +
  theme_void(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    legend.position = "right"
  )



#Total Bank Assets by public_status

library(dplyr)
library(ggplot2)
library(maps)
library(scales)

# # State name field 
df <- df %>%
  mutate(state_name = tolower(state.name[match(bankstate, state.abb)]))

# NA was filtered out and classified for statistics
df_public_assets <- df %>%
  filter(!is.na(state_name), !is.na(public)) %>%
  mutate(public_status = ifelse(public == 1, "Public", "Non-Public")) %>%
  group_by(state_name, public_status) %>%
  summarise(total_assets = sum(totalassets, na.rm = TRUE), .groups = "drop") %>%
  rename(region = state_name)

# Load map data and merge
us_states <- map_data("state")
map_data_public <- left_join(us_states, df_public_assets, by = "region") %>%
  filter(!is.na(public_status))

ggplot(map_data_public, aes(x = long, y = lat, group = group, fill = total_assets)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(
    low = "#e0f3db", high = "#0868ac",
    labels = label_number(scale_cut = cut_short_scale()),
    name = "Total Assets"
  ) +
  coord_fixed(1.3) +
  facet_wrap(~ public_status, ncol = 2) +
  labs(
    title = "Total Bank Assets by State",
    caption = "Source: Bank Auditors Dataset"
  ) +
  theme_void(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    legend.position = "right"
  )



# Data Manipulation
# Filter to last year
last_year <- max(df$year, na.rm = TRUE)
df_last   <- df %>% filter(year == last_year, !is.na(bankstate))

# Compute total assets across all banks (for ratiototal)
total_assets_all <- sum(df_last$totalassets, na.rm = TRUE)

# Aggregate by state
df_state_summary <- df_last %>%
  group_by(bankstate) %>%
  summarise(
    nbanks       = n_distinct(bankId),
    npublic      = n_distinct(bankId[public == 1]),
    totass       = sum(totalassets, na.rm = TRUE),
    avass        = mean(totalassets, na.rm = TRUE),
    maxass       = max(totalassets, na.rm = TRUE),
    public_assets = sum(totalassets[public == 1], na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(
    ratiopublic = public_assets / totass,
    ratiototal  = totass / total_assets_all
  ) %>%
  select(-public_assets) %>%
  arrange(bankstate)

# Inspect and export
print(df_state_summary)
write.csv(df_state_summary, "bankstate_summary.csv", row.names = FALSE)
