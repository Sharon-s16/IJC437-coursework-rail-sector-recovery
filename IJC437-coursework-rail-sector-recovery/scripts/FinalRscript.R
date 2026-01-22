getwd()
#setwd("C:/Users/gowth/OneDrive/Desktop/IJC437_Rail Project")

# ===============================
# IJC437 Coursework Script (Run All)
# Run this script from the project folder:
#   IJC437_Rail Project/
#     data/
#     scripts/
#     output/
# ===============================

rm(list = ls())

# Create output folder (safe if it already exists)
dir.create("output", showWarnings = FALSE)

# Install/load required packages (installs only if missing)
pkgs <- c("ggplot2","viridis","dplyr","broom")
missing <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(missing) > 0) install.packages(missing)
invisible(lapply(pkgs, library, character.only = TRUE))

# Check project structure + input file
if (!dir.exists("data")) stop("Missing 'data' folder. Set working directory to the project folder.")
if (!file.exists("data/passenger_journeys_sector_quarterly.csv")) {
  stop("Missing file: data/passenger_journeys_sector_quarterly.csv")
}





#Loading dataset
sector_data <- read.csv("data/passenger_journeys_sector_quarterly.csv")
head(sector_data)
str(sector_data)
colnames(sector_data)

#To know where excatly does quarterly table begins
which(grepl("quarter", apply(sector_data, 1, paste, collapse=" "), ignore.case = TRUE))


#Cleaning Dataset
### ========= 1) Find the quarterly header row =========
# We know row 38 is around where quarterly starts
start_search <- 38

# Find the next "Time period" row AFTER the quarterly section starts
time_period_rows <- which(grepl("^\\s*Time period\\s*$", sector_data[[1]], ignore.case = TRUE))
header_row <- min(time_period_rows[time_period_rows > start_search])

header_row  # just prints the row number so you can see it


### ========= 2) Extract the quarterly table and set clean column names =========
# Pull the header values from that row across all columns
headers_raw <- as.character(unlist(sector_data[header_row, ]))

# Clean the header text (remove newlines, extra spaces)
clean_text <- function(x) {
  x <- gsub("\\n", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
headers <- clean_text(headers_raw)

# Take everything BELOW the header row
qtab <- sector_data[(header_row + 1):nrow(sector_data), ]

# Remove completely empty rows
is_empty_row <- apply(qtab, 1, function(r) all(trimws(as.character(r)) == "" | is.na(r)))
qtab <- qtab[!is_empty_row, ]

# Stop if hit another table title (safety)
stop_at <- which(grepl("^\\s*Table", qtab[[1]], ignore.case = TRUE))
if (length(stop_at) > 0) {
  qtab <- qtab[1:(stop_at[1] - 1), ]
}

# Apply the cleaned headers as column names
colnames(qtab) <- headers

# First column should be the time period
time_col <- colnames(qtab)[1]


### ========= 3) Convert from wide to tidy long =========
# All other columns are sectors
sector_cols <- colnames(qtab)[-1]

# Convert journeys columns to numeric safely (remove commas)
for (c in sector_cols) {
  qtab[[c]] <- suppressWarnings(as.numeric(gsub(",", "", as.character(qtab[[c]]))))
}

# Build long format (TimePeriod, Sector, Journeys)
sector_long <- data.frame(
  TimePeriod = rep(qtab[[time_col]], times = length(sector_cols)),
  Sector = rep(sector_cols, each = nrow(qtab)),
  Journeys = as.vector(as.matrix(qtab[sector_cols])),
  stringsAsFactors = FALSE
)

# Drop rows with no journeys
sector_long <- sector_long[!is.na(sector_long$Journeys), ]


### ========= 4) Create Year + Quarter + Covid + Trend =========
# Year = first 4-digit year we can find
sector_long$Year <- suppressWarnings(as.integer(sub(".*?(\\d{4}).*", "\\1", sector_long$TimePeriod)))

# Quarter from the month range text (common ORR format)
tp <- tolower(sector_long$TimePeriod)

sector_long$Quarter <- NA_character_
sector_long$Quarter[grepl("apr", tp) | grepl("apr to jun", tp)] <- "Q1"
sector_long$Quarter[grepl("jul", tp) | grepl("jul to sep", tp)] <- "Q2"
sector_long$Quarter[grepl("oct", tp) | grepl("oct to dec", tp)] <- "Q3"
sector_long$Quarter[grepl("jan", tp) | grepl("jan to mar", tp)] <- "Q4"

# Covid indicator (simple + correct for your analysis)
sector_long$Covid <- ifelse(sector_long$Year >= 2020, 1, 0)

# Create a sortable quarter index for Trend
q_num <- match(sector_long$Quarter, c("Q1","Q2","Q3","Q4"))
sector_long$QuarterIndex <- sector_long$Year * 4 + q_num

# Trend = 0,1,2,... by quarter (same for all sectors)
sector_long$Trend <- match(sector_long$QuarterIndex, sort(unique(sector_long$QuarterIndex))) - 1


### ========= 5) Quick checks =========
head(sector_long)
str(sector_long)
table(sector_long$Sector)
range(sector_long$Year, na.rm = TRUE)
table(sector_long$Quarter, useNA = "ifany")

#Setting up for plots

#checking here for n/a valuesIf Quarter has lots of NAs, then the Time variable and plots can break or look wrong. This quick check prevents wasted time.
colSums(is.na(sector_long[, c("Year","Quarter","Sector","Journeys","Covid","Trend")]))

#Create the Time variable
sector_long$Time <- sector_long$Year + (match(sector_long$Quarter, c("Q1","Q2","Q3","Q4")) - 1)/4


####Plot - 1 Trend over time by sectors only ggplot(2015+)

library(ggplot2)

# Clean sector names (remove notes)
sector_long$Sector <- gsub("\\s*\\[note.*\\]", "", sector_long$Sector)
sector_long$Sector <- trimws(sector_long$Sector)

# Remove Total and filter 2015+
sector_plot <- subset(sector_long, Sector != "Total passenger journeys" & Year >= 2015)

p1 <- ggplot(sector_plot, aes(x = Time, y = Journeys, colour = Sector)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2020, linetype = "dashed") +
  labs(
    title = "Passenger journeys by rail sector (quarterly, 2015+)",
    subtitle = "Dashed line marks start of Covid period (2020)",
    x = "Year",
    y = "Passenger journeys (million)"
  ) +
  theme_minimal()

p1

#Improving Plot 1 with virdis

##### PLOT 1 

install.packages("viridis")
library(viridis)
library(ggplot2)

# Use 2015+ and remove Total
sector_plot <- subset(sector_long, Year >= 2015)
sector_plot$Sector <- gsub("\\s*\\[note.*\\]", "", sector_plot$Sector)
sector_plot$Sector <- trimws(sector_plot$Sector)
sector_plot <- subset(sector_plot, Sector != "Total passenger journeys")

# Shorten sector names for readability
sector_plot$SectorShort <- sector_plot$Sector
sector_plot$SectorShort[sector_plot$Sector == "Franchised London and South East operators"] <- "London & SE"
sector_plot$SectorShort[sector_plot$Sector == "Franchised long distance operators"] <- "Long Distance"
sector_plot$SectorShort[sector_plot$Sector == "Franchised regional operators"] <- "Regional"
sector_plot$SectorShort[grepl("Open access", sector_plot$Sector, ignore.case = TRUE)] <- "Open Access"

p1 <- ggplot(sector_plot, aes(x = Time, y = Journeys, colour = SectorShort)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 2020, linetype = "dashed") +
  scale_colour_viridis_d() +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Passenger journeys by rail sector (quarterly, 2015+)",
    subtitle = "All sectors collapse sharply in 2020; London & SE shows the largest fall.\nRecovery patterns differ across sectors after 2021.",
    x = "Year",
    y = "Passenger journeys (million)",
    colour = "Sector"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p1


#Saving output 
ggsave("output/finalfigure1_sector_trends_2015_2025.png", p1, width = 13, height = 8, dpi = 300)


list.files("output")





####Plot 2 (Pre vs Post Covid Boxplot comparison)

#Finalll PLOT 2
library(ggplot2)
library(viridis)

sector_plot$Period <- ifelse(sector_plot$Covid == 0, "Pre-Covid", "Post-Covid")

# Order sectors by average PRE-Covid journeys (largest to smallest)
order_df <- aggregate(Journeys ~ SectorShort,
                      data = subset(sector_plot, Period == "Pre-Covid"),
                      FUN = mean)

sector_plot$SectorShort <- factor(
  sector_plot$SectorShort,
  levels = order_df$SectorShort[order(order_df$Journeys, decreasing = TRUE)]
)

p2 <- ggplot(sector_plot, aes(x = SectorShort, y = Journeys, fill = Period)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point",
               position = position_dodge(width = 0.8), size = 2) +
  scale_fill_viridis_d() +
  scale_y_continuous(limits = c(0, max(sector_plot$Journeys) * 1.15)) +
  labs(
    title = "Passenger journeys by sector: Pre vs Post Covid (quarterly, 2015+)",
    subtitle = "Journeys are lower post-Covid across all sectors; London & SE shows the largest absolute decline.",
    caption = "Interpretation: London & SE likely reflects commuter travel demand, which fell most during Covid.",
    x = "Sector",
    y = "Passenger journeys (million)",
    fill = "Period"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

p2

#Saving Output
ggsave("output/finalfigure2_pre_post_covid_boxplot.png", p2, width = 12, height = 6, dpi = 300)




####Plot-3 (Recovery comparision plot)

#Start of Plot 3 using ggplot2 because before one is too messy

install.packages("ggplot2")
library(ggplot2)

sector_long$Sector <- gsub("\\s*\\[note.*\\]","", sector_long$Sector)
sector_long$Sector <- trimws(sector_long$Sector)
#removing total passenger journeys
sector_clean <- subset(sector_long, Sector != "Total passenger journeys")

#Creating the 2019 baseline + index
baseline <- aggregate(Journeys ~ Sector,
                      data = sector_clean[sector_clean$Year == 2019, ],
                      FUN = mean)

sector_idx <- merge(sector_clean, baseline, by = "Sector", suffixes = c("", "_2019"))
sector_idx$Index2019 <- (sector_idx$Journeys / sector_idx$Journeys_2019) * 100

#Filter to 2015
sector_idx <- subset(sector_idx, Year >= 2015)


#Plot 3 in ggplot2


p3 <- ggplot(sector_idx, aes(x = Time, y = Index2019)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 2020, linetype = "dashed") +
  geom_hline(yintercept = 100, linetype = "dotted") +
  facet_wrap(~ Sector) +
  labs(
    title = "Rail sector recovery indexed to 2019 (quarterly, 2015+)",
    subtitle = "Dashed line marks start of Covid period (2020)",
    x = "Year",
    y = "Passenger journeys index (2019 = 100)"
  ) +
  theme_minimal()

p3

#Saving the ouput
ggsave("output/finalfigure3_indexed_recovery_2019_100.png", p3, width = 10, height = 6, dpi = 300)


####REGRESSION MODEL 1

# Make sure SectorShort is treated as a category
sector_plot$SectorShort <- as.factor(sector_plot$SectorShort)

model1 <- lm(Journeys ~ Trend + Covid + SectorShort, data = sector_plot)
summary(model1)

## Formatting regression table

# If you haven't installed these before, run once:
# install.packages(c("broom", "dplyr"))

library(broom)
library(dplyr)


# Tidy model output with confidence intervals
reg_table1 <- tidy(model1, conf.int = TRUE)

# Clean variable names
reg_table1$term <- reg_table1$term %>%
  gsub("SectorShort", "", .) %>%
  gsub("\\(Intercept\\)", "Intercept", .)

# Rename columns for readability
reg_table1 <- reg_table1 %>%
  rename(
    Variable  = term,
    Estimate  = estimate,
    Std_Error = std.error,
    t_value   = statistic,
    p_value   = p.value,
    CI_Lower  = conf.low,
    CI_Upper  = conf.high
  )

# Safety line (prevents ordering issues)
reg_table1$Variable <- as.character(reg_table1$Variable)

# Round numeric columns BUT keep p-values readable
reg_table1 <- reg_table1 %>%
  mutate(
    across(c(Estimate, Std_Error, t_value, CI_Lower, CI_Upper), ~ round(., 3)),
    p_value = ifelse(p_value < 0.001, "<0.001", as.character(round(p_value, 3)))
  )

# Reorder rows (Intercept, Trend, Covid, then sectors)
reg_table1 <- reg_table1 %>%
  arrange(match(Variable, c("Intercept", "Trend", "Covid",
                            "Regional", "Long Distance", "Open Access")))

# Add reference category column (London & SE is the baseline)
reg_table1$Reference <- ifelse(
  reg_table1$Variable %in% c("Regional", "Long Distance", "Open Access"),
  "London & SE",
  ""
)


# View table
reg_table1

# Save as CSV
write.csv(reg_table1, "output/finalmodel1_regression_table_formatted111.csv", row.names = FALSE)



#### Covid Impact Table (Prevspost by sector)

# If you haven't installed dplyr before, run once:
# install.packages("dplyr")

library(dplyr)

# Ensure output folder exists
dir.create("output", showWarnings = FALSE)

# Define Period (matches your Covid dummy: 0 = pre-2020, 1 = 2020+)
sector_plot$Period <- ifelse(sector_plot$Covid == 0, "Pre-Covid (2015–2019)", "Post-Covid (2020+)")

# Mean journeys by sector and period
covid_table <- aggregate(Journeys ~ SectorShort + Period,
                         data = sector_plot, FUN = mean)

# Split into pre and post
pre  <- covid_table[covid_table$Period == "Pre-Covid (2015–2019)",
                    c("SectorShort", "Journeys")]
post <- covid_table[covid_table$Period == "Post-Covid (2020+)",
                    c("SectorShort", "Journeys")]

# Rename columns
names(pre)[2]  <- "PreCovid_Mean"
names(post)[2] <- "PostCovid_Mean"

# Merge into one table
covid_impact <- merge(pre, post, by = "SectorShort")

# Calculate changes (Post - Pre)
covid_impact$Abs_Change <- covid_impact$PostCovid_Mean - covid_impact$PreCovid_Mean
covid_impact$Pct_Change <- (covid_impact$Abs_Change / covid_impact$PreCovid_Mean) * 100

# Add a simple direction label (optional but clearer)
covid_impact$Direction <- ifelse(covid_impact$Abs_Change < 0, "Decrease", "Increase")

# Round values
covid_impact <- covid_impact %>%
  mutate(
    PreCovid_Mean  = round(PreCovid_Mean, 1),
    PostCovid_Mean = round(PostCovid_Mean, 1),
    Abs_Change     = round(Abs_Change, 1),
    Pct_Change     = round(Pct_Change, 1)
  )

# Order by largest percentage drop (most negative first)
covid_impact <- covid_impact[order(covid_impact$Pct_Change), ]

# View + save
covid_impact
write.csv(covid_impact, "output/finalcovid_impact_table.csv", row.names = FALSE)




#### Recovery Ratio table (2019 baseline -> latest year)


# Automatically detect most recent year in your filtered dataset (2015+)
latest_year <- max(sector_plot$Year, na.rm = TRUE)

# Baseline = mean journeys in 2019 by sector
base2019 <- aggregate(Journeys ~ SectorShort,
                      data = sector_plot[sector_plot$Year == 2019, ],
                      FUN = mean)
names(base2019)[2] <- "Mean_2019"

# Latest year mean (average across that year's quarters)
latest <- aggregate(Journeys ~ SectorShort,
                    data = sector_plot[sector_plot$Year == latest_year, ],
                    FUN = mean)
names(latest)[2] <- paste0("Mean_", latest_year)

# Merge and compute recovery metrics
recovery <- merge(base2019, latest, by = "SectorShort")

latest_col <- paste0("Mean_", latest_year)

recovery$Recovery_Ratio <- recovery[[latest_col]] / recovery$Mean_2019
recovery$Recovery_Index <- recovery$Recovery_Ratio * 100

# Round nicely
recovery$Mean_2019        <- round(recovery$Mean_2019, 1)
recovery[[latest_col]]    <- round(recovery[[latest_col]], 1)
recovery$Recovery_Ratio   <- round(recovery$Recovery_Ratio, 3)
recovery$Recovery_Index   <- round(recovery$Recovery_Index, 1)

# Order: highest recovery first
recovery <- recovery[order(recovery$Recovery_Index, decreasing = TRUE), ]

# View + save
recovery
write.csv(recovery, "output/finalrecovery_ratio_table.csv", row.names = FALSE)

# (Optional) print which year was used
latest_year


list.files("output")