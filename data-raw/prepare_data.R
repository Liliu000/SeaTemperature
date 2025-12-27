
library(dplyr)
library(tidyr)
library(openxlsx)
library(haven)
library(Hmisc)

# ===== 1. Prepare sea.deep dataset (2000-2017) =====

# Read your cleaned data
sea.deep.raw <- read.xlsx("../Sea_Deep_Clean.xlsx")

# Original month labels in Catalan
original_mes_labels <- c(
  "Gener", "Febrer", "Març", "Abril", "Maig", "Juny",
  "Juliol", "Agost", "Setembre", "Octubre", "Novembre",
  "Desembre", "Mitjana anual"
)

# Prepare the dataset
sea.deep <- sea.deep.raw %>%
  mutate(
    Mes = factor(Mes, levels = original_mes_labels),
    Any = as.integer(Any),
    Profunditat = as.numeric(Profunditat),
    Temperatura = as.numeric(Temperatura)
  ) %>%
  filter(!is.na(Temperatura), !is.na(Mes))

# Add labels
label(sea.deep$Any) <- "Any"
label(sea.deep$Mes) <- "Mes"
label(sea.deep$Profunditat) <- "Profunditat (m)"
label(sea.deep$Temperatura) <- "Temperatura mitjana (°C)"

# Save to package data
usethis::use_data(sea.deep, overwrite = TRUE)


# ===== 2. Prepare baseline_1974_1999 dataset =====

# Read original Excel file
raw <- read.xlsx("../sea_temperature.xlsx", colNames = FALSE)

# Find row with title "Period 1974-1999"
r0 <- which(apply(raw, 1, function(r) any(grepl("1974-1999", r, ignore.case = TRUE))))[1]

# Extract 12 rows of monthly data
baseline_raw <- raw[(r0+1):(r0+12), c("X2","X3","X4","X5","X6")]
colnames(baseline_raw) <- c("Mes", "T_0", "T_20", "T_50", "T_80")

baseline_1974_1999 <- baseline_raw %>%
  mutate(Mes_num = 1:12) %>%
  pivot_longer(cols = starts_with("T_"),
               names_to = "depth",
               values_to = "baseline_temp") %>%
  mutate(
    Profunditat = case_when(
      depth == "T_0"  ~ 0,
      depth == "T_20" ~ -20,
      depth == "T_50" ~ -50,
      depth == "T_80" ~ -80
    ),
    baseline_temp = as.numeric(baseline_temp)
  ) %>%
  select(Mes_num, Profunditat, baseline_temp)

# Save to package data
usethis::use_data(baseline_1974_1999, overwrite = TRUE)
