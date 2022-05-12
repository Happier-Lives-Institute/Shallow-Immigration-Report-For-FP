# Read dependencies
source("_dependencies/dependencies.R")

# Get the data from the gsheet
d.all <- read_sheet(
  "https://docs.google.com/spreadsheets/d/13s3TOFG8tlroWkJ_MjAYDyqs7KOj6ZMUREMTAr97W08/",
  sheet = "Immigrate <> SWB"
)

# Get data for calculation of how much the gap is closed
# Exclude the data from Lönnqvist et al., (2015) that is before immigration
d.gap <- d.all %>% filter(`type of % calculation` == "% of gap" & 
                            `FU from immigration to measure in years` != "-0.5") %>% 
  select(Citation, `sample size`, `FU from immigration to measure in years`, `% of gap closed`) %>% 
  rename(N = `sample size`, gap = `% of gap closed`)
GAP <- weighted.mean(x = d.gap$gap, w = d.gap$N); GAP*100
range(d.gap$gap)*100
