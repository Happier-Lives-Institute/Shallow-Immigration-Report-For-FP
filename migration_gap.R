# Read dependencies
source("_dependencies/dependencies.R")

# Get the data from the gsheet
d.all <- read_sheet(
  "https://docs.google.com/spreadsheets/d/13s3TOFG8tlroWkJ_MjAYDyqs7KOj6ZMUREMTAr97W08/",
  sheet = "Immigrate <> SWB (closing the gap)"
)

# Get data for calculation of how much the gap is closed
# Exclude the data from Lönnqvist et al., (2015) that is before immigration
d.gap <- d.all %>% filter(`type of % calculation` == "% of gap" & 
                            `FU from immigration to measure in years` != "-0.5") %>% 
  select(Citation, `sample size`, `FU from immigration to measure in years`, `% of gap closed`) %>% 
  rename(N = `sample size`, gap = `% of gap closed`)
GAP <- weighted.mean(x = d.gap$gap, w = d.gap$N); GAP*100
range(d.gap$gap)*100

# Effect on the natives #
d.nativespillover <- read_sheet(
  "https://docs.google.com/spreadsheets/d/13s3TOFG8tlroWkJ_MjAYDyqs7KOj6ZMUREMTAr97W08/",
  sheet = "Immigrate <> SWB (effects on host)"
)

# Add d_se and modify attitude (to be 1% increases)
d.nativespillover <-  d.nativespillover %>% rowwise() %>% mutate(
  attitudes = attitudes * 100,
  n1 = `sample size`/2,
  n2 = `sample size`/2,
  d_se = getDSE(`d adjusted`, n1=n1, n2=n2)
) %>% ungroup()

# details
nrow(d.nativespillover) # number of effect sizes
sum(d.nativespillover$`sample size`) # observations
sum(d.nativespillover$`d adjusted` > 0)

mod.nativespillover1 <- rma.mv(yi = `d adjusted`, V = d_se^2,
                slab = Citation,
                random =  ~1 | Citation,
                test = "t", method="REML",
                data = d.nativespillover)
summary(mod.nativespillover1)

mod.nativespillover2 <- rma.mv(yi = `d adjusted`, V = d_se^2,
                               slab = Citation,
                               random =  ~1 | Citation,
                               mods = ~ attitudes,
                               test = "t", method="REML",
                               data = d.nativespillover)
summary(mod.nativespillover2)
regplot(mod.nativespillover2)
ggplot(d.nativespillover, aes(x=attitudes, y=`d adjusted`)) +
  geom_point(aes(color=Citation, size=1/d_se)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  theme_cowplot()

# Trying quadratic modelling
# mod.nativespillover3 <- rma.mv(yi = `d adjusted`, V = d_se^2,
#                                slab = Citation,
#                                random =  ~1 | Citation,
#                                mods = ~ attitudes*I(attitudes^2),
#                                test = "t", method="REML",
#                                data = d.nativespillover)
# summary(mod.nativespillover3)
# regplot(mod.nativespillover3)

# Trying to look at size areas
# ggplot(d.nativespillover, aes(x=`how big the area is`, y=`d adjusted`)) +
#   geom_point(aes(color=Citation)) + 
#   # geom_smooth() +
#   geom_abline() +
#   theme_cowplot()
