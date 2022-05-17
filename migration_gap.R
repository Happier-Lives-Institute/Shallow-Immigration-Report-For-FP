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

GWP <- read_csv("happiness-cantril-ladder.csv", show_col_types = FALSE) %>% 
  rename(ls = `Life satisfaction in Cantril Ladder (World Happiness Report 2021)`,
         country = Entity) %>% 
  mutate(time = Year - 2005)

# Gap between happy and unhappy countries

# Get average before 2010
GWP00s <- GWP %>% filter(Year < 2010) %>% group_by(country) %>% summarise(
  pre2010avg = mean(ls, na.rm = T)
)
GWP <- full_join(GWP, GWP00s)
# Get scores after 2010
# Separate between unhappy and happy
GWP10s <- GWP %>% filter(Year >= 2010) %>% mutate(unhappy = pre2010avg < 5)

GWP10s.mod1 <- lm(ls ~ unhappy*time, GWP10s)
GWP10s.mod2 <- lm(log(ls) ~ unhappy*time, GWP10s)
exp(coef(GWP10s.mod2))

# LS over time analysis
summary(lm(ls ~ time, GWP))
summary(lm(log(ls) ~ time, GWP))

# SD of LS over time analysis
GWP_sd <- GWP %>% group_by(Year) %>% 
  summarise(sd = sd(ls, na.rm = T)) %>% 
  mutate(time = Year - 2005)
GWP.mod1 <- lm(sd ~ time, GWP_sd)
summary(GWP.mod1)
GWP.mod2 <- lm(log(sd) ~ time, GWP_sd)
summary(GWP.mod2)
exp(coef(GWP.mod2))




hhspillovers <- read_sheet(
  "https://docs.google.com/spreadsheets/d/13s3TOFG8tlroWkJ_MjAYDyqs7KOj6ZMUREMTAr97W08/",
  sheet = "Immigrate <> SWB (hh spillovers)"
)

hhspillovers <- hhspillovers %>% rowwise() %>% mutate(
  n1 = `sample size`/2,
  n2 = `sample size`/2,
  d_se = getDSE(d, n1=n1, n2=n2)
) %>% ungroup()

mod.hhspillovers1 <- rma.mv(yi = d, V = d_se^2,
                               slab = Citation,
                               random =  ~1 | Citation,
                               test = "t", method="REML",
                               data = hhspillovers)
summary(mod.hhspillovers1)

# hhspillovers <- hhspillovers %>% rename(N = `sample size`, gap = `% of gap closed`)
# GAP.hh <- weighted.mean(x = hhspillovers$gap, w = hhspillovers$N, na.rm = T); GAP.hh*100
