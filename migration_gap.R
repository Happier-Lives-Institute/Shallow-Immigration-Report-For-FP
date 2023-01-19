# Read dependencies
source("_dependencies/dependencies.R")

######################################
# Calculating the closing of the gap #
######################################

# Get the data from the gsheet
d.all <- read_sheet(
  "https://docs.google.com/spreadsheets/d/13s3TOFG8tlroWkJ_MjAYDyqs7KOj6ZMUREMTAr97W08/",
  sheet = "Closing the SWB gap"
)

# Get data for calculation of how much the gap is closed
# Exclude the data from Lönnqvist et al., (2015) that is before immigration
d.gap <- d.all %>% filter(`type of % calculation` == "% of gap" & 
                            `FU from immigration to measure in years` != "-0.5") %>% 
  select(Citation, `sample size`, `FU from immigration to measure in years`, `% of gap closed`) %>% 
  rename(N = `sample size`, gap = `% of gap closed`)

uniqueN(d.gap$Citation) - 2 # number of sources understanding that PINZMS is only one
sum(d.gap$N) # number of observations

GAP <- weighted.mean(x = d.gap$gap, w = d.gap$N); GAP*100
range(d.gap$gap)*100

##############################
# gap for individual sources #
##############################

# For Hendriks et al., (2018)
d.gap.hendriks <- d.gap %>% filter(Citation == "Hendriks et al., (2018)")
weighted.mean(x = d.gap.hendriks$gap, w = d.gap.hendriks$N)*100
sum(d.gap.hendriks$N)
nrow(d.gap.hendriks)

# For Helliwell et al. 2018
d.gap.helliwell <- d.gap %>% filter(Citation == "Helliwell et al. 2018")
weighted.mean(x = d.gap.helliwell$gap, w = d.gap.helliwell$N)*100
sum(d.gap.helliwell$N)
nrow(d.gap.helliwell)

# For Lönnqvist et al., (2015)
d.gap.lonnqvist <- d.gap %>% filter(Citation == "Lonnqvist et al., (2015)")
weighted.mean(x = d.gap.lonnqvist$gap, w = d.gap.lonnqvist$N)*100
sum(d.gap.lonnqvist$N)
nrow(d.gap.lonnqvist)

# For PINZMS
d.gap.PINZMS <- d.gap %>% filter(str_detect(Citation, "PINZMS"))
weighted.mean(x = d.gap.PINZMS$gap, w = d.gap.PINZMS$N)*100
sum(d.gap.PINZMS$N)
nrow(d.gap.PINZMS)

# For Baltatescu 2007
d.gap.baltatescu <- d.gap %>% filter(Citation == "Baltatescu 2007")
weighted.mean(x = d.gap.baltatescu$gap, w = d.gap.baltatescu$N)*100
sum(d.gap.baltatescu$N)
nrow(d.gap.baltatescu)


######################################################
# Calculating how LS in countries changes over time  #
######################################################

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
summary(GWP10s.mod1)
# GWP10s.mod2 <- lm(log(ls) ~ unhappy*time, GWP10s)
# exp(coef(GWP10s.mod2))
# exp(coef(GWP10s.mod2)[3]+coef(GWP10s.mod2)[4])

######################################
# Calculating the effect on natives  #
######################################

# Effect on the natives #
d.nativespillover <- read_sheet(
  "https://docs.google.com/spreadsheets/d/13s3TOFG8tlroWkJ_MjAYDyqs7KOj6ZMUREMTAr97W08/",
  sheet = "SWB effects on natives"
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

####################################################
# Calculating the effect on households left behind #
####################################################

hhspillovers <- read_sheet(
  "https://docs.google.com/spreadsheets/d/13s3TOFG8tlroWkJ_MjAYDyqs7KOj6ZMUREMTAr97W08/",
  sheet = "SWB hh spillovers"
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
coef(mod.hhspillovers1)[[1]]*2
sum((hhspillovers %>% filter(!is.na(d)))$`sample size`)
nrow((hhspillovers %>% filter(!is.na(d))))
     
      