


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times.
library("lubridate") # lubridate, for date/times.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.
library("vctrs")
library("precis")

library("tibbletime") # https://business-science.github.io/tibbletime/

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)
library("bdata")

library("qtax")

library("BEAData")


#****************************************************************************************************
#                States to focus on ####
#****************************************************************************************************



#****************************************************************************************************
#                Get data ####
#****************************************************************************************************
data(package="bdata")
# slgfin 
# spop.a
glimpse(slgfin)
slgfin %>% filter(stabbr=="NY", level==2, year==2015, aggvar=="tottax") # value is in $ thousands

data(package="BEAData") 
# sgdp.a
# spi.a
glimpse(spi.a) # spi is in $ billions
spi.a %>% filter(stabbr=="NY", year==2015) # pop is in thousands, spi is in $ billions

qtd <- "d:/Data/CensusFinanceData/QTax/qtax rfiles/"
nb <- readRDS(paste0(qtd, "stateneighbors.rds"))
glimpse(nb)
nb %>% filter(stabbr=="NY")

count(slgfin, aggvar) %>% data.frame
count(slgfin %>% filter(str_detect(aggvar, "gen")), aggvar) # totx.gen
count(slgfin %>% filter(str_detect(aggvar, "igr")), aggvar) # igr fedigr local igr; igr and local igr not avail until 2004 in my data
slgfin %>% filter(str_detect(aggvar, "gen"), level==2, year==2015, stabbr=="US") %>% arrange(-value)

slgfin %>% filter(stabbr=="US", level==2, year==2015, aggvar=="totx.gen")

#****************************************************************************************************
#                Spending relative to gdp, pi, and per capita ####
#****************************************************************************************************
glimpse(slgfin)
getgdppi()
gdppi <- getgdppi() %>% select(year, gdppi) %>%
  mutate(igdppi=gdppi[year==2015] / gdppi)
gdppi
# pop is in thousands, spi is in $ billions

# 
xtrend <- slgfin %>%
  ungroup %>%
  filter(level==2, aggvar %in% c("totx.gen", "fedigr"), stabbr!="DC") %>%
  select(stabbr, year, aggvar, value) %>%
  spread(aggvar, value) %>%
  mutate(ownx.gen=totx.gen - fedigr) %>%
  left_join(spi.a %>% select(stabbr, year, spi, pop)) %>%
  left_join(sgdp.a %>% select(stabbr, year, gdp) %>% mutate(gdp=gdp / 1000)) %>% # put in same units as spi
  left_join(gdppi %>% select(year, igdppi)) %>%
  mutate(xpi=ownx.gen / (spi * 1e6) * 100,
         xgdp=ownx.gen / (gdp * 1e6) * 100,
         xrpc=(ownx.gen / pop) * igdppi,
         txpi=totx.gen / (spi * 1e6) * 100,
         txgdp=totx.gen / (gdp * 1e6) * 100,
         txrpc=(totx.gen / pop) * igdppi,
         rpi=spi * igdppi,
         rx=ownx.gen * igdppi,
         rtx=totx.gen * igdppi,
         pirpc=(spi * 1e6 / pop) * igdppi)
xtrend %>% filter(stabbr=="NY")

# how much difference does the denominator make?

#..trend analysis----
st2 <- sts
st2 <- c("CA", "NY", "US")
xtrend %>% 
  filter(stabbr %in% st2, year>=1997) %>%
  select(year, stabbr, xpi, xgdp, txpi) %>%
  gather(variable, value, -year, -stabbr) %>%
  ggplot(aes(year, value, colour=variable)) +
  geom_line() + geom_point() +
  facet_wrap(~stabbr, nrow=3, scales="free") +
  ggtitle("Own-source general expenditures as % of state GDP and state PI, selected states") +
  scale_y_continuous(name="Expenditures as % of GDP or PI", breaks=seq(0, 20, .5))


#..levels analysis----
pdata <- xtrend %>% 
  filter(year %in% c(2005, 2015), stabbr %in% state.abb) %>%
  select(year, stabbr, xpi, xgdp) %>%
  mutate(year=factor(year)) %>%
  group_by(year) %>%
  mutate(xpi.rank=rank(xpi), xgdp.rank=rank(xgdp))

pdata %>%
  ggplot(aes(x=xpi.rank, y=xgdp.rank, label=stabbr)) + 
  geom_text(size=3, colour="blue") +
  geom_abline(slope=1, intercept=0) +
  coord_equal() +
  facet_wrap(~year, ncol=2) +
  ggtitle("State general expenditures from own funds as % of GDP and of PI, ranked, 2005 and 2015")


#..time period analysis----
# look at real spending per capita, and real pi per capita
# xpi xrpc rpi  pirpc
glimpse(xtrend)

vars <- c("pop", "rpi", "rtx", "rx")
vnames <- c("Population", "Real\npersonal\nincome", "Real\ngeneral\nexpenditures", "Real\nown-source\ngeneral\nexpenditures")

vars <- c("txpi", "xpi",  "txrpc", "xrpc")
vnames <- c("General\nexpenditures\nas % of PI",
            "Own-source\ngen. exp.\nas % of PI",
            "Real general\nexpenditures\nper capita", "Real\nown-source gen. exp.\nper capita")

xtrend %>%
  select(stabbr, year, vars) %>%
  gather(variable, value, -stabbr, -year) %>%
  group_by(stabbr, variable) %>%
  mutate(pchya=value / value[match(year - 1, year)] * 100 - 100)  %>%
  filter(stabbr %in% sts, year>=1990) %>%
  mutate(varf=factor(variable, levels=vars, labels=vnames)) %>%
  arrange(varf) %>%
  ggplot(aes(x=year, y=pchya, colour=varf)) +
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  facet_wrap(~stabbr, scales="free", ncol=3) +
  ggtitle("Year-over-year % change in selected variables, selected states with diverse economic structures")



vars <- c("txpi", "xpi")
vnames <- c("General\nexpenditures\nas % of PI",
            "Own-source\ngen. exp.\nas % of PI")
xtrend %>%
  select(stabbr, year, vars) %>%
  gather(variable, value, -stabbr, -year) %>%
  group_by(stabbr, variable) %>%
  filter(stabbr %in% sts, year>=1990) %>%
  mutate(varf=factor(variable, levels=vars, labels=vnames)) %>%
  arrange(varf) %>%
  ggplot(aes(x=year, y=value, colour=varf)) +
  geom_line() + 
  geom_point() + 
  facet_wrap(~stabbr, scales="free", ncol=3) +
  ggtitle("Selected expenditure measures, selected states with diverse economic structures")


#****************************************************************************************************
#                Moving mean ####
#****************************************************************************************************
# we will only have values for spending components from about 2004 on
ma <- function(x, n) {
  # note that this requires zoo
  zoo::rollapply(x, n, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")
}
movmean <- slgfin %>%
  ungroup %>%
  filter(level==2, stabbr!="DC", year>=1961) %>%
  select(stabbr, year, aggvar, value) %>%
  spread(aggvar, value) %>%
  mutate(ownx.gen=totx.gen - fedigr,
         other.gen=totx.gen - hiedx.gen - k12x.gen - pwelfx.gen - corrx.gen - highwayx.gen) %>%
  left_join(spi.a %>% select(stabbr, year, spi)) %>%
  left_join(gdppi %>% select(year, igdppi)) %>%
  mutate_at(vars(-stabbr, -year, -igdppi), funs(. * igdppi)) %>% # get real values
  select(-igdppi) %>%
  group_by(stabbr) %>%
  arrange(year) %>%
  mutate_at(vars(-stabbr, -year), funs(ma(., 3))) %>% # 3-year moving average
  mutate_at(vars(-stabbr, -year, -spi), funs(. / (spi * 1e6) * 100)) %>%
  select(-spi) %>%
  select(stabbr, year, totx.gen, fedigr, ownx.gen, everything())

movmean %>% filter(stabbr=="NY")

peakyear <- movmean %>%
  filter(year %in% 2008:2011) %>%
  select(stabbr, year, totx.gen, ownx.gen) %>%
  group_by(stabbr) %>%
  top_n(1, totx.gen) %>%
  select(stabbr, peakyear=year)

vars <- c("totx.gen", "ownx.gen")
vnames <- c("General\nexpenditures\nas % of PI",
            "Own-source\ngen. exp.\nas % of PI")

lastvaldf <- movmean %>%
  select(stabbr, year, vars) %>%
  filter(stabbr %in% sts, year==2015) %>%
  gather(variable, value, -stabbr, -year) %>%
  mutate(varf=factor(variable, levels=vars, labels=vnames))
lastvaldf

movmean %>%
  select(stabbr, year, vars) %>%
  gather(variable, value, -stabbr, -year) %>%
  group_by(stabbr, variable) %>%
  filter(stabbr %in% sts, year>=1990) %>%
  mutate(varf=factor(variable, levels=vars, labels=vnames)) %>%
  arrange(varf) %>%
  ggplot(aes(x=year, y=value, colour=varf)) +
  geom_line() + 
  geom_point() + 
  geom_hline(aes(yintercept = value), data=lastvaldf) +
  geom_vline(xintercept=seq(1995, 2010, 5), linetype="dashed") +
  scale_y_continuous(name="% of PI", breaks=seq(0, 40, .5)) +
  facet_wrap(~stabbr, scales="free", ncol=3) +
  ggtitle("Selected expenditure measures, selected states with diverse economic structures",
          subtitle="Constructed with 3-year moving average of inflation-adjusted numerator and denominator")


#..line graphs of moving mean general expenditures, 2015 vs peak in 2008-2011 ----
plotdf <- movmean %>%
  select(stabbr, year, totx.gen, ownx.gen) %>%
  left_join(peakyear) %>%
  filter(year >= peakyear) %>%
  select(-peakyear) %>%
  gather(variable, value, -stabbr, -year) %>%
  group_by(stabbr, variable) %>%
  arrange(year) %>%
  mutate(diffpi=value - first(value))
glimpse(plotdf)


vars <- c("totx.gen", "ownx.gen")
vnames <- c("General\nexpenditures\nas % of PI",
            "Own-source\ngen. exp.\nas % of PI")
plotdf %>%
  filter(stabbr %in% sts) %>%
  mutate(varf=factor(variable, levels=vars, labels=vnames)) %>%
  arrange(varf) %>%
  ggplot(aes(x=year, y=diffpi, colour=varf)) +
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 0) +
  scale_y_continuous(name="Change vs peak year, as % of PI", breaks=seq(-40, 40, 0.5)) +
  facet_wrap(~stabbr, scales="free", ncol=3) +
  ggtitle("Expenditure measures relative to state-specific pre-recession peak, selected states with diverse economic structures",
          subtitle="Constructed with 3-year moving average of inflation-adjusted numerator and denominator")


#..scatterplot of moving mean general expenditures, 2015 vs peak in 2008-2011 ----

glimpse(movmean)
genx.plotdf <- movmean %>%
  select(stabbr, year, totx.gen) %>%
  left_join(peakyear) %>%
  filter(year==peakyear | year==2015) %>%
  mutate(year=ifelse(year==2015, "end", "base")) %>%
  select(stabbr, year, totx.gen) %>%
  spread(year, totx.gen) %>%
  mutate(change=end - base)

genx.plotdf <-
  genx.plotdf %>%
  filter(stabbr!="AK")

# compute x limits for equal coordinates
yrange <- range(genx.plotdf$end) %>% diff
xrange <- range(genx.plotdf$change) %>% diff
rangediff <- yrange - xrange
xlim.min <- min(genx.plotdf$change) - rangediff / 2
xlim.max <- max(genx.plotdf$change) + rangediff / 2
(xlim.max - xlim.min)
yrange


us2015 <- genx.plotdf %>% filter(stabbr=="US") %>% .[["end"]]
uschange <- genx.plotdf %>% filter(stabbr=="US") %>% .[["change"]]

base.state <- "NM"
comps <- nb$nstabbr[nb$stabbr==base.state]
outnote <- "\nNote: (1) 3-year moving averages; (2) Outlier state of AK excluded (Pew could include, with axis break)"
# outnote <- "\nNote: 3-year moving averages"
p <- genx.plotdf %>%
  # filter(!stabbr %in% c("AK")) %>%
  mutate(stgroup=case_when(stabbr==base.state ~ "base",
                           stabbr %in% comps ~ "comp",
                           TRUE ~ "other")) %>%
  ggplot(aes(x=change, y=end, label=stabbr)) +
  geom_text(aes(colour=stgroup, size=stgroup), fontface="bold") +
  scale_colour_manual(values=c("darkblue", "red", "darkgrey")) +
  scale_size_manual(values=c(4, 3.25, 2.75)) +
  geom_vline(xintercept=uschange) +
  geom_hline(yintercept=us2015) +
  theme_bw() +
  ggtitle("State government general expenditure as % of personal income: recession peak and 2015",
          subtitle = paste0(getstname(base.state), " and its neighbors")) +
  theme(legend.position="none") +
  labs(caption=outnote) +
  theme(plot.caption = element_text(hjust=0, size=10)) +
  scale_y_continuous(name="General expenditure as % of personal income, 2015", breaks=seq(0, 40, .5))

p + scale_x_continuous(name="Change in general expenditure as % of personal income, recession peak to 2015",
                       breaks=seq(-10, 10, .5), limits=c(xlim.min, xlim.max)) +
  coord_equal()

p + scale_x_continuous(name="Change in general expenditure as % of personal income, recession peak to 2015",
                       breaks=seq(-10, 10, .5))



#..scatterplot, change in 3yma of genx and own-genx, 2015 vs genx peak year in 2008-2011 ----

peakyear

glimpse(movmean)
plotdf <- movmean %>%
  select(stabbr, year, totx.gen, ownx.gen) %>%
  left_join(peakyear) %>%
  filter(year==peakyear | year==2015) %>%
  mutate(year=ifelse(year==2015, "end", "base")) %>%
  select(-peakyear) %>%
  gather(variable, value, -stabbr, -year) %>%
  unite(yearvar, year, variable) %>%
  spread(yearvar, value) %>%
  mutate(genx.change=end_totx.gen - base_totx.gen,
         ownx.change=end_ownx.gen - base_ownx.gen)
plotdf

usgenx <- plotdf %>% filter(stabbr=="US") %>% .[["genx.change"]]
usownx <- plotdf %>% filter(stabbr=="US") %>% .[["ownx.change"]]

base.state <- "NM"
comps <- nb$nstabbr[nb$stabbr==base.state]
outnote <- "\nNote: Based upon 3-year moving averages"
p <- plotdf %>%
  mutate(stgroup=case_when(stabbr==base.state ~ "base",
                           stabbr %in% comps ~ "comp",
                           TRUE ~ "other")) %>%
  ggplot(aes(x=ownx.change, y=genx.change, label=stabbr)) +
  geom_text(aes(colour=stgroup, size=stgroup), fontface="bold") +
  scale_colour_manual(values=c("darkblue", "red", "darkgrey")) +
  scale_size_manual(values=c(4, 3.25, 2.75)) +
  geom_vline(xintercept=usownx) +
  geom_hline(yintercept=usgenx) +
  theme_bw() +
  ggtitle("State government total and own-source general expenditures as % of personal income:\nChange from recession-era peak to 2015",
          subtitle = paste0(getstname(base.state), " and its neighbors")) +
  theme(legend.position="none") +
  labs(caption=outnote) +
  theme(plot.caption = element_text(hjust=0, size=10)) +
  scale_x_continuous(name="Change in own-source general expenditure as % of personal income", 
                     breaks=seq(-40, 40, .5),
                     limits=c(-3, NA)) +
  scale_y_continuous(name="Change in total general expenditure as % of personal income", breaks=seq(-40, 40, .5)) +
  geom_smooth(method=lm, se=FALSE, linetype="dashed", size=0.5)
p + coord_equal()


# decomposition of moving mean ----
slgfin %>% filter(str_detect(aggvar, "gen"), level==2, year==2015, stabbr=="US") %>% arrange(-value)
glimpse(movmean)
ht(movmean)
peakyear

dcomp <- movmean %>%
  mutate_at(vars(-stabbr, -year), funs(naz)) %>%
  left_join(peakyear) %>%
  filter(year==peakyear | year==2015) %>%
  select(-peakyear) %>%
  mutate(year=ifelse(year==2015, "end", "base")) %>%
  gather(variable, value, -stabbr, -year) %>%
  spread(year, value) %>%
  mutate(change=end - base)
glimpse(dcomp)

dcomp %>%
  filter(stabbr=="NM")
dcomp %>% filter(stabbr=="NM", variable %in% vorder) %>% arrange(-end)

# make bar graph, selected state and US
st <- "NM"
bardat <- dcomp %>%
  filter(stabbr %in% c(st, "US")) %>%
  select(stabbr, variable, change)
bardat

vorder <- c("totx.gen", "k12x.gen", "hiedx.gen", "pwelfx.gen", "highwayx.gen", 
            "hospx.gen", "healthx.gen", "corrx.gen", "natresx.gen", "adminx.gen", "interestondebt.gen", "other.gen")
vnames <- c("Total", "K-12 education", "Higher education", "Public welfare", "Highways", 
            "Hospitals", "Health (public health)", "Corrections", "Natural resources", "Administration", "Interest", "All other")
bardat2 <- bardat %>%
  ungroup %>%
  filter(variable %in% vorder) %>%
  spread(variable, change) %>%
  mutate(other.gen=totx.gen - hiedx.gen - k12x.gen - pwelfx.gen - highwayx.gen
           - hospx.gen - healthx.gen - corrx.gen - adminx.gen - interestondebt.gen) %>%
  gather(variable, change, -stabbr) %>%
  mutate(state=getstname(stabbr),
         varf=factor(variable, levels=vorder, labels=vnames)) %>%
  arrange(varf)

bardat2 %>%
  ggplot(aes(varf, change)) +
  geom_bar(aes(fill=state),
           stat="identity",
           width = .8, position = position_dodge(width=.8)) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(name="Change in general expenditure as percentage of personal income",
                     breaks=seq(-5, 5, .1)) +
  ggtitle("Change in general expenditures as percentage of personal income, recession-period peak to 2015\nMeasure constructed with 3-year moving averages",
          subtitle=paste0(getstname(st), " and the United States")) +
  scale_fill_manual(values=c("blue", "gray")) +
  scale_x_discrete(name=NULL) +
  theme_bw() +
  theme(legend.position="right",
        legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = -30, hjust=0))

# horizontal bars
bardat2 %>%
  mutate(varf=factor(variable, levels=rev(vorder), labels=rev(vnames), ordered=TRUE)) %>%
  ggplot(aes(x=varf, y=change)) +
  geom_bar(aes(fill=state),
           stat="identity",
           width = .8, position = position_dodge(width=.8)) +
  geom_hline(yintercept = 0) +
  scale_y_reverse(name="Change in general expenditure as percentage of personal income",
                      breaks=seq(-5, 5, .1)) +
  scale_x_discrete(name=NULL) +
  ggtitle("Change in general expenditures as percentage of personal income, 2005 to 2015",
          subtitle=paste0(getstname(st), " and the United States")) +
  scale_fill_manual(values=c("blue", "gray")) +
  theme_bw() +
  theme(legend.position="right", legend.title = element_blank()) +
  coord_flip()




# OLD???----
#..scatterplot of moving mean ----
# get xpi in 2015, and change from 2005
glimpse(movmean)
genx.plotdf <- movmean %>%
  select(stabbr, year, xpi) %>%
  filter(year %in% c(2005, 2015)) %>%
  mutate(year=paste0("xpi_", year)) %>%
  spread(year, xpi) %>%
  mutate(change=xpi_2015 - xpi_2005)

us2015 <- genx.plotdf %>% filter(stabbr=="US") %>% .[["xpi_2015"]]
uschange <- genx.plotdf %>% filter(stabbr=="US") %>% .[["change"]]
# plotdf %>%
#   filter(!stabbr %in% c("AK", "ND")) %>%
#   ggplot(aes(x=xpi_2015, y=change, label=stabbr)) +
#   geom_text(colour="blue", size=3) +
#   geom_vline(xintercept=us2015) +
#   geom_hline(yintercept=uschange) +
#   theme_bw() +
#   scale_x_continuous(name="Own-source spending as % of personal income, 2015") +
#   scale_y_continuous(name="Change in own-source spending as % of personal income, 2005 to 2015")
#   # geom_point() +

# base.state <- "AZ"
base.state <- "VA"
comps <- nb$nstabbr[nb$stabbr==base.state]
outnote <- "\nNote: (1) 3-year moving averages; (2) Outlier state of AK excluded (Pew could include, with axis break)"
p <- genx.plotdf %>%
  filter(!stabbr %in% c("AK")) %>%
  mutate(stgroup=case_when(stabbr==base.state ~ "base",
                           stabbr %in% comps ~ "comp",
                           TRUE ~ "other")) %>%
  ggplot(aes(x=change, y=xpi_2015, label=stabbr)) +
  geom_text(aes(colour=stgroup, size=stgroup), fontface="bold") +
  scale_colour_manual(values=c("darkblue", "red", "darkgrey")) +
  scale_size_manual(values=c(4, 3.25, 2.75)) +
  geom_vline(xintercept=uschange) +
  geom_hline(yintercept=us2015) +
  theme_bw() +
  scale_y_continuous(name="General expenditure as % of personal income, 2015", breaks=seq(0, 40, .5)) +
  scale_x_continuous(name="Change in general expenditure as % of personal income, 2005 to 2015", breaks=seq(-5, 5, .5)) +
  ggtitle("State government general expenditure as % of personal income: 2005 and 2015",
          subtitle = paste0(getstname(base.state), " and its neighbors")) +
  theme(legend.position="none") +
  labs(caption=outnote) +
  theme(plot.caption = element_text(hjust=0, size=10))
# p
p + scale_x_continuous(name="Change in general expenditure as % of personal income, 2005 to 2015",
                       breaks=seq(-10, 10, .5), limits=c(-7, 8)) + 
  coord_equal()




#****************************************************************************************************
#                Scatter with Pew's measure ####
#****************************************************************************************************
# create Pew's measure
pewx <- slgfin %>%
  filter(level==2, aggvar %in% c("totx.gen", "igr"), stabbr!="DC") %>%
  select(stabbr, year, aggvar, value) %>%
  filter(year >= 2004) %>%
  spread(aggvar, value) %>%
  mutate(ownx.gen=totx.gen - igr)
summary(pewx)

pewdf <- pewx %>%
  left_join(spi.a %>% select(stabbr, year, spi)) %>%
  mutate(xpi=ownx.gen / (spi * 1e6) * 100)
pewdf %>% filter(stabbr=="NY") # xpi is as a percentage

# get xpi in 2015, and change from 2005
pew.plotdf <- pewdf %>%
  select(stabbr, year, xpi) %>%
  filter(year %in% c(2005, 2015)) %>%
  mutate(year=paste0("xpi_", year)) %>%
  spread(year, xpi) %>%
  mutate(change=xpi_2015 - xpi_2005)

us2015 <- pew.plotdf %>% filter(stabbr=="US") %>% .[["xpi_2015"]]
uschange <- pew.plotdf %>% filter(stabbr=="US") %>% .[["change"]]

base.state <- "AZ"
base.state <- "OH"
comps <- nb$nstabbr[nb$stabbr==base.state]
outnote <- "\nNote: Outlier states of AK and ND excluded (Pew could include, with axis break)"
p <- pew.plotdf %>%
  filter(!stabbr %in% c("AK", "ND")) %>%
  mutate(stgroup=case_when(stabbr==base.state ~ "base",
                           stabbr %in% comps ~ "comp",
                           TRUE ~ "other")) %>%
  ggplot(aes(x=change, y=xpi_2015, label=stabbr)) +
  geom_text(aes(colour=stgroup, size=stgroup), fontface="bold") +
  scale_colour_manual(values=c("darkblue", "red", "darkgrey")) +
  scale_size_manual(values=c(4, 3.25, 2.75)) +
  geom_vline(xintercept=uschange) +
  geom_hline(yintercept=us2015) +
  theme_bw() +
  scale_y_continuous(name="Own-source spending as % of personal income, 2015", breaks=seq(0, 20, .5)) +
  scale_x_continuous(name="Change in own-source spending as % of personal income, 2005 to 2015", breaks=seq(-5, 5, .5)) +
  ggtitle("State government spending from own funds as % of personal income: 2005 and 2015",
          subtitle = paste0(getstname(base.state), " and its neighbors")) +
  theme(legend.position="none") +
  labs(caption=outnote) +
  theme(plot.caption = element_text(hjust=0, size=10))
p + scale_x_continuous(name="Change in own-source spending as % of personal income, 2005 to 2015",
                       breaks=seq(-5, 5, .5), limits=c(-4, 5)) + 
  coord_equal()

p
p + coord_equal()

# ggsave(plot=p, "./bopsalt_pop.png", width=12, height=3)


#****************************************************************************************************
#                Scatter with gross gen expends ####
#****************************************************************************************************
# create Pew's measure
genx <- slgfin %>%
  filter(level==2, aggvar=="totx.gen", stabbr!="DC") %>%
  select(stabbr, year, genx=value) %>%
  filter(year >= 2004)
summary(genx)

genxdf <- genx %>%
  left_join(spi.a %>% select(stabbr, year, spi)) %>%
  mutate(xpi=genx / (spi * 1e6) * 100)
genxdf %>% filter(stabbr=="NY") # xpi is as a percentage

# get xpi in 2015, and change from 2005
genx.plotdf <- genxdf %>%
  select(stabbr, year, xpi) %>%
  filter(year %in% c(2005, 2015)) %>%
  mutate(year=paste0("xpi_", year)) %>%
  spread(year, xpi) %>%
  mutate(change=xpi_2015 - xpi_2005)

us2015 <- genx.plotdf %>% filter(stabbr=="US") %>% .[["xpi_2015"]]
uschange <- genx.plotdf %>% filter(stabbr=="US") %>% .[["change"]]


base.state <- "AZ"
base.state <- "OH"
comps <- nb$nstabbr[nb$stabbr==base.state]
outnote <- "\nNote: Outlier states of AK and ND excluded (Pew could include, with axis break)"
p <- genx.plotdf %>%
  filter(!stabbr %in% c("AK", "ND")) %>%
  mutate(stgroup=case_when(stabbr==base.state ~ "base",
                           stabbr %in% comps ~ "comp",
                           TRUE ~ "other")) %>%
  ggplot(aes(x=change, y=xpi_2015, label=stabbr)) +
  geom_text(aes(colour=stgroup, size=stgroup), fontface="bold") +
  scale_colour_manual(values=c("darkblue", "red", "darkgrey")) +
  scale_size_manual(values=c(4, 3.25, 2.75)) +
  geom_vline(xintercept=uschange) +
  geom_hline(yintercept=us2015) +
  theme_bw() +
  scale_y_continuous(name="General expenditure as % of personal income, 2015", breaks=seq(0, 40, .5)) +
  scale_x_continuous(name="Change in general expenditure as % of personal income, 2005 to 2015", breaks=seq(-5, 5, .5)) +
  ggtitle("State government general expenditure as % of personal income: 2005 and 2015",
          subtitle = paste0(getstname(base.state), " and its neighbors")) +
  theme(legend.position="none") +
  labs(caption=outnote) +
  theme(plot.caption = element_text(hjust=0, size=10))
p
p + scale_x_continuous(name="Change in general expenditure as % of personal income, 2005 to 2015",
                       breaks=seq(-10, 10, .5), limits=c(-7, 8)) + 
  coord_equal()


#****************************************************************************************************
#                Compare own funds vs gen expends rankings ####
#****************************************************************************************************
glimpse(pew.plotdf)
glimpse(genx.plotdf)

comp <- bind_rows(
  pew.plotdf %>% mutate(measure="own"),
  genx.plotdf %>% mutate(measure="genx"))

compranks <- comp %>%
  filter(stabbr!="US") %>%
  group_by(measure) %>%
  mutate(x2015rank=rank(xpi_2015),
         changerank=rank(change)) %>%
  select(stabbr, measure, contains("rank")) %>%
  gather(variable, rank, -stabbr, -measure) %>%
  unite(vname, variable, measure) %>%
  spread(vname, rank) %>%
  mutate(changediff=changerank_genx - changerank_own,
         x2015diff=x2015rank_genx - x2015rank_own,
         totabsdiff=abs(changediff) + abs(x2015diff)) %>%
  arrange(-totabsdiff)
compranks


#****************************************************************************************************
#                Decompose genx ####
#****************************************************************************************************
glimpse(slgfin)

# totx.gen
# hiedx.gen
# k12x.gen
# corrx.gen
# highwayx.gen
# pwelfx.gen

vars <- c("totx.gen", "hiedx.gen", "k12x.gen", "corrx.gen", "highwayx.gen", "pwelfx.gen")
dcomp <- slgfin %>%
  filter(level==2, aggvar %in% vars, stabbr!="DC") %>%
  select(stabbr, year, aggvar, value) %>%
  filter(year %in% c(2005, 2015)) %>%
  left_join(spi.a %>% select(stabbr, year, spi)) %>%
  mutate(spi=spi * 1e6, xpi=value / spi * 100) %>%
  select(stabbr, year, aggvar, xpi) %>%
  spread(aggvar, xpi) %>%
  mutate_at(vars(-stabbr, -year), funs(naz)) %>%
  mutate(other.gen=totx.gen - hiedx.gen - k12x.gen - pwelfx.gen - corrx.gen - highwayx.gen) %>%
  gather(aggvar, value, -stabbr, -year) %>%
  spread(year, value) %>%
  mutate(change=`2015` - `2005`) %>%
  group_by(stabbr) %>%
  mutate(chgshare=change / change[aggvar=="totx.gen"] * 100)
glimpse(dcomp)

dcomp %>%
  filter(stabbr=="NY")

dcomp %>%
  select(stabbr, aggvar, chgshare) %>%
  group_by(aggvar) %>%
  mutate(chgshare.us=chgshare[stabbr=="US"],
         smus=chgshare - chgshare.us) %>%
  filter(stabbr %in% c("NY", "CA"))

dcomp %>%
  select(stabbr, aggvar, change) %>%
  group_by(aggvar) %>%
  mutate(change.us=change[stabbr=="US"],
         smus=change - change.us) %>%
  filter(stabbr %in% c("NY", "CA", "OH"))



# make bar graph, OH and US
ohus <- dcomp %>%
  filter(stabbr %in% c("OH", "US")) %>%
  select(stabbr, aggvar, change)
ohus


vorder <- c("totx.gen", "k12x.gen", "hiedx.gen", "pwelfx.gen", "corrx.gen", "highwayx.gen", "other.gen")
vnames <- c("Total", "K-12 education", "Higher education", "Public welfare", "Corrections", "Highways", "Other")
ohus %>%
  ungroup %>%
  mutate(state=getstname(stabbr),
         aggvarf=factor(aggvar, levels=vorder, labels=vnames)) %>%
  arrange(aggvarf) %>%
  ggplot(aes(aggvarf, change)) +
  geom_bar(aes(fill=state),
           stat="identity",
           width = .8, position = position_dodge(width=.8)) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(name="Change in general expenditure as percentage of personal income",
                     breaks=seq(-1, 1, .1)) +
  ggtitle("Change in general expenditures as percentage of personal income, 2005 to 2015") +
  scale_fill_manual(values=c("blue", "darkgreen")) +
  theme_bw() +
  theme(legend.position="right",
        legend.title = element_blank(),
        axis.title.x=element_blank())






#****************************************************************************************************
#                Compare xpi to LT averages ####
#****************************************************************************************************
glimpse(slgfin)
getgdppi()
gdppi <- getgdppi() %>% select(year, gdppi) %>%
  mutate(igdppi=gdppi[year==2015] / gdppi)
gdppi
# pop is in thousands, spi is in $ billions

# 
xvals <- slgfin %>%
  ungroup %>%
  filter(level==2, aggvar %in% c("totx.gen", "fedigr"), stabbr!="DC") %>%
  select(stabbr, year, aggvar, value) %>%
  spread(aggvar, value) %>%
  mutate(ownx.gen=totx.gen - fedigr) %>%
  left_join(spi.a %>% select(stabbr, year, spi, pop)) %>%
  left_join(gdppi %>% select(year, igdppi)) %>%
  mutate(xpi=ownx.gen / (spi * 1e6) * 100,
         xrpc=(ownx.gen / pop) * igdppi)
xvals %>% filter(stabbr=="NY")

peakyear <- xvals %>%
  filter(year %in% 2008:2011) %>%
  select(stabbr, year, xpi, xrpc) %>%
  group_by(stabbr) %>%
  mutate(xpi.peak=max(xpi),
         xrpc.peak=max(xrpc),
         xpi.peakyear=year[which(xpi==xpi.peak)],
         xrpc.peakyear=year[which(xrpc==xrpc.peak)])

peakyear <- xvals %>%
  filter(year %in% 2008:2011) %>%
  select(stabbr, year, xpi, xrpc) %>%
  group_by(stabbr) %>%
  mutate(xpi.peak=max(xpi),
         xrpc.peak=max(xrpc),
         xpi.peakyear=year[which.max(xpi)],
         xrpc.peakyear=year[which.max(xrpc)]) %>%
  filter(year==2008) %>%
  select(-year, -xpi, -xrpc)

  
xpeakavg <- xvals %>%
  filter(year>=1990) %>%
  left_join(peakyear, by="stabbr") %>%
  group_by(stabbr) %>%
  arrange(year) %>%
  mutate(xpi.avg10=mean(xpi[year>=2006]),
         xpi.avg20=mean(xpi[year>=1996]),
         xrpc.avg10=mean(xrpc[year>=2006]),
         xrpc.avg20=mean(xrpc[year>=1996])) %>%
  ungroup %>%
  arrange(stabbr, year)
ht(xpeakavg)

avgs <- xpeakavg %>%
  filter(year==2015)
avgs

xpeakavg %>%
  filter(stabbr %in% sts) %>%
  ggplot(aes(x=year, y=xpi)) +
  geom_line() + 
  geom_point() +
  geom_hline(aes(yintercept=xpi.avg10), data=avgs %>% filter(stabbr %in% sts),
             linetype="dashed",
             colour="blue") +
  geom_hline(aes(yintercept=xpi.avg20), data=avgs %>% filter(stabbr %in% sts),
             linetype="dashed",
             colour="red") +
  geom_hline(aes(yintercept=xpi), data=avgs %>% filter(stabbr %in% sts),
             linetype="dashed",
             colour="black") +
  facet_wrap(~stabbr, scales="free", ncol=3) +
  ggtitle("Own-source expenditures as % of personal income, selected states",
          subtitle="Blue=10-year average, Red=20-year average")

# table
xpeakavg %>%
  filter(year==2015) %>%
  mutate(stname=getstname(stabbr)) %>%
  select(stname, xpi, xpi.avg10, xpi.avg20, xpi.peak, xpi.peakyear) %>%
  mutate(chg.vs10yearavg=xpi - xpi.avg10,
         chg.vs20yearavg=xpi - xpi.avg20) %>%
  kable(digits=2)


xpeakavg %>%
  filter(stabbr %in% sts) %>%
  ggplot(aes(x=year, y=xrpc)) +
  geom_line() + 
  geom_point() +
  geom_hline(aes(yintercept=xrpc.avg10), data=avgs %>% filter(stabbr %in% sts),
             linetype="dashed",
             colour="blue") +
  geom_hline(aes(yintercept=xrpc.avg20), data=avgs %>% filter(stabbr %in% sts),
             linetype="dashed",
             colour="red") +
  geom_hline(aes(yintercept=xrpc), data=avgs %>% filter(stabbr %in% sts),
             linetype="dashed",
             colour="black") +
  facet_wrap(~stabbr, scales="free", ncol=3) +
  ggtitle("Own-source expenditures, real per capita, selected states",
          subtitle="Blue=10-year average, Red=20-year average")


# table
xpeakavg %>%
  filter(year==2015) %>%
  mutate(stname=getstname(stabbr)) %>%
  select(stname, xrpc, xrpc.avg10, xrpc.avg20, xrpc.peak, xrpc.peakyear) %>%
  mutate(chg.vs10yearavg=xrpc - xrpc.avg10,
         chg.vs20yearavg=xrpc - xrpc.avg20) %>%
  kable(digits=2)



#****************************************************************************************************
#                Compare xpi to NASBO pi ####
#****************************************************************************************************
glimpse(nasboxr)
count(nasboxr, xtype, fundtype, fundtypef)
count(nasboxr, stabbr)
count(nasboxr, purpose, purposef)

nasboxr %>% filter(stabbr=="NY", year==max(year), purpose=="totx")

nasbodf <- nasboxr %>%
  filter(stabbr %in% state.abb, xtype=="total", purpose=="totx", fundtype %in% c("af", "ff")) %>%
  select(stabbr, year, fundtype, value) %>%
  spread(fundtype, value) %>%
  mutate(nasbo=(af - ff) * 1000) %>%
  select(stabbr, year, nasbo)

cendf <- slgfin %>%
  ungroup %>%
  filter(level==2, aggvar %in% c("totx.gen", "fedigr"), stabbr %in% state.abb) %>%
  select(stabbr, year, aggvar, value) %>%
  spread(aggvar, value) %>%
  mutate(census=totx.gen - fedigr) %>%
  select(stabbr, year, census)

cenbo <- cendf %>%
  filter(year>=1991) %>%
  left_join(nasbodf) %>%
  left_join(spi.a %>% select(stabbr, year, spi)) %>%
  mutate(census=census / (spi * 1e6) * 100,
         nasbo=nasbo / (spi * 1e6) * 100) %>%
  select(-spi)

cenbo %>%
  filter(stabbr %in% sts) %>%
  gather(variable, value, -stabbr, -year) %>%
  ggplot(aes(x=year, y=value, colour=variable)) +
  geom_line() + 
  geom_point() + 
  scale_y_continuous(name=NULL, breaks=seq(-40, 40, 0.5)) +
  facet_wrap(~stabbr, scales="free", ncol=3) +
  ggtitle("Own-source expenditures as % of PI, selected states with diverse economic structures",
          subtitle="Census and NASBO")


cenbo %>%
  filter(stabbr %in% sts[1:9]) %>%
  gather(variable, value, -stabbr, -year) %>%
  #filter(value!=0) %>%
  ggplot(aes(x=year, y=value, colour=variable)) +
  geom_line() + 
  geom_point() + 
  scale_y_continuous(name=NULL, breaks=seq(-40, 40, 0.5)) +
  facet_wrap(~stabbr, scales="free", ncol=3) +
  ggtitle("Own-source expenditures as % of PI, selected states with diverse economic structures",
          subtitle="Census and NASBO")

