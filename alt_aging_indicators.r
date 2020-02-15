

# code folding ----
#   alt-L, alt-shift-L  one section
#   alt-O, alt-shift-O  all sections
#   alt-R run current code section (remapped from Ctrl-alt-T)



# libraries ----
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times
library("lubridate") # lubridate, for date/times
library("vctrs")

library("grDevices")
library("knitr")
library("kableExtra")

# library("btools")

library("DT") # for datatable

library("zoo") # for rollapply

library("broom") # for automating the cleanup of complex output
library("tsoutliers")

library("janitor")
library("btools")

# getdata ----
#. ONETIME: Annual pop by age ----
# https://factfinder.census.gov/bkmk/table/1.0/en/PEP/2018/PEPSYASEX?
# https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/
# PEP_2018_PEPSYASEX_with_ann.csv # annotations merged
fn <- "PEP_2018_PEPSYASEX_with_ann.csv"
vnames <- read_csv(here::here("data/popests/PEP_2018_PEPSYASEX/", fn), n_max = 0) %>% names
df <- read_csv(here::here("data/popests/PEP_2018_PEPSYASEX/", fn), col_names = vnames, skip=2) # skip 2 so that we just read data
ns(df) %>% ht
df[1:4, 1:5]
dim(df)

# long file
get_stabbr <- function(x){bdata::stcodes$stabbr[match(x, bdata::stcodes$stfips)]}
# get_stabbr(c("01", "02", "36"))
dfl <- df %>%
  rename(stname=`GEO.display-label`,
         stfips=GEO.id2) %>%
  select(-GEO.id) %>%
  pivot_longer(-c(stname, stfips)) %>%
  mutate(stfips=ifelse(stname=="United States", "00", stfips),
         stabbr=get_stabbr(stfips))
glimpse(dfl)
ht(dfl)
count(dfl, stabbr, stname, stfips)

# https://regexone.com/
# regex101.com

# dfl2 <- dfl %>%
#   mutate(src=str_extract(name, "(?:(?!sex).)*"))
#  mutate(src=str_split(name, "sex", simplify = TRUE)[, 1])

dfl2 <- dfl  %>%
  separate(col=name, into=c("src", "part2"), sep="(sex)", remove=FALSE) %>%
  separate(col=part2, into=c("sex", "part3"), remove=FALSE) %>%
  mutate(vtype=ifelse(part3 != "medage", "pop", "medage")) %>%
  separate(col=part3, into=c("part4", "age"), sep="age", remove=FALSE) %>%
  mutate(age=ifelse(age=="85plus", "85", age),
         age=as.integer(age),
         sex=as.integer(sex),
         year=str_sub(src, -4, -1) %>% as.integer,
         month=str_sub(src, -5, -5) %>% as.integer,
         src=str_sub(src, 1, 3))

glimpse(dfl2)
ht(dfl2)
count(dfl2, age) %>% ht # 85 is 85plus, 999 is total
count(dfl2, sex) # 0 is total
count(dfl2, year)
count(dfl2, month)
count(dfl2, src)
count(dfl2, vtype)

dfl3 <- dfl2 %>%
  select(stabbr, src, year, month, vtype, sex, age, value)
glimpse(dfl3)
saveRDS(dfl3, file=here::here("data", "popagesex.rds"))

pop <- readRDS(here::here("data", "popagesex.rds")) %>%
  filter(vtype=="pop", src=="est", sex==0, month==7) %>%
  select(stabbr, year, age, value)
glimpse(pop)
pop %>%
  filter(stabbr=="NY", age==999)
pop %>%
  filter(stabbr=="NY", year==2010)

#. ONETIME: USMDB life tables ----
#.. USMDB documentation ----
# The USMDB death rates and life tables have been computed as in the Human Mortality Database, following six steps, corresponding to each of six data types.
# Here is an overview of the process:

# Births. Annual counts of live births by sex have been collected for each population for all years since 1959. 
#   These counts have been used mainly for making population estimates at younger ages.
# Deaths. Death tabulations were built from the NCHS Mortality Files at the finest level of detail available and 
#   uniform methods were implemented to estimate death counts by completed age (i.e., age-last-birthday at time of death), calendar year
#   of death, and calendar year of birth.
# Population size. July 1st annual estimates of population size were obtained from the US Census Bureau for years since 1970 and inter-censal
#   population estimates were constructed for years 1959-1969 from a combination of US Census data, birth and death counts.
# Exposure-to-risk. Estimates of the population exposed to the risk of death during each age-time interval were estimated
#   from the annual population estimates, after correcting for the timing of deaths within each interval.
# Death rates. Death rates were computed as the ratios of the death counts for a given age-time interval divided by the estimate
#   of the exposure-to-risk in the same interval.
# Life tables. Probabilities of death were computed from death rates and used to construct all other life table functions,
#   including life expectancies and other useful indicators of mortality and longevity. 

# The life tables include the following columns
# Year     Calendar year or range of years of occurrence
# Age      Age group for n-year interval from exact age x to just before exact age x+n
# m(x)     Central death rate between age x and age x+n
# q(x)     Probability of death between age x and age x+n
# a(x)     Average length of survival between age x and age x+n for persons dying in the interval
# l(x)     Number of survivors at exact age x, assuming l(0) = 100,000
# d(x)     Number of deaths between age x and age x+n
# L(x)     Number of person-years lived between age x and age x+n
# T(x)     Number of person-years remaining after exact age x
# e(x)     Life expectancy at exact age x (in years) = remaining length of life for survivors to age x

#.. get USMDB ----
mydir <- here::here("data/lifetables/States")

df <- list.files(path=mydir, pattern="*_bltper_1x1.csv", full.names=TRUE, recursive=TRUE) %>% 
  map_df(~read_csv(.))
glimpse(df)
# popname, sex, year, age, mx, qx, ax, lx, dx, Lx, Tx, ex

df2 %>%
  filter(stabbr=="NY", year==2010, age<110) %>%
  ggplot(aes(x=mx, y=qx)) + geom_point() +
  geom_abline(slope=1)

df2 <- df %>%
  rename(stabbr=PopName, sex=Sex, year=Year, age=Age) %>%
  mutate(age=ifelse(age=="110+", "110", age),
         age=as.integer(age))
glimpse(df2)
df2

df2 %>%
  filter(age==atage) %>%
  filter(stabbr %in% sts) %>%
  ggplot(aes(year, mx, colour=stabbr)) +
  geom_line() +
  geom_point() +
  ggtitle(paste0("Death rate at age ", atage, ", selected states")) +
  scale_x_continuous(name=NULL, breaks=seq(1950, 2030, 5)) +
  scale_y_continuous(name="Death rate", breaks=seq(0, 0.1, .002)) +
  theme_bw()

saveRDS(df2, file=here::here("data", "usmdb.rds"))

#.. play with USMDB ----


df3 <- readRDS(file=here::here("data", "usmdb.rds")) %>%
  filter(age==65)

changes <- df3 %>%
  filter(year %in% c(1959, 2017)) %>%
  select(stabbr, year, ex) %>%
  pivot_wider(names_from = year, values_from = ex) %>%
  mutate(diff=`2017` - `1959`) %>%
  arrange(-diff)

sts <- c("NY", "HI", "OK", "MS")
atage <- 65
readRDS(file=here::here("data", "usmdb.rds")) %>%
  filter(age==atage) %>%
  filter(stabbr %in% sts) %>%
  ggplot(aes(year, ex, colour=stabbr)) +
  geom_line() +
  geom_point() +
  ggtitle(paste0("Life expectancy at age ", atage, ", selected states")) +
  scale_x_continuous(name=NULL, breaks=seq(1950, 2030, 5)) +
  scale_y_continuous(name="Life expectancy", breaks=seq(60, 120, 2)) +
  theme_bw()



rle15 <- df2 %>%
  group_by(stabbr, year) %>%
  mutate(i=min(which(ex < 15)) - 1) %>% # mutate(i=Position(function(x) x < 15, ex) - 1) %>%
  filter(row_number()==i) %>%
  ungroup

#  filter(ex>14, ex<16)

rle15 %>%
  filter(year==2017) %>% 
  select(-sex, -year, -Tx, -i) %>%
  arrange(-age) %>%
  mutate(le=age + ex)

df2 %>% filter(stabbr=="HI", year==2017, age >=60)

# analysis ----
usmdb <- readRDS(here::here("data", "usmdb.rds"))

pop <- readRDS(here::here("data", "popagesex.rds")) %>%
  filter(vtype=="pop", src=="est", sex==0, month==7) %>%
  select(stabbr, year, age, value)
glimpse(pop)
count(pop, age) %>% ht
count(pop, stabbr) # US, DC

# quick check -- good
pop %>%
  mutate(name=ifelse(age<999, "dtl", "tot")) %>%
  group_by(stabbr, year, name) %>%
  summarise(value=sum(value)) %>%
  pivot_wider() %>%
  filter(dtl!=tot)

pop %>%
  filter(stabbr=="NY", age==999)

pop %>%
  filter(stabbr=="NY", year==2010)

#.. age dep ratios ----
agedr <- pop %>%
  filter(age<999) %>%
  mutate(agroup=case_when(age < 21 ~ "young",
                          age >=65 ~ "old",
                          TRUE ~ "workage"),
         agroup=factor(agroup, levels=c("young", "workage", "old"))) %>%
  group_by(year, stabbr, agroup) %>%
  summarise(value=sum(value)) %>%
  pivot_wider(names_from = agroup) %>%
  ungroup %>%
  mutate(oadr=old / workage * 100,
         tdr=(old + young) / workage * 100)

agedr %>%
  filter(year==max(year), stabbr %in% c("US", state.abb)) %>%
  arrange(-oadr)

sts <- c("FL", "ME", "WV", "CO", "TX", "UT")
agedr %>%
  filter(stabbr %in% sts) %>%
  ggplot(aes(year, oadr, colour=stabbr)) +
  geom_line() +
  geom_point()

agedr %>%
  filter(stabbr %in% sts) %>%
  ggplot(aes(year, tdr, colour=stabbr)) +
  geom_line() +
  geom_point()

agedr %>%
  select(year, stabbr, oadr, tdr) %>%
  pivot_longer(cols=c(oadr, tdr), values_to = "state", names_to = "measure") %>%
  group_by(year, measure) %>%
  mutate(us=state[stabbr=="US"]) %>%
  pivot_longer(cols=c(state, us)) %>%
  filter(stabbr %in% sts) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~measure+stabbr, nrow=2, scales="free")

agedr_vus <- agedr %>%
  select(year, stabbr, oadr, tdr) %>%
  pivot_longer(cols=c(oadr, tdr), values_to = "state", names_to = "measure") %>%
  group_by(year, measure) %>%
  mutate(us=state[stabbr=="US"]) %>%
  pivot_longer(cols=c(state, us))

agedr_vus %>%
  filter(stabbr %in% sts, measure=="oadr") %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~stabbr, scales="fixed")
  

#. conform and then link the two data frames ----
glimpse(usmdb) # ex is main variable of interest
count(usmdb, age) %>% ht # 110 is highest age (110+), 0 is first age, there is no total age
count(usmdb, stabbr) # 50 states plus DC, no US, no PR
count(usmdb, year) %>% ht # 1959-2017


glimpse(pop)
count(pop, age) %>% ht # 0 is first age, 85 is highest age (85+), 999 is total age
count(pop, stabbr) # 50 states DC PR US = 53
count(pop, year) # 2010-2017


# create lowest common denominator file
usmdb2 <- usmdb %>%
  select(stabbr, year, age, lx, ex) %>%
  mutate(age=ifelse(age>=85, 85, age)) %>%
  group_by(stabbr, year, age) %>%
  summarise(lxtot=sum(lx),
         ex=sum(ex * lx) / lxtot) %>% # I'm not sure this is the best way to collapse the 85+ group....
  ungroup
usmdb2 %>% filter(stabbr=="NY", year==2016, age >= 70)

usmdb2 %>%
  filter(year==2017, age==65)

# c("HI", "NJ", "LA", "IN")
sts <- c("NY", "MS")
usmdb2 %>%
  filter(stabbr %in% sts, age==65) %>%
  ggplot(aes(year, ex, colour=stabbr)) +
  geom_line() +
  geom_point()

# get rle15- by interpolation
rle <- usmdb2 %>%
  # filter(stabbr=="CA", year==2016, age>=50) %>%
  arrange(stabbr, year, ex) %>%
  group_by(stabbr, year) %>%
  mutate(xmin=min(which(ex>15)),
         xmax=max(which(ex<15))) %>%
  filter(row_number() %in% c(xmin[1], xmax[1])) %>%
  select(stabbr, year, age, ex) %>%
  mutate(ex_part=15 - min(ex),
         ex_full=max(ex) - min(ex),
         rle=ex_part / ex_full + min(age)) %>%
  filter(row_number()==1) %>%
  ungroup
rle %>% filter(stabbr=="NY")
rle %>% filter(year==2017) %>% arrange(-rle)

rle %>%
  filter(stabbr %in% c("HI", "NJ", "LA", "IN")) %>%
  ggplot(aes(year, rle, colour=stabbr)) +
  geom_line() +
  geom_point()


count(usmdb2, age) %>% ht

pop2 <- pop %>%
  filter(stabbr %in% c(state.abb, "DC")) %>%
  filter(age != 999, year<=2017) %>%
  rename(pop=value)
pop2 %>% filter(stabbr=="NY", year==2016, age >= 70)

# lcd <- pop2 %>%
#   left_join(usmdb2)
# glimpse(lcd)
# ht(lcd)
# count(lcd, year)
# # check -- good
# lcd %>%
#   group_by(stabbr, year) %>%
#   summarise(pop=sum(pop)) %>%
#   filter(stabbr=="NY")

lcd <- pop2 %>%
  left_join(rle %>% select(stabbr, year, rle), by = c("stabbr", "year")) %>%
  mutate(pop65p=pop * (age >= 65),
         share_rle=ifelse(age > rle, pmin((age - rle), 1), 0),
         pop_rle=pop * share_rle,
         pop_active=ifelse(age <= 21, 0, pop - pop_rle)) %>%
  arrange(year, stabbr, age)

lcd %>% filter(stabbr=="NY", year==2017, age %in% c(15:25, 60:85))

rle15 <- lcd %>%
  arrange(stabbr, year, age) %>%
  group_by(stabbr, year) %>%
  summarise_at(vars(pop, pop65p, pop_rle, pop_active), ~sum(.)) %>%
  mutate(oadr=pop65p / pop * 100,
         prle15=pop_rle / pop * 100,
         poadr=pop_rle / pop_active * 100)
rle15 %>% filter(stabbr=="NY")

rle15 %>% filter(year==2017) %>% arrange(-prle15)

rle15 %>%
  filter(stabbr %in% c("MS", "NY")) %>%
  ggplot(aes(year, prle15, colour=stabbr)) +
  geom_line() +
  geom_point()

rle15 %>%
  filter(stabbr %in% c("MS", "NY")) %>%
  ggplot(aes(year, poadr, colour=stabbr)) +
  geom_line() +
  geom_point()

rle15 %>%
  select(stabbr, year, oadr, prle15, poadr) %>%
  pivot_longer(cols=-c(stabbr, year)) %>%
  filter(stabbr %in% c("MS", "NY")) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~stabbr)



# calculate and graph the weighted remaining life expectancy
rle15 <- lcd %>%
  arrange(stabbr, year, age) %>%
  group_by(stabbr, year) %>%
  mutate(i=min(which(ex < 15)) - 1,  # which ages have <= 15 expected years left?
         rn=row_number(),
         rle15=pop * (rn > i),
         wap=pop * (age >20 & age<65))
rle15 %>%
  filter(stabbr=="NY", year==2017, age<10 | age>=40)

check <- rle15 %>%
  group_by(stabbr, year) %>%
  summarise(poptot=sum(pop), pop65p=sum(pop * (age >=65)), rle15=sum(rle15), wap=sum(wap)) %>%
  mutate(rlepct=rle15 / poptot * 100,
         pop65pct=pop65p / poptot * 100,
         oadr=pop65p / wap * 100) %>%
  ungroup

check %>%
  filter(stabbr %in% c("MS", "NY")) %>%
  ggplot(aes(year, rlepct, colour=stabbr)) +
  geom_line() +
  geom_point()

check %>%
  filter(year==2017) %>%
  arrange(-rlepct)

check %>%
  filter(stabbr %in% c("MS", "NY")) %>%
  ggplot(aes(year, pop65pct, colour=stabbr)) +
  geom_line() +
  geom_point()

check %>%
  filter(year==2017) %>%
  ggplot(aes(pop65pct, rlepct)) +
  geom_point(size=.5) +
  geom_hline(aes(yintercept = y), data=. %>% summarise(y= median(rlepct))) +
  geom_vline(aes(xintercept = x), data=. %>% summarise(x= median(pop65pct))) +
  geom_text(aes(label=stabbr), size=3, colour="blue") +
  theme_bw()
 
check %>%
  filter(year==2017) %>%
  ggplot(aes(oadr, rlepct)) +
  geom_point(size=.5) +
  geom_hline(aes(yintercept = y), data=. %>% summarise(y= median(rlepct))) +
  geom_vline(aes(xintercept = x), data=. %>% summarise(x= median(oadr))) +
  geom_text(aes(label=stabbr), size=3, colour="blue") +
  theme_bw()


# ACS disability ----
# https://www.census.gov/programs-surveys/acs/technical-documentation/pums/documentation.html

library("DBI")

dbdir <- "D:/Data/CensusACS/RSQLite1YearDB/"
dbf <- paste0(dbdir, "acs_1year.sqlite")

con <- DBI::dbConnect(RSQLite::SQLite(), dbf)

tname <- "p2018"

dbListTables(con)
dbListFields(con, tname)

selstring <- paste0("SELECT * FROM ", tname, " LIMIT 5")
tmp <- dbGetQuery(con, selstring)
glimpse(tmp)

selstring <- paste0("SELECT COUNT(*) FROM ", tname)
dbGetQuery(con, selstring )

# DIS Character 1
# Disability recode
# 1 .With a disability
# 2 .Without a disability

# institutionalized group quarters relp==16

# vars <- c("st", "pwgtp", "agep", "dis")
# vars <- expression(st, pwgtp, agep, dis)
# bquote(st, pwgtp, agep, dis)
# paste0("a", vars)

(selstring <- paste0("SELECT ", vars, " FROM ", tname, " LIMIT 5"))
(selstring <- paste0("SELECT st, pwgtp, agep, dis FROM ", tname, " LIMIT 5"))
dbGetQuery(con, selstring )

f <- function(year){
  tname <- paste0("p", year)
  # selstring <- paste0("SELECT st, pwgtp, agep, dis FROM ", tname, " LIMIT 5")
  selstring <- paste0("SELECT st, pwgtp, agep, relp, dis FROM ", tname)
  df <- dbGetQuery(con, selstring ) %>%
    mutate(year=year)
  return(df)
}

system.time(df <- ldply(2010:2018, f))
glimpse(df)
count(df, year)
summary(df)

a <- proc.time()
df2 <- df %>%
  mutate(agroup=case_when(agep < 21 ~ "young",
                          agep >= 21 & agep < 65 ~ "workage",
                          agep >= 65 ~ "old",
                          TRUE ~ "error")) %>%
  mutate(stabbr=stcodes$stabbr[match(st, as.integer(stcodes$stfips))],
         igq=(relp==16)) %>%
  group_by(year, stabbr, igq, agroup, dis) %>%
  summarise(n=n(), pop=sum(pwgtp)) %>%
  ungroup
b <- proc.time()
b - a

saveRDS(df2, here::here("data", "age_disb.rds"))

df2 <- readRDS(here::here("data", "age_disb.rds"))
glimpse(df2)
# get total rates of disability
df2 %>%
  filter(!igq) %>%
  group_by(year, stabbr) %>%
  summarise(n=sum(n), dpop=sum(pop * (dis==1)), pop=sum(pop)) %>%
  mutate(drate=dpop / pop * 100) %>%
  filter(stabbr=="AL") 

df3 <- df2 %>% 
  ungroup %>%
  select(-n) %>%
  pivot_wider(names_from = agroup, values_from = pop) %>%
  group_by(year, stabbr, dis) %>%
  mutate(totpop=old + workage + young) %>%
  ungroup

df3 %>% filter(year==2017, stabbr=="AL")




dwide <- df3 %>%
  select(year, dis, stabbr, old) %>%
  mutate(dis=factor(dis, levels=1:2, labels=c("disabled", "healthy"))) %>%
  pivot_wider(names_from = dis, values_from = old) %>%
  mutate(dpct=disabled / (disabled + healthy) * 100)



dwide %>%
  filter(year==2018) %>%
  arrange(-dpct)

sts <- c("OK", "AR", "WV", "CT", "NJ", "DE")
dwide %>%
  filter(stabbr %in% sts) %>%
  ggplot(aes(year, dpct, colour=stabbr)) +
  geom_line() +
  geom_point()


# getall <- tbl(acsdb, tname) # dplyr does lazy loading, so this does not really grab full table
# # getall <- tbl(acsdb, sql("SELECT * FROM acs2016_5year")) # dplyr does lazy loading, so this does not really grab full table
# str(getall)
# glimpse(getall)

getall %>% summarise(pop=sum(pwgtp)) # warning

df <- getall %>%
  select(pwgtp)

df %>% summarise(pop=sum(pwgtp, na.rm=TRUE))




dbDisconnect(con)
# dbDisconnect(con, shutdown=TRUE)



# EDR economic dependency ratio ----
#. get LAUS data ----
# url <- "https://download.bls.gov/pub/time.series/la/la.data.3.AllStatesS" # SA state data
urlbase <- "https://download.bls.gov/pub/time.series/la/"
fn <- "la.data.3.AllStatesS" # SA state data
download.file(paste0(urlbase, fn), here::here("data/bls", fn), mode="wb")

# series_id                     	year	period	       value	footnote_codes
# LASST010000000000003          	1976	M01	         6.7	

# area_type_code	area_code	area_text	display_level	selectable	sort_sequence
# A	ST0100000000000	Alabama	0	T	1
# A	ST0200000000000	Alaska	0	T	146

# measure_code	measure_text
# 03	unemployment rate
# 04	unemployment
# 05	employment
# 06	labor force

#. NILF and LF by age ----
library(readxl)
library(bdata)

# table14full18.xlsx
y2 <- "03"
y2 <- 18
df <- read_excel(here::here("data/bls", paste0("table14full", y2, ".xlsx")))
glimpse(df)
ht(df)
df[3:10, ]
df[3:5, ]
# stfips	group_code	stname	group	civnipop	civlf	lfpct	civemp	emppct	civunemp	unemppct	eratelow	junk	eratehigh # 2003

f <- function(year){
  vnames <- c("stfips", "group_code", "stname", "group", "civnipop", "civlf", "lfpct", "civemp", "emppct", "civunemp", "unemppct")
  y2 <- str_sub(year, 3, 4)
  df <- read_excel(here::here("data/bls", paste0("table14full", y2, ".xlsx")))
  df <- df[-c(1:7), 1:11] %>%
    setNames(vnames) %>%
    mutate(year=year) %>%
    filter(!is.na(stname))
  df
}

df <- ldply(2003:2018, f)
glimpse(df)

count(df, year)
count(df, stfips, stname) # 50 states plus DC
count(df, group_code, group)

df2 <- df %>%
  filter(str_detect(group, "Total,")) %>%
  mutate(stabbr=stcodes$stabbr[match(stfips, stcodes$stfips)]) %>%
  select(year, stabbr, stname, group_code, group, civnipop, civlf, civemp, civunemp) %>%
  mutate_at(vars(c(group_code, civnipop, civlf, civemp, civunemp)), ~as.numeric(.))

glimpse(df2)
count(df2, stabbr, stname)
count(df2, group_code, group)

df3 <- df2 %>%
  mutate(youngold=ifelse(group_code <25, "young", "old")) %>%
  group_by(year, stabbr, youngold) %>%
  summarise_at(vars(civnipop, civlf, civemp), ~sum(., na.rm=TRUE)) %>%
  group_by(year, stabbr) %>%
  mutate(nilf=civnipop - civlf,
         totpop=sum(civnipop),
         totlf=sum(civlf),
         adr=nilf / totlf * 100) %>%
  ungroup
df3
count(df2, stabbr)
df2 %>%
  filter(group_code==25) %>%
  group_by(year) %>%
  summarise(ns=length(unique(stabbr)))

tmp <- df3 %>%
  filter(youngold=="old", year==2018) %>%
  arrange(-adr)

setdiff(state.abb, tmp$stabbr) # SD is missing
df3 %>%
  filter(youngold=="old", year==2015) %>%
  {setdiff(state.abb, .$stabbr)}

tmp <- df3 %>%
  group_by(youngold, year) %>%
  select(stabbr) %>%
  distinct() %>%
  summarise(mstate=setdiff(state.abb, stabbr))


df3 %>%
  filter(youngold=="old", year==2018) %>%
  arrange(-adr)

p <- df3 %>%
  filter(youngold=="old") %>%
  filter(stabbr %in% c("WV", "SC", "MA", "UT")) %>%
  ggplot(aes(year, adr, colour=stabbr)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(name="Adult 65+ dependency ratio", breaks=seq(0, 50, 5), limits=c(0, NA)) +
  scale_x_continuous(name=NULL, breaks=seq(2000, 2020, 2)) +
  geom_hline(yintercept = 0) +
  ggtitle("Inactive old (age 65+, not in labor force) as % of Active adults (age 20+, in labor force)",
          subtitle="Two states with high dependency ratios (SC and WV), and two with low ratios (MA and UT)") +
  theme_bw()
p
