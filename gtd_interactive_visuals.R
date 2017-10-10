# Load packages
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(purrr)
library(purrrlyr)
library(tidyverse)
library(reshape2)
library(caret) # For data partition
library(plotly) # For interactive mapping and visuals
library(leaflet)
library(cowplot)
library(ggthemes)
library(viridis)
library(knitr)
library(scales)
library(gridExtra)
library(MASS)
library(ggmap)
library(forcats)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggrepel)
db <- read.csv("globalterrorismdb_0616dist.csv", na.string = c("", "NA", " "))
dt <- db %>%
  dplyr::select(eventid, iyear, imonth, iday, extended, summary, doubtterr, multiple, related, country_txt, region_txt, provstate, latitude, longitude, attacktype1_txt, attacktype2_txt, attacktype3_txt, success, suicide, weaptype1_txt, weaptype2_txt, weaptype3_txt, weaptype4_txt, target1, targtype1_txt, natlty1_txt, target2, targtype2_txt, natlty2_txt, target3, targtype3_txt,natlty3_txt, gname, gname2, gname3, guncertain1, guncertain2, guncertain3, nperps, nperpcap, claimed, compclaim, motive, nkill, nkillter, nwound, nwoundte, property, propextent, ishostkid, nhostkid, INT_LOG, INT_IDEO, INT_MISC, INT_ANY) %>% 
  filter(doubtterr == 0, country_txt == "United States")
dt_w <- db %>%
  dplyr::select(eventid, iyear, imonth, iday, extended, summary, doubtterr, multiple, related, country_txt, region_txt, provstate, latitude, longitude, attacktype1_txt, attacktype2_txt, attacktype3_txt, success, suicide, weaptype1_txt, weaptype2_txt, weaptype3_txt, weaptype4_txt, target1, targtype1_txt, natlty1_txt, target2, targtype2_txt, natlty2_txt, target3, targtype3_txt,natlty3_txt, gname, gname2, gname3, guncertain1, guncertain2, guncertain3, nperps, nperpcap, claimed, compclaim, motive, nkill, nkillter, nwound, nwoundte, property, propextent, ishostkid, nhostkid, INT_LOG, INT_IDEO, INT_MISC, INT_ANY) %>% 
  filter(doubtterr == 0) # 217 is the country code for United States

# Change variables with strings back to character vector
dt$summary <- as.character(dt$summary)
dt$target1 <- as.character(dt$target1)
dt$target2 <- as.character(dt$target2)
dt$target3 <- as.character(dt$target3)

# Trim factor variables to only have included levels
dt <- dt %>% dmap_if(is.factor, fct_drop)

# Recode -9 and -99 as NA in the dataframe
dt[dt == -9 | dt == -99] <- NA

# Recode factors "." and "Unknown" into NA in the factors
for (i in 1:ncol(dt)){
  if (is.factor(dt[,i])){
    levels(dt[,i]) <- sub("^.$", NA, levels(dt[,i]))
    levels(dt[,i]) <- sub("Unknown", NA, levels(dt[,i]))
  }
}

# Run the same codes on the world reference dataframe
# Change variables with strings back to character vector
dt_w$summary <- as.character(dt_w$summary)
dt_w$target1 <- as.character(dt_w$target1)
dt_w$target2 <- as.character(dt_w$target2)
dt_w$target3 <- as.character(dt_w$target3)
# Trim factor variables to only have included levels
dt_w <- dt_w %>% dmap_if(is.factor, fct_drop)
# Recode -9 and -99 as NA in the dataframe
dt_w[dt_w == -9 | dt_w == -99] <- NA
# Recode factors "." and "Unknown" into NA in the factors
for (i in 1:ncol(dt_w)){
  if (is.factor(dt_w[,i])){
    levels(dt_w[,i]) <- sub("^.$", NA, levels(dt_w[,i]))
    levels(dt_w[,i]) <- sub("Unknown", NA, levels(dt_w[,i]))
  }
}

# Replace unknown days (0 according to the code book) with 1
dt$iday <- as.integer(gsub(0, 1, dt$iday))

# Create a new variable "idate"
dt$idate <- as.Date(paste0(dt$iyear,
                           stringr::str_pad(as.character(dt$imonth), width = 2, side = "left", pad = "0"),
                           stringr::str_pad(as.character(dt$iday), width = 2, side = "left", pad = "0")), "%Y%m%d")
# There are 23 incidents where the date object fails to show
# a closer look finds that these are incidents where iday = 31
# when the month actually won't have 31 days.
# We will treat these idays as 30, i.e. end of the month
dt$iday[is.na(dt$idate)] <- 30
dt$idate <- as.Date(paste0(dt$iyear,
                           stringr::str_pad(as.character(dt$imonth), width = 2, side = "left", pad = "0"),
                           stringr::str_pad(as.character(dt$iday), width = 2, side = "left", pad = "0")), "%Y%m%d")

# Repeat Same Step for the World Refernce Dataframe
# Replace unknown days (0 according to the code book) with 1
dt_w$iday <- as.integer(gsub(0, 1, dt_w$iday))

# Create a new variable "idate"
dt_w$idate <- as.Date(paste0(dt_w$iyear,
                             stringr::str_pad(as.character(dt_w$imonth), width = 2, side = "left", pad = "0"),
                             stringr::str_pad(as.character(dt_w$iday), width = 2, side = "left", pad = "0")), "%Y%m%d")
# There are 23 incidents where the date object fails to show
# a closer look finds that these are incidents where iday = 31
# when the month actually won't have 31 days.
# We will treat these idays as 30, i.e. end of the month
dt_w$iday[is.na(dt_w$idate)] <- 30
dt_w$idate <- as.Date(paste0(dt_w$iyear,
                             stringr::str_pad(as.character(dt_w$imonth), width = 2, side = "left", pad = "0"),
                             stringr::str_pad(as.character(dt_w$iday), width = 2, side = "left", pad = "0")), "%Y%m%d")


# Generate a dataframe for time by type
dt3 <- dt %>%
  dplyr::select(eventid, iyear, targtype1_txt, targtype2_txt,
                targtype3_txt, attacktype1_txt, attacktype2_txt, attacktype3_txt) %>%
  melt(id.vars =c('eventid','iyear', 'attacktype1_txt', 'attacktype2_txt','attacktype3_txt'), 
       measure.vars = c('targtype1_txt','targtype2_txt', 'targtype3_txt'),
       na.rm = TRUE, value.name = "targtype_txt") %>%
  dplyr::select(-variable) %>%
  melt(id.vars = c('eventid','iyear','targtype_txt'),
       measure.vars = c('attacktype1_txt', 'attacktype2_txt','attacktype3_txt'),
       na.rm = TRUE, value.name = "attacktype_txt") %>%
  dplyr::select(-variable) %>%
  mutate(targtype_txt = factor(
    ifelse(targtype_txt=="Business","Business",
           ifelse(targtype_txt=="Government (General)","Government (General)",
                  ifelse(targtype_txt=="Private Citizens & Property", "Private Citizens & Property","Other")))),
    attacktype_txt = factor(
      ifelse(attacktype_txt=="Bombing/Explosion", "Bombing/Explosion",
             ifelse(attacktype_txt=="Facility/Infrastructure Attack", "Facility/Infrastructure Attack",
                    "Other"))))
# Look at the attack by attack type over the years
g10 <- dt3 %>% mutate(attack_type = attacktype_txt) %>% dplyr::group_by(iyear, attack_type) %>% summarise(n_incident = n()) %>%
  ggplot(aes(x=iyear, y = n_incident, group = attack_type, color = attack_type, 
             text = paste(attack_type, "\n Count", n_incident))) + geom_line()+
  xlab("") + ylab("Occurence of Attacks") + ggtitle("Attack Occurence by Attack Type") +
  theme_light() + theme(legend.position = c(0.85, 0.83),
                        legend.title = element_blank(),
                        legend.text = element_text(size = 12),
                        legend.background = element_blank(),
                        plot.title = element_text(size=15))
ggplotly(g10, tooltip = c("text")) %>% layout(dragmode = 'pan')

# Look at the attack by target type over the years
g11 <- dt3 %>% mutate(target_type = targtype_txt) %>% dplyr::group_by(iyear, target_type) %>% summarise(n_incident = n()) %>%
  ggplot(aes(x=iyear, y = n_incident, group = target_type, color = target_type, 
             text = paste(target_type, '\n Count:', n_incident))) + geom_line()+
  xlab("") + ylab("Occurence of Attacks") + ggtitle("Attack Occurence by Target Type") +
  theme_light() + theme(legend.position = c(0.85, 0.83),
                        legend.title = element_blank(),
                        legend.text = element_text(size = 12),
                        legend.background = element_blank(),
                        plot.title = element_text(size=15))
ggplotly(g11, tooltip = c('text')) %>% layout(dragmode = 'pan')

# Generate a dataframe for time by type
dt4 <- dt %>% 
  dplyr::select(eventid, iyear, targtype1_txt, targtype2_txt,
                targtype3_txt, attacktype1_txt, attacktype2_txt, attacktype3_txt) %>%
  melt(id.vars =c('eventid','iyear', 'attacktype1_txt', 'attacktype2_txt','attacktype3_txt'), 
       measure.vars = c('targtype1_txt','targtype2_txt', 'targtype3_txt'),
       na.rm = TRUE, value.name = "targtype_txt") %>%
  dplyr::select(-variable) %>%
  melt(id.vars = c('eventid','iyear','targtype_txt'),
       measure.vars = c('attacktype1_txt', 'attacktype2_txt','attacktype3_txt'),
       na.rm = TRUE, value.name = "attacktype_txt") %>%
  dplyr::select(-variable) %>% mutate(targtype_txt = factor(targtype_txt),
                                      attacktype_txt = factor(attacktype_txt)) %>%
  filter(iyear >= 2000)

# Explore the frequent attacktype in recent years
g12 <- dt4 %>% dplyr::group_by(iyear, attacktype_txt) %>% summarise(n_incident = n()) %>%
  ggplot(aes(x=iyear, y = n_incident, group = attacktype_txt, color = attacktype_txt,
             text = paste(attacktype_txt, '\n Count:', n_incident))) + geom_line()+
  xlab("") + ylab("Occurence of Attacks") + ggtitle("Attack Occurence by Attack Type (Since 2000)") +
  theme_light() + theme(legend.position = c(0.82, 0.80),
                        legend.title = element_blank(),
                        legend.text = element_text(size = 12),
                        legend.background = element_blank(),
                        plot.title = element_text(size=15))
ggplotly(g12, tooltip = c('text')) %>% layout(dragmode = 'pan')

# Explore the most frequent target type in recent years
g13 <- dt4 %>% dplyr::group_by(iyear, targtype_txt) %>% summarise(n_incident = n()) %>%
  ggplot(aes(x=iyear, y = n_incident, group = targtype_txt, color = targtype_txt,
             text = paste(targtype_txt, "\n Count", n_incident))) + geom_line()+
  xlab("") + ylab("Occurence of Attacks") + ggtitle("Attack Occurence by Target Type (Since 2000)") +
  theme_light() + theme(legend.title = element_blank(),
                        legend.text = element_text(size = 12),
                        legend.background = element_blank(),
                        plot.title = element_text(size=15))
ggplotly(g13, tooltip = c('text')) %>% layout(dragmode = 'pan')

# Generate geolocation-friendly dataframe for the analysis
dt5 <- dt %>% mutate(iweekday = factor(weekdays(dt$idate),
                                       levels = c('Sunday','Monday','Tuesday',
                                                  'Wednesday','Thursday','Friday','Saturday')),
                     imonth = factor(imonth),
                     nkill = ifelse(is.na(nkill), 0, nkill),
                     nwound = ifelse(is.na(nwound), 0, nwound),
                     n_killwound = nkill + nwound, 
                     INT_ANY = factor(ifelse(is.na(INT_ANY), "Not Sure", 
                                             ifelse(INT_ANY==0, "No Foreign Connections","Has Foreign Connections" ))),
                     idecades = factor(ifelse(iyear >= 2000, "Since 2000", 
                                              ifelse(iyear >= 1990,  "1990s","1970~1980s"))))
