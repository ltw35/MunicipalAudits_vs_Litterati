# Municipal audits vs. citizen science for understanding urban litter patterns
# Analysis for comparing Vancouver, B.C. Municipal Audits to Litterati submissions for same time/space (2017-2019)

# Data shared by Litterati and transcribed from Vancouver pdf litter audits
# Code written by Lisa Watkins (ltw35@cornell.edu)
# Code started Dec 27, 2021

### FILES REQUIRED
## Litterati files, manipulated in QGIS by Lisa (see parentheticals for how)
# data_Litterati\\vancouver_litteraticensusareastreettype_qgis.csv (spatial join between 2017-2019 litterati data + Vancouver census areas downloaded from https://opendata.vancouver.ca/explore/dataset/local-area-boundary/ + Vancouver street types downloaded from https://opendata.vancouver.ca/explore/dataset/public-streets/export/)
# data_Litterati\\Vancouver_litterati_withlocalareaid_qgis.csv (spatial join between 2017-2019 Litterati data + Vancouver census areas downloaded from https://opendata.vancouver.ca/explore/dataset/local-area-boundary/ )
# data_Litterati\\litterati_nomapid_butinlimits_qgis.csv (2017-2019 Litterati data spatially joined in QGIS with Vancouver census areas downloaded and found to not fallwithin census area, but within nearby city park. Goal is to keep these large, nearby clusters to further explore)

## Audit files, transcribed from pdf audit reports
# data_municipalaudits\\Vancouver_2017_site_characteristics_transcribed.csv
# data_municipalaudits\\Vancouver_2018_site_characteristics_transcribed.csv
# data_municipalaudits\\Vancouver_2019_site_characteristics_transcribed.csv
# data_municipalaudits\\Vancouver_2017_site_large_count.csv
# data_municipalaudits\\Vancouver_2018_site_large_count.csv
# data_municipalaudits\\Vancouver_2019_site_large_count.csv
# data_municipalaudits\\Vancouver_2017_site_small_count.csv
# data_municipalaudits\\Vancouver_2018_site_small_count.csv
# data_municipalaudits\\Vancouver_2019_site_small_count.csv
# data_municipalaudits\\Vancouver_localarea_municipalauditlocations_qgis.csv (hand-drawn into QGIS shapefile layer based on pdf map in audit reports + spatial join with Vancouver census areas downloaded from https://opendata.vancouver.ca/explore/dataset/local-area-boundary/)

## supplementary data files for correlations
# data_iNaturalist\\inaturalist_censusarea.csv (all iNaturalist submissions in the Vancouver area from 2017-2019 as downloaded from inaturalist.org, spatially joined in QGIS with Vancouver census areas downloaded from https://opendata.vancouver.ca/explore/dataset/local-area-boundary/)
# data_Vancouver311\\201709CaseLocationsDetails.csv (downloaded from https://opendata.vancouver.ca/explore/dataset/3-1-1-contact-centre-metrics/information/)
# data_Vancouver311\\201709CaseLocationsDetails.csv (see above)
# data_Vancouver311\\201709CaseLocationsDetails.csv (see above)
# data_demographics\\Vancouver_localarea_demographics_download.csv


# Load Libraries
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(tibble) # rownames_to_column
library(janitor) # row_to_names
library(corrplot) #corrplot()
#library(lme4) # for the glmm ??or should I be using lmerTest()??
#library(lmerTest)
#library(glmnet) # for lasso + cross validation
#library(jtools) # for effect_plot()
#library(effects) # for effect()
library(scales) # for label_number()
library(car) # for calculating




# READ IN ORIGINAL DATAFILES-------

## Litterati data for Vancouver metro area, years 2017-2019

litt_streets_temp = read.csv("data_Litterati\\vancouver_litteraticensusareastreettype_qgis.csv") %>% select(id,hblock,streetuse) %>% mutate(streetuse = ifelse(streetuse == "",NA,streetuse), hblock = ifelse(hblock =="",NA,hblock))
# THE LITTERATI DATA
van_litterati = read.csv('data_Litterati\\Vancouver_litterati_withlocalareaid_qgis.csv') %>% 
  mutate(time = as.Date(time)) %>% 
  select(-url, - display_name, -country_code) %>% 
  mutate(year = year(time), month = month(time), day = day(time)) %>% 
  filter(year == 2017| year == 2018| year == 2019) %>% 
  left_join(litt_streets_temp, by = "id") %>% 
  left_join(read.csv("data_Litterati\\litterati_nomapid_butinlimits_qgis.csv")[ ,c('field_1','id')], by = "id") %>% mutate(mapid = ifelse(mapid ==""& !is.na(field_1),"suburb",mapid)) %>% select(-field_1) %>% filter(mapid!="") %>% 
  distinct(id,.keep_all= TRUE)

  
## Municipal audits for Vancouver, september 2017, 2018, 2019
     #Note: 2019 municipal audit lacks large counts for sites 73 & 46 and small counts for sites 91 & 94, with no notes in report as to why. It also has 0.5 counts, but I'm not sure what a half an item means or how this is calculated.

### join large count, small count, and site characteristics; join each year; join site to municipal area name
van_municipal_2017_temp = read.csv("data_municipalaudits\\Vancouver_2017_site_characteristics_transcribed.csv", fileEncoding="UTF-8-BOM") %>% left_join(read.csv("data_municipalaudits\\Vancouver_2017_site_large_count.csv", fileEncoding="UTF-8-BOM"), by = "siteid") %>% left_join(read.csv("data_municipalaudits\\Vancouver_2017_site_small_count.csv", fileEncoding="UTF-8-BOM"), by = "siteid") %>% mutate(year = 2017, month = 9)
van_municipal_2018_temp = read.csv("data_municipalaudits\\Vancouver_2018_site_characteristics_transcribed.csv", fileEncoding="UTF-8-BOM") %>% left_join(read.csv("data_municipalaudits\\Vancouver_2018_site_large_count.csv", fileEncoding="UTF-8-BOM"), by = "siteid") %>% left_join(read.csv("data_municipalaudits\\Vancouver_2018_site_small_count.csv", fileEncoding="UTF-8-BOM"), by = "siteid") %>% mutate(year = 2018, month = 9)
van_municipal_2019_temp = read.csv("data_municipalaudits\\Vancouver_2019_site_characteristics_transcribed.csv", fileEncoding="UTF-8-BOM") %>% left_join(read.csv("data_municipalaudits\\Vancouver_2019_site_large_count.csv", fileEncoding="UTF-8-BOM"), by = "siteid") %>% left_join(read.csv("data_municipalaudits\\Vancouver_2019_site_small_count.csv", fileEncoding="UTF-8-BOM"), by = "siteid") %>%  mutate(year = 2019, month = 9)

van_municipal_transcribed_temp = van_municipal_2017_temp %>% rbind(van_municipal_2018_temp) %>% rbind(van_municipal_2019_temp) %>% 
  mutate(sitearea_m2_large = area_ft2_large/10.7639, sitearea_m2_small = area_ft2_small/10.7639, muni_total_count = muni_large_count + muni_small_count, muni_countperm2 = muni_large_count/sitearea_m2_large+muni_small_count/sitearea_m2_small) %>% 
  select(-area_ft2_large, -area_ft2_small) %>% 
  mutate(fast.food.or.convenience.store = as.factor(fast.food.or.convenience.store), bus.stop.within.site.or.sight. = as.factor(bus.stop.within.site.or.sight.), litter.bin = as.factor(litter.bin), grass.height = as.factor(grass.height), street.type = as.factor(street.type), area = as.factor(area))

van_municipal_location_temp = read.csv('data_municipalaudits\\Vancouver_localarea_municipalauditlocations_qgis.csv') %>% rename(siteid = id, maparea_m2 = area_m2)


### INATURALIST data by municipal area

inat_all_temp = read.csv("data_iNaturalist\\inaturalist_censusarea_qgis.csv") %>% 
  select(mapid,name,id, observed_on, user_id) %>% 
  mutate(observed_on = as.Date(observed_on, "%m/%d/%Y")) %>% 
  rename(inat_id = id, inat_observed_on = observed_on, inat_userid = user_id) %>% 
  mutate(month = month(inat_observed_on), year = year(inat_observed_on)) %>% 
  group_by(mapid, year) %>% 
  summarize(inat_submissions = n(), inat_users = n_distinct(inat_userid))


### 311 Call data 
##(from September of each year only because of how much data there is)

Sept_311_temp = read.csv("data_Vancouver311\\201709CaseLocationsDetails.csv", fileEncoding="UTF-8-BOM") %>% 
  rbind(read.csv("data_Vancouver311\\201809CaseLocationsDetails.csv", fileEncoding="UTF-8-BOM")) %>% 
  rbind(read.csv("data_Vancouver311\\201909CaseLocationsDetails.csv", fileEncoding="UTF-8-BOM")) %>% 
  select(Year,Month, Day, Division, Case_Type, Local_Area) %>% 
  rename(year = Year, month = Month) %>% 
  mutate(mapid = ifelse(Local_Area =="Strathcona", "STR",ifelse(Local_Area == "Arbutus-Ridge"|Local_Area =="Arbutus Ridge","AR",ifelse(Local_Area =="Kitsilano","KITS",ifelse(Local_Area =="Marpole","MARP",ifelse(Local_Area =="Kensington-Cedar Cottage","KC",ifelse(Local_Area =="Shaughnessy","SHAU",ifelse(Local_Area =="Victoria-Fraserview","VF",ifelse(Local_Area =="Riley Park","RP",ifelse(Local_Area =="Renfrew-Collingwood","RC",ifelse(Local_Area =="Mount Pleasant","MP",ifelse(Local_Area =="Hastings-Sunrise","HS",ifelse(Local_Area =="Downtown","CBD",ifelse(Local_Area=="Dunbar-Southlands"|Local_Area =="Dunbar Southlands","DS",ifelse(Local_Area =="Grandview-Woodland","GW",ifelse(Local_Area=="West Point Grey","WPG",ifelse(Local_Area =="Fairview","FAIR",ifelse(Local_Area =="Kerrisdale","KERR",ifelse(Local_Area =="Killarney","KIL",ifelse(Local_Area =="South Cambie", "SC", ifelse(Local_Area =="Sunset","SUN",ifelse(Local_Area =="West End","WE",ifelse(Local_Area =="Oakridge","OAK",NA))))))))))))))))))))))) %>% 
  filter(!is.na(mapid)) %>%
  mutate(littercomplaint = ifelse(Case_Type == "Illegal Dumping/Abandoned Garbage Pickup",1,0), graffiticomplaint = ifelse(Case_Type == "Graffiti Removal - City Property"|Case_Type =="Graffiti Removal - External Organization",1,0)) %>% 
  group_by(mapid,year,month) %>% 
  summarize(total_311 = n(), litter_311 = sum(littercomplaint), graffiti_311 = sum(graffiticomplaint))


## Census Data by municipal area
###three additional categories were hand-calculated in excel + removing commas in row one and reformatting place names to remove spaces before import
###Cheat to censusid's included here: 1 = total population, 94 = average age, 95 = median age, 23130 = percent_commutebikeorwalk is 2317walked and 2318biked [dividedby] 2313total, 25310 = 2533immigrants [divided by] 2531total population for immigrant question, 3963 = average total household income, 3964 median total household income, 41080 = No certificate, diploma or degree [divided by] total population for education question

van_census_by_muniarea_temp = read.csv("data_demographics\\Vancouver_localarea_demographics_download.csv") %>% 
  mutate(censuscategoryid=ID,censuscategory=Variable) %>% 
  select(-ID,-Variable) %>% 
  filter(censuscategoryid ==1 | censuscategoryid==94 | censuscategoryid==95 | censuscategoryid==23130 | censuscategoryid == 25310 | is.element(censuscategoryid, 3963:3964) | censuscategoryid == 41080) %>%
  select(-censuscategoryid) %>%
  t() %>% as.data.frame() %>% 
  arrange(desc(V1)) %>% 
  row_to_names(row_number = 1) %>% 
  rownames_to_column("name") %>% clean_names() %>% 
  mutate(mapid = ifelse(name =="Strathcona", "STR",ifelse(name == "Arbutus-Ridge"|name =="Arbutus Ridge"|name=="Arbutus_Ridge","AR",ifelse(name =="Kitsilano","KITS",
     ifelse(name =="Marpole","MARP",ifelse(name =="Kensington-Cedar Cottage"|name=="Kensington_Cedar_Cottage","KC",ifelse(name =="Shaughnessy","SHAU",
     ifelse(name =="Victoria-Fraserview"|name=="Victoria_Fraserview","VF",ifelse(name =="Riley Park"|name=="Riley_Park","RP",ifelse(name =="Renfrew-Collingwood"|name=="Renfrew_Collingwood","RC",
     ifelse(name =="Mount Pleasant"|name=="Mount_Pleasant","MP",ifelse(name =="Hastings-Sunrise"|name=="Hastings_Sunrise","HS",ifelse(name =="Downtown","CBD",
     ifelse(name=="Dunbar-Southlands"|name =="Dunbar Southlands"|name=="Dunbar_Southlands","DS",ifelse(name =="Grandview-Woodland"|name=="Grandview_Woodland","GW",
     ifelse(name=="West Point Grey"|name=="West_Point_Grey","WPG",ifelse(name =="Fairview","FAIR",ifelse(name =="Kerrisdale","KERR",
     ifelse(name =="Killarney","KIL",ifelse(name =="South Cambie"|name=="South_Cambie", "SC", ifelse(name =="Sunset","SUN",
     ifelse(name =="West End"|name=="West_End","WE",ifelse(name =="Oakridge","OAK",NA))))))))))))))))))))))) %>% 
  select(-name) %>% 
  mutate(total_population = as.numeric(total_population), average_age_of_the_population = as.numeric(average_age_of_the_population), median_age_of_the_population = as.numeric(median_age_of_the_population), percent_commutebikeorwalk = as.numeric(percent_commutebikeorwalk), percent_immigrants = as.numeric(percent_immigrants), totalhouseholdincome_median= as.numeric(totalhouseholdincome_median), totalhouseholdincome_average = as.numeric(totalhouseholdincome_average), percent_nodegree = as.numeric(percent_nodegree))



# CREATE NEW, JOINED FILES FOR ANALYSIS------

###THE MUNICIPAL DATA WITH ALL ATTRIBUTES
#Join municipal characteristics + counts + locations with inaturalist (tallied by full year in current form ??should this by Sept only?) and 311 calls (September only )
van_municipal = van_municipal_transcribed_temp %>% 
  full_join(van_municipal_location_temp, by = "siteid") %>% 
  filter(siteid!=29 & siteid!=99) %>% 
  left_join(inat_all_temp, by = c("mapid", "year")) %>% mutate(inat_submissions = ifelse(is.na(inat_submissions),0,inat_submissions),inat_users = ifelse(is.na(inat_users),0,inat_users)) %>% 
    left_join(Sept_311_temp, by = c("mapid","year","month")) %>% 
  left_join(van_census_by_muniarea_temp, by = "mapid")

van_municipal_byarea = van_municipal %>% group_by(mapid, year) %>% summarize(muni_meancount_area = mean(muni_total_count, na.rm = T))



## BASIC DATA SUMMARY CALCULATIONS-----

## {{ITEM COUNTS}}------
van_count = van_litterati %>% mutate(streettype_noted = ifelse(is.na(streetuse),0,1),arterial = ifelse(is.na(streetuse),NA,ifelse(streetuse == "Arterial"|streetuse =="Secondary Arterial",1,0))) %>%  group_by(year,mapid) %>% summarize(litt_count = n(), litt_users = n_distinct(username),perc_arterial = sum(arterial,na.rm = T)/sum(streettype_noted))  %>% 
  right_join(van_municipal, by = c("mapid", "year")) %>% left_join(van_municipal_byarea, by = c("mapid","year"))%>% 
  mutate(litt_count = ifelse(is.na(litt_count),0,litt_count), litt_users = ifelse(is.na(litt_users),0,litt_users),littcountperuser = ifelse(litt_count ==0,0,litt_count/litt_users), total311perpop = total_311/total_population, inatuserperpop = inat_users/total_population,litt_count_binary =ifelse(litt_count>0,1,0),inat_subperuser = ifelse(inat_users == 0,0,ifelse(inat_submissions ==0,0,inat_submissions/inat_users)), pop_density_m2 = total_population/maparea_m2,inat_subperpop = inat_submissions/total_population)


## number of submissions, users, NA tag
nrow(van_litterati)
n_distinct(van_litterati$username)
van_litterati %>% filter(tags=="") %>% nrow()

##average litterati submissions/user-day
van_litterati %>% group_by(username,month, day) %>% summarize(submissions = n()) %>% ungroup() %>% summarize(mean(submissions))

## number of active users per month, by year
temp =van_litterati %>% group_by(year, month) %>% summarize(n())#summarize(n_distinct(username))

## Mean litterati submissions/day & Max litterati submissions in a given day (and the date when that occurred)----
van_litterati %>% group_by(time) %>% summarize(submissions = n()) %>% ungroup() %>% summarize(mean(submissions), median(submissions), min(submissions), max(submissions))
                    # Max value of 1339 submissions/day occurs on 2019-06-30 (with a close second on 6/29/2019 as if there was an event)

## mean and max municipal items/audit ----
van_municipal %>% group_by(year) %>% summarize(totalitems = sum(muni_total_count, na.rm = T)) %>% ungroup() %>% summarize(mean(totalitems), max(totalitems))
## mean and max municipal items/site----
van_municipal %>% group_by(year, siteid) %>% summarize(totalitems = sum(muni_total_count, na.rm = T)) %>% ungroup() %>% summarize(mean(totalitems), max(totalitems))
## mean and max municipal items/m2----
van_municipal %>% mutate(itemsperarea_small = muni_small_count/sitearea_m2_small, itemsperarea_large = muni_large_count/sitearea_m2_large, itemsperarea_total = itemsperarea_small + itemsperarea_large) %>% summarize(mean(itemsperarea_total, na.rm = T), max(itemsperarea_total, na.rm = T)) 

## Litterati counts by month to answer: is September similar to annual average in terms of types of items, counts?----
van_litterati %>% group_by(month) %>% summarize(submissions = n(), dayscovered = n_distinct(time), submissionsperday = submissions/dayscovered)

## spread of litterati observations on major vs. minor streets
van_litterati %>% group_by(streetuse) %>% summarize(submissions = n())

## PLOT: distribution of litterati observations over time
# Figure 5 (5A)
van_litterati %>% 
  mutate(dayofweek = wday(time, label = TRUE, abbr = FALSE)) %>% 
  group_by(dayofweek) %>% summarize(submissions = n()) %>% 
  ggplot(aes(x = dayofweek, y = submissions))+
  geom_col()+
  xlab("Day Observed")+
  ylab("Number of Submissions")+
  theme_bw()+
  theme(text = element_text(size = 18),axis.text.x = element_text(angle = 25, vjust = 0.7, hjust=0.5))
  
#ggsave("Litterati_dayofweek.png", width = 5, height = 4, dpi = 300)

# Figure 5 (5B)
van_litterati %>% 
  mutate(month = month(month, label = TRUE)) %>% 
  group_by(month) %>% summarize(submissions = n()) %>% 
  ggplot(aes(x = month, y = submissions))+
  geom_col()+
  xlab("Month Observed")+
  ylab("Number of Submissions")+
  theme_bw()+
  theme(text = element_text(size = 18),axis.text.x = element_text(angle = 25, vjust = 0.7, hjust=0.5))
#ggsave("Litterati_monthobserved.png", width = 5, height = 5, dpi = 300)


## Quick investigation of "challenges" or "events"  ----
# How many people and submissions were associated with two of the largest cleanups:
  #influence of singular events
  #challenge = "ocean heroes"
  van_litterati %>% filter(challenges =="Ocean Heroes") %>% summarize(users = n_distinct(username), submissions = n(), days = n_distinct(day))
  #challenge = "West End Cleanup"
  van_litterati %>% filter(challenges =="West End Cleanup") %>% summarize(users = n_distinct(username), submissions = n(), days = n_distinct(day))
  
  
## {{Item Types}}------
van_type = van_litterati %>% select(year, month, time, mapid,username,  tags) %>% 
  mutate(itemmaterial = NA) %>% 
  mutate(itemmaterial = ifelse (str_detect(tags,"nylon")|tags=="ziptie"|tags=="chip bag"|str_detect(tags,"floss")|str_detect(tags,"earplug")|str_detect(tags,"ear plug")|tags=="bottle"|str_detect(tags, "starbucksstopper")|str_detect(tags,"danactive")|str_detect(tags,"syringe")|str_detect(tags,"nintendo")|str_detect(tags,"styrofoam")|str_detect(tags,"lid")|str_detect(tags,"lighter")|str_detect(tags,"tape")|str_detect(tags,"ziploc")|str_detect(tags,"chipbag")|str_detect(tags,"wrapper")|str_detect(tags,"plastic")|str_detect(tags,"glove")|str_detect(tags,"straw")|str_detect(tags,"rubber")|str_detect(tags,"gatorade")|str_detect(tags,"foam")|str_detect(tags,"ball")|str_detect(tags,"balloon")|str_detect(tags,"doritos")|str_detect(tags,"jollyrancher")|str_detect(tags,"poopbag")|str_detect(tags,"cheetos")|str_detect(tags,"snickers")|str_detect(tags,"wrigleys")|str_detect(tags,"starburst")|str_detect(tags,"trident")|str_detect(tags,"nowandlater")|str_detect(tags,"reeces")|str_detect(tags,"pen")|str_detect(tags,"potatochips")|str_detect(tags,"polystyrene")|str_detect(tags,"waterbottle"), "plastic",
                            ifelse(str_detect(tags,"paper")|tags =="cup"|str_detect(tags,"litteredcup")|tags=="timhortons+coffeecup"|str_detect(tags,"receipt")|str_detect(tags,"cardboard")|str_detect(tags,"napkin")|str_detect(tags,"mail")|str_detect(tags,"tissue")|str_detect(tags,"coffeesleeve")|str_detect(tags,"box"),"paper",
                                   ifelse(str_detect(tags,"cig")|str_detect(tags,"marlboro")|str_detect(tags,"camel")|str_detect(tags,"tobacco"),"plastic",
                                          ifelse(str_detect(tags,"gum"),"plastic",
                                                 ifelse(str_detect(tags,"cloth")|str_detect(tags,"fabric")|str_detect(tags,"string")|str_detect(tags,"rope")|str_detect(tags,"compostable"),"natural",
                                                        ifelse(str_detect(tags,"foil")|str_detect(tags,"coke")|str_detect(tags,"can")|str_detect(tags,"aluminum")|str_detect(tags,"aluminium")|str_detect(tags,"blade")|str_detect(tags,"metal")|str_detect(tags,"bottlecap"),"metal",
                                                               ifelse(str_detect(tags,"glass")|tags=="beer bottle","glass",
                                                                      ifelse(str_detect(tags,"wood")|tags=="icecreamsrick"|tags=="lollipop stick","natural",
                                                                             ifelse(tags=="","none",
                                                                                    "unknown or other")))))))))) %>% 
  mutate(itemtype = NA) %>% 
  mutate(itemtype = ifelse (str_detect(tags,"straw")|str_detect(tags, "starbucksstopper"),"straws & stirrers",
                            ifelse(str_detect(tags,"cup"), "cups",
                                   ifelse(str_detect(tags,"cardboard"),"cardboard",
                                          ifelse(itemmaterial =="none","none",
                                          ifelse(str_detect(tags,"sticker")|tags=="product information","sticker",
                                          ifelse(str_detect(tags,"lid"),"lids",
                                                 ifelse(str_detect(tags,"spoon"),"utensils",
                                                        ifelse(str_detect(tags,"bag")&str_detect(tags,"garbage",negate = TRUE),"bag",
                                                        ifelse(str_detect(tags,"bottle")|str_detect(tags,"can")|str_detect(tags,"coke"),"beverage containers",
                                                        ifelse(str_detect(tags,"tissue")|str_detect(tags,"napkin")|str_detect(tags,"applepie"),"other paper",
                                                               ifelse(str_detect(tags,"wrapper")|str_detect(tags,"chipbag"),"wrapper",
                                                                      ifelse(str_detect(tags,"cig")|str_detect(tags,"marlboro")|str_detect(tags,"vape")|str_detect(tags,"tobacco")|str_detect(tags,"camel")|str_detect(tags,"lighter"),"tobacco products",
                                                                             ifelse(str_detect(tags,"gum"),"gum",
                                                                                    ifelse(str_detect(tags,"needle")|str_detect(tags,"syringe"),"needles/syringes", 
                                                                                           ifelse(str_detect(tags,"cap"), "bottle caps",
                                                                                                  ifelse(str_detect(tags,"foil"),"Aluminum/Foil Debris",
                                                                                                         ifelse(str_detect(tags,"receipt")|str_detect(tags,"mail")|str_detect(tags,"paper"),"Paper/Fibre Material",
                                                                                                                "other"))))))))))))))))))

#by neighborhood
van_litterati %>% group_by(mapid) %>% summarize(count =n()) %>% ungroup() %>% filter %>% summarize(sum(count))

 temp = van_type %>% filter(itemtype =="other")    
 

# Summarize most prominent item materials in vancouver----
van_type %>% group_by(itemmaterial) %>% summarize(count = n(), percent_ignoringnones = count/nrow(subset(van_type,van_type$itemmaterial != "none"))) %>% mutate(percent_ignoringnones = ifelse(itemmaterial=="none",NA, percent_ignoringnones))
van_type %>% group_by(year) %>% summarize(submission = n())
van_type %>% group_by(itemmaterial,year) %>% summarize(count = n()) %>% mutate(total2017 = 122, total2018 =359, total2019 = 3436) %>% mutate(perc_2017 = count/total2017, perc_2018 = count/total2018, perc_2019 = count/total2019)
van_type %>% filter(itemmaterial!="none")%>% group_by(itemmaterial,year) %>% summarize(count = n()) %>% mutate(total2017 = 122-37, total2018 =359-123, total2019 = 3436-1055) %>% mutate(perc_2017 = count/total2017, perc_2018 = count/total2018, perc_2019 = count/total2019)

## Plot: item types by year
# Figure 2 (2A)
## Muni data from audits 2017-2019, large items only, figure 2
year = c(2017, 2018, 2019)
  plastic = c(.34, .42, .46)
  paper = c(.39,.38,.33)
  other = c(.24,.18,.19)
  metal = c(0.01,.01,0.01)
  composite = c(0.01,.01,0)
  glass = c(0.01,0,.01)
muni_materialtype = data.frame(year,plastic,paper,other,metal,composite,glass) %>% rename(Plastic = plastic, Paper = paper, Other=other, Metal=metal, Composite = composite, Glass = glass) %>% pivot_longer(cols = c(Plastic,Paper,Other,Metal,Composite,Glass), names_to = "Material", values_to = "percent")
cbPalette <- c("dark gray", "light blue", "#E69F00", "light gray", "#009E73","#0072B2")  
ggplot(data = muni_materialtype, aes(x = as.integer(year), y = percent, fill = Material))+
    geom_col()+
  theme_bw()+
  scale_x_continuous(breaks= c(2017,2018,2019))+
  scale_y_continuous(labels = scales::percent)+
  ylab("Percent of audited items")+
  xlab("Year of audit")+
  theme(text = element_text(size = 18))    + 
  scale_fill_manual(values = cbPalette)    
  #replaced to match CRK project color scale. scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#CC79A7"))#composite, glass, metal, other, paper, plastic
#ggsave("muni_materialtype_percents.png", width = 5, height = 5, dpi = 300)

#Figure 2 (2B)
## Litterati data from van_type, as above, all item sizes
plastic_lit = c(.482,.665,.71) #if none is included: c(.336,.437,.492)
paper_lit = c(.212,.169,.172) #if none is included: c(.148,.111,.119)
  natural_lit = c(.0235,.00424,.0172) #if none is included: c(.0164,.00279,.0119)
  glass_lit = c(0,0,0.0084) #if none is included: c(0,0,.00582)
  metal_lit = c(.0706,.0636,.0424) #if none is included: c(.0492,.0418,.0294)
  other_lit = c(.212,.0975,.05) #if none is included: c(.148,.0641,.0346) #"unknown"
  #if none is included: tag_missing = c(.303,.343,.307)
litterati_materialtype = data.frame(year,plastic_lit,paper_lit,other_lit,metal_lit,natural_lit,glass_lit) %>% rename(Plastic = plastic_lit, Paper = paper_lit, Other = other_lit, Metal = metal_lit, 'Cloth/wood' = natural_lit, Glass = glass_lit)%>% pivot_longer(cols = c(Plastic,Paper,Other,Metal,'Cloth/wood',Glass), names_to = "Material", values_to = "percent")
cbPalette <- c("dark gray", "light blue", "#E69F00", "light gray", "#009E73","#0072B2")  
ggplot(data = litterati_materialtype, aes(x = year, y = percent, fill = Material))+
  geom_col()+
  theme_bw()+
  scale_x_continuous(breaks= c(2017,2018,2019))+
  scale_y_continuous(labels = scales::percent)+
  ylab("Percent of tagged submissions")+
  xlab("Year")+
    theme(text = element_text(size = 18))  + 
  scale_fill_manual(values = cbPalette)  
#replaced to match CRK project color scale. scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#CC79A7"))#cloth_wood, glass, metal, other, paper, plastic, tag_missing:"#999999"
#ggsave("litterati_materialtype_percents.png", width = 5, height = 5, dpi = 300)
  
  
## Plot to compare against 2019 muni plot fig 2 as pie chart
van_type %>% filter(year == 2019) %>% group_by(itemmaterial) %>% summarize(count = n(), percent_ignoringnones = count/nrow(subset(van_type,van_type$itemmaterial != "none"))) %>% mutate(percent_ignoringnones = ifelse(itemmaterial=="none",NA, percent_ignoringnones))  %>% filter(itemmaterial!="none") %>% 
  ggplot(aes(x = "", y = count, fill = itemmaterial))+geom_bar(stat = "identity", width = 1)+coord_polar("y", start = 0)+theme_void()+scale_fill_manual(values = c("gray","purple", "lime green", "brown", "dark blue", "dark green"))

# most prominent item types in vancouver----
temp =van_type %>% group_by(year,itemtype) %>% summarize(count = n(), percent_ignoringnones = count/nrow(subset(van_type,van_type$itemtype != "other"))) %>% mutate(percent_ignoringnones = ifelse(itemtype=="other",NA, percent_ignoringnones))





## Item Locations----
# Region with highest litterati count vs. region with highest municipal count
van_count%>% group_by(year, mapid, litt_count) %>% summarize(muni_mapid_total = sum(muni_total_count))
        #top 2 highest count 2017-- MUNI: STR & KC @179 items, litt: WE & MP at 29 & 26 items. 2018-- MUNI: RC & KC at 226 & 190. litt: MP & CBD. 2019 -- MUNI: CBD & RC, litt: WE and CBD



## Regression----
# check correlation plot
van_count_num = van_count %>% mutate(mapid_num = as.numeric(as.factor(mapid))) %>% 
  mutate(fast.food.or.convenience.store = as.numeric(fast.food.or.convenience.store), bus.stop.within.site.or.sight. = as.numeric(bus.stop.within.site.or.sight.), litter.bin = as.numeric(litter.bin)) %>% mutate(inat_subperpop = inat_submissions/total_population) %>% 
  select(-name,-street.type,-mapid,-area,-grass.height)
van_count_num %>% cor(use = "complete.obs", method = "spearman") %>% 
corrplot()


## Explore Basic lm
lm_basic = lm(data = van_count, muni_countperm2 ~ as.factor(litter.bin) + as.factor(bus.stop.within.site.or.sight.) + as.factor(fast.food.or.convenience.store) + as.factor(grass.height) + street.type + area + as.factor(year) + pop_density_m2 + inatuserperpop + total311perpop + litt_count)
summary(lm_basic)
plot(van_count$area)
ggplot(data = lm_basic, aes(x = area, y = muni_countperm2))+geom_boxplot()

#check individual variable histograms
plot(van_count$litter.bin)
plot(van_count$bus.stop.within.site.or.sight.)
plot(van_count$grass.height)
plot(van_count$street.type)
plot(as.factor(van_count$year))
hist(van_count$pop_density_m2)
hist(van_count$inatuserperpop)
hist(van_count$total311perpop)
hist(van_count$litt_count)
hist(van_count$muni_countperm2)

van_count_lm = van_count %>% 
  #mutate(zoning = ifelse(area == "residential"|area=="park", "residential", ifelse(area =="commercial","commercial","other, developed"))) %>%
  mutate(zoning = ifelse(area == "residential", "residential", ifelse(area =="commercial","commercial",ifelse(area =="mixed, park","park","other, developed")))) %>% 
  mutate(litt_bin = ifelse(litt_count==0,1,ifelse(litt_count>0&litt_count<=2,2,ifelse(litt_count>2&litt_count<=13,3,4)))) %>% 
  mutate(grass.height_numbers = ifelse(grass.height=="na",0,ifelse(grass.height =="short",1,ifelse(grass.height =="mid",2,3)))) %>% 
  mutate(year_numbers = ifelse(year==2017,0,ifelse(year ==2018,1,2))) %>% 
  mutate(street.type_numbers = ifelse(street.type == "minor",0,1)) %>% 
  mutate(pop_density_km2 = pop_density_m2*1000000) %>% 
  mutate(litt_userperpop = litt_users/total_population)
 # don't make them factors when anticipate increasing value = increasing count; factors lose ordering. mutate(litt_bin = as.factor(litt_bin),litter.bin = as.factor(litter.bin), bus.stop.within.site.or.sight.= as.factor(bus.stop.within.site.or.sight.), fast.food.or.convenience.store = as.factor(fast.food.or.convenience.store), grass.height = as.factor(grass.height), year = as.factor(year))

#Manuscript submission & dissertation contain lm_cleaned_numeric
lm_cleaned = lm(data = van_count_lm, muni_countperm2 ~ litter.bin + bus.stop.within.site.or.sight. + fast.food.or.convenience.store + grass.height_numbers + street.type + zoning + year_numbers + pop_density_km2 + inatuserperpop + total311perpop + litt_bin)
summary(lm_cleaned)
lm_cleaned_numeric = lm(data = van_count_lm, muni_countperm2 ~ as.numeric(litter.bin) + as.numeric(bus.stop.within.site.or.sight.) + as.numeric(fast.food.or.convenience.store) + grass.height_numbers + street.type + zoning + year_numbers + pop_density_km2 + inatuserperpop + total311perpop + litt_bin)
lm_cleaned_numeric_streettype = lm(data = van_count_lm, muni_countperm2 ~ as.numeric(litter.bin) + as.numeric(bus.stop.within.site.or.sight.) + as.numeric(fast.food.or.convenience.store) + grass.height_numbers + street.type_numbers + zoning + year_numbers + pop_density_km2 + inatuserperpop + total311perpop + litt_bin)
summary(lm_cleaned_numeric_streettype)

## plots of these predictors
# Pop density vs. item density, by street type
majorresidential_varpopdens = data.frame(litter.bin = mean(as.numeric(van_count_lm$litter.bin)),bus.stop.within.site.or.sight. = mean(as.numeric(van_count_lm$bus.stop.within.site.or.sight.)), fast.food.or.convenience.store = mean(as.numeric(van_count_lm$fast.food.or.convenience.store)), grass.height_numbers = mean(van_count_lm$grass.height_numbers), street.type ="major", zoning = "residential", year_numbers = mean(van_count_lm$year_numbers),pop_density_km2 = seq(min(van_count_lm$pop_density_km2),max(van_count_lm$pop_density_km2),length.out = nrow(van_count_lm)),inatuserperpop=mean(van_count_lm$inatuserperpop), total311perpop=mean(van_count_lm$total311perpop), litt_bin= mean(van_count_lm$litt_bin)) 
  majorresidential_varpopdens_predict = majorresidential_varpopdens %>% cbind(predict(lm_cleaned_numeric, newdata =majorresidential_varpopdens,  type = "response",interval = "confidence"))
minorresidential_varpopdens = data.frame(litter.bin = mean(as.numeric(van_count_lm$litter.bin)),bus.stop.within.site.or.sight. = mean(as.numeric(van_count_lm$bus.stop.within.site.or.sight.)), fast.food.or.convenience.store = mean(as.numeric(van_count_lm$fast.food.or.convenience.store)), grass.height_numbers = mean(van_count_lm$grass.height_numbers), street.type ="minor", zoning = "residential", year_numbers = mean(van_count_lm$year_numbers),pop_density_km2 = seq(min(van_count_lm$pop_density_km2),max(van_count_lm$pop_density_km2),length.out = nrow(van_count_lm)),inatuserperpop=mean(van_count_lm$inatuserperpop), total311perpop=mean(van_count_lm$total311perpop), litt_bin= mean(van_count_lm$litt_bin))
  minorresidential_varpopdens_predict = minorresidential_varpopdens %>% cbind(predict(lm_cleaned_numeric, type = "response",newdata = minorresidential_varpopdens, interval = "confidence"))

  # Figure 3 (3A)
ggplot()+
  geom_point(data = van_count_lm, aes(x = pop_density_km2, y = muni_countperm2), color = "gray")+
  geom_line(data = majorresidential_varpopdens_predict, aes(x = pop_density_km2, y = fit,linetype = "Major"), size = 2)+
  geom_line(data = minorresidential_varpopdens_predict, aes(x = pop_density_km2, y = fit, linetype = "Minor"),  size = 2)+
  geom_ribbon(data = minorresidential_varpopdens_predict, aes(x = pop_density_km2, ymin = lwr,ymax=upr),  alpha = 0.35, fill = "gray", show.legend = F)+
  geom_ribbon(data = majorresidential_varpopdens_predict, aes(x = pop_density_km2, ymin = lwr,ymax=upr),  alpha = 0.35, fill = "gray", show.legend = F)+
  theme_bw()+
  xlab(expression(paste("Population Density (people/km"^2,")")))+
  ylab(expression(paste("Litter Density (items/m"^2,")")))+
  scale_linetype_manual(values = c("Major" = 1, "Minor" = 2))+
  guides(linetype = guide_legend(override.aes = list(linetype = c(1,6),size = 1.2) ) )+
  labs(linetype = "Street Type:")+
  theme(legend.position="top")+
  theme(text = element_text(size = 18))
#ggsave("muni_popdens_predictions_95conf.png",width = 6, height = 6, dpi = 500)



#Calls to 311 vs item density, by zoning category
residential_var311 = data.frame(litter.bin = mean(as.numeric(van_count_lm$litter.bin)),bus.stop.within.site.or.sight. = mean(as.numeric(van_count_lm$bus.stop.within.site.or.sight.)), fast.food.or.convenience.store = mean(as.numeric(van_count_lm$fast.food.or.convenience.store)), grass.height_numbers = mean(van_count_lm$grass.height_numbers), street.type_numbers =mean(van_count_lm$street.type_numbers), zoning = "residential", year_numbers = mean(van_count_lm$year_numbers),pop_density_km2 = mean(van_count_lm$pop_density_km2),inatuserperpop=mean(van_count_lm$inatuserperpop), total311perpop=seq(min(van_count_lm$total311perpop),max(van_count_lm$total311perpop),length.out = nrow(van_count_lm)), litt_bin= mean(van_count_lm$litt_bin))  
  residential_var311_predict = residential_var311 %>% cbind(predict(lm_cleaned_numeric_streettype,newdata = residential_var311, type = "response", interval = "confidence"))
commercial_var311 = data.frame(litter.bin = mean(as.numeric(van_count_lm$litter.bin)),bus.stop.within.site.or.sight. = mean(as.numeric(van_count_lm$bus.stop.within.site.or.sight.)), fast.food.or.convenience.store = mean(as.numeric(van_count_lm$fast.food.or.convenience.store)), grass.height_numbers = mean(van_count_lm$grass.height_numbers), street.type_numbers =mean(van_count_lm$street.type_numbers), zoning = "commercial", year_numbers = mean(van_count_lm$year_numbers),pop_density_km2 = mean(van_count_lm$pop_density_km2),inatuserperpop=mean(van_count_lm$inatuserperpop), total311perpop=seq(min(van_count_lm$total311perpop),max(van_count_lm$total311perpop),length.out = nrow(van_count_lm)), litt_bin= mean(van_count_lm$litt_bin)) 
  commercial_var311_predict = commercial_var311 %>% cbind(predict(lm_cleaned_numeric_streettype,newdata = commercial_var311, type = "response", interval = "confidence"))
other_var311 = data.frame(litter.bin = mean(as.numeric(van_count_lm$litter.bin)),bus.stop.within.site.or.sight. = mean(as.numeric(van_count_lm$bus.stop.within.site.or.sight.)), fast.food.or.convenience.store = mean(as.numeric(van_count_lm$fast.food.or.convenience.store)), grass.height_numbers = mean(van_count_lm$grass.height_numbers), street.type_numbers =mean(van_count_lm$street.type_numbers), zoning = "other, developed", year_numbers = mean(van_count_lm$year_numbers),pop_density_km2 = mean(van_count_lm$pop_density_km2),inatuserperpop=mean(van_count_lm$inatuserperpop), total311perpop=seq(min(van_count_lm$total311perpop),max(van_count_lm$total311perpop),length.out = nrow(van_count_lm)), litt_bin= mean(van_count_lm$litt_bin))
  other_var311_predict = other_var311 %>% cbind(predict(lm_cleaned_numeric_streettype,newdata = other_var311, type = "response", interval = "confidence"))
park_var311 = data.frame(litter.bin = mean(as.numeric(van_count_lm$litter.bin)),bus.stop.within.site.or.sight. = mean(as.numeric(van_count_lm$bus.stop.within.site.or.sight.)), fast.food.or.convenience.store = mean(as.numeric(van_count_lm$fast.food.or.convenience.store)), grass.height_numbers = mean(van_count_lm$grass.height_numbers), street.type_numbers =mean(van_count_lm$street.type_numbers), zoning = "park", year_numbers = mean(van_count_lm$year_numbers),pop_density_km2 = mean(van_count_lm$pop_density_km2),inatuserperpop=mean(van_count_lm$inatuserperpop), total311perpop=seq(min(van_count_lm$total311perpop),max(van_count_lm$total311perpop),length.out = nrow(van_count_lm)), litt_bin= mean(van_count_lm$litt_bin)) 
  park_var311_predict = park_var311 %>% cbind(predict(lm_cleaned_numeric_streettype,newdata = park_var311, type = "response", interval = "confidence"))

  #Figure 3 (3B)
  ggplot()+
    geom_point(data = van_count_lm, aes(x = total311perpop, y = muni_countperm2), color = "gray")+
    geom_ribbon(data = other_var311_predict, aes(x = total311perpop, ymin = lwr, ymax = upr,fill = "Developed, other",color = NULL), fill = "gray",alpha = .35,show.legend = F) +
    geom_ribbon(data = park_var311_predict, aes(x = total311perpop, ymin = lwr, ymax = upr,fill = "Park",color = NULL),fill = "gray", alpha = .35,show.legend = F) +
    geom_ribbon(data = commercial_var311_predict, aes(x = total311perpop, ymin = lwr, ymax = upr,fill = "Commercial",color = NULL),fill = "gray", alpha = .35,show.legend = F) +
    geom_ribbon(data = residential_var311_predict, aes(x = total311perpop,ymin = lwr, ymax = upr,fill = "Residential",color = NULL), fill = "gray",alpha = .35,show.legend = F) +
    geom_line(data = residential_var311_predict, aes(x = total311perpop, y = fit, color = "Residential"),  size = 2)+
    geom_line(data = commercial_var311_predict, aes(x = total311perpop, y = fit, color = "Commercial"),  size =2)+
    geom_line(data = park_var311_predict, aes(x = total311perpop, y = fit, color = "Park"),  size = 2)+
    geom_line(data = other_var311_predict, aes(x = total311perpop, y = fit,color="Developed, other"),  size =2)+
    theme_bw()+
    xlab("Calls to 311 (calls/person)")+
    ylab(expression(paste("Litter Density (items/m"^2,")")))+
    scale_color_manual(values=c("Residential"="#E69F00", "Commercial"="#0072B2","Park"="#009E73", "Developed, other"="#CC79A7"))+
    labs(colour="Zoning Category:")+
    theme(legend.position="top")+
    theme(text = element_text(size = 18))+
    guides(color=guide_legend(nrow=2, byrow=TRUE))

#ggsave("muni_311_predictions_95conf.png",width = 6, height = 6, dpi = 500)
  


### Litterati version of the same
lm_cleaned_litterati = lm(data = van_count_lm, litt_count ~year_numbers + pop_density_km2 + inatuserperpop + total311perpop + muni_countperm2)
summary(lm_cleaned_litterati)
plot(lm_cleaned_litterati)
van_count_lm_litt = van_count_lm %>% mutate(litt_userper1000pop = litt_userperpop*1000,loglitt_count = log(litt_count+1))
lm_cleaned_litterati_logcount = lm(data = van_count_lm_litt, loglitt_count ~year_numbers + pop_density_km2 + inatuserperpop + total311perpop +litt_userper1000pop)
lm_cleaned_litterati_logcountperuser = lm(data = van_count_lm, log(littcountperuser+1) ~year_numbers + pop_density_km2 + inatuserperpop + total311perpop + I(litt_userperpop*1000))

summary(lm_cleaned_litterati_logcount)
hist(resid(lm_cleaned_litterati_logcount))
plot(lm_cleaned_litterati_logcount)




## plots of this Litterati predictor
# Pop density vs. item density, by street type
litt_varpopdens = data.frame(year_numbers = mean(van_count_lm_litt$year_numbers),pop_density_km2 = seq(min(van_count_lm_litt$pop_density_km2),max(van_count_lm_litt$pop_density_km2),length.out = nrow(van_count_lm_litt)),inatuserperpop=mean(van_count_lm_litt$inatuserperpop), total311perpop=mean(van_count_lm_litt$total311perpop), litt_userper1000pop = mean(van_count_lm_litt$litt_userper1000pop))
litt_varpopdens_predict = litt_varpopdens %>% cbind(predict(lm_cleaned_litterati_logcount, newdata =litt_varpopdens,  type = "response", interval = "confidence")) %>% rename(predicted_log = fit, uprlog = upr, lwrlog = lwr) %>% mutate(predicted = exp(predicted_log)-1, upr = exp(uprlog)-1, lwr = exp(lwrlog)-1)

#Figure 4
ggplot()+
  geom_point(data = van_count_lm, aes(x = pop_density_km2, y = litt_count), color = "gray")+
  geom_line(data = litt_varpopdens_predict, aes(x = pop_density_km2, y = predicted), size = 2)+
  geom_ribbon(data = litt_varpopdens_predict,aes(x = pop_density_km2, ymax = upr, ymin = lwr), fill = "gray", alpha = 0.35, show.legend = F)+
  theme_bw()+
  xlab(expression(paste("Population Density (people/km"^2,")")))+
  ylab("Litterati Submissions")+  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                                labels = trans_format("log10", math_format(10^.x)))+
  theme(text = element_text(size = 18))

#ggsave("litt_popdens_predictions.png",width = 6, height = 6, dpi = 500)


# Predictors of submission totals to Litterati
# Spearman's coefficients, reported in manuscript section 3.4
#litterati vs. population density, calls to 311 (this is done through the table 3 linear regression, too)
van_count_lm_litt %>% 
  select(litt_count,pop_density_m2, year,total311perpop) %>% 
  cor(method = "spearman")
#litterati vs. municipal audit
van_count_lm_litt %>% 
  select(litt_count,muni_countperm2, year) %>% 
  cor(method = "spearman",use = "complete.obs")
#litterati count per user vs. population density, year of submission, calls to 311
van_count_lm_litt %>% 
  select(littcountperuser,pop_density_m2, year,total311perpop) %>% 
  cor(method = "spearman")


