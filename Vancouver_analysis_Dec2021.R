# Litterati project
# Analysis redo from scratch
# Dec 27, 2021

# Load Libraries
library(stringr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tibble) # rownames_to_column
library(janitor) # row_to_names
library(corrplot) #corrplot()
library(lme4) # for the glmm ??or should I be using lmerTest()??
library(lmerTest)

# Read original data files
## Litterati data for Vancouver metro area, years 2017-2019
litt_streets_temp = read.csv("vancouver_litteraticensusareastreettype_qgis.csv") %>% select(id,hblock,streetuse) %>% mutate(streetuse = ifelse(streetuse == "",NA,streetuse), hblock = ifelse(hblock =="",NA,hblock))
# THE LITTERATI DATA
van_litterati = read.csv('Vancouver_litterati_withlocalareaid_qgis.csv') %>% 
  mutate(time = as.Date(time)) %>% 
  select(-url, - display_name, -country_code) %>% 
  mutate(year = year(time), month = month(time), day = day(time)) %>% 
  filter(year == 2017| year == 2018| year == 2019) %>% 
  left_join(litt_streets_temp, by = "id") %>% 
  left_join(read.csv("litterati_nomapid_butinlimits.csv")[ ,c('field_1','id')], by = "id") %>% mutate(mapid = ifelse(mapid ==""& !is.na(field_1),"suburb",mapid)) %>% select(-field_1) %>% filter(mapid!="")

  
## Municipal audits for Vancouver, september 2017, 2018, 2019
     #Note: 2019 municipal audit lacks large counts for sites 73 & 46 and small counts for sites 91 & 94, with no notes in report as to why. It also has 0.5 counts, but I'm not sure what a half an item means or how this is calculated.
### join large count, small count, and site characteristics; join each year; join site to municipal area name
van_municipal_2017_temp = read.csv("Vancouver_2017_site_characteristics_transcribed.csv", fileEncoding="UTF-8-BOM") %>% left_join(read.csv("Vancouver_2017_site_large_count.csv", fileEncoding="UTF-8-BOM"), by = "siteid") %>% left_join(read.csv("Vancouver_2017_site_small_count.csv", fileEncoding="UTF-8-BOM"), by = "siteid") %>% mutate(year = 2017, month = 9)
van_municipal_2018_temp = read.csv("Vancouver_2018_site_characteristics_transcribed.csv", fileEncoding="UTF-8-BOM") %>% left_join(read.csv("Vancouver_2018_site_large_count.csv", fileEncoding="UTF-8-BOM"), by = "siteid") %>% left_join(read.csv("Vancouver_2018_site_small_count.csv", fileEncoding="UTF-8-BOM"), by = "siteid") %>% mutate(year = 2018, month = 9)
van_municipal_2019_temp = read.csv("Vancouver_2019_site_characteristics_transcribed.csv", fileEncoding="UTF-8-BOM") %>% left_join(read.csv("Vancouver_2019_site_large_count.csv", fileEncoding="UTF-8-BOM"), by = "siteid") %>% left_join(read.csv("Vancouver_2019_site_small_count.csv", fileEncoding="UTF-8-BOM"), by = "siteid") %>%  mutate(year = 2019, month = 9)

van_municipal_transcribed_temp = van_municipal_2017_temp %>% rbind(van_municipal_2018_temp) %>% rbind(van_municipal_2019_temp) %>% 
  mutate(sitearea_m2_large = area_ft2_large/10.7639, sitearea_m2_small = area_ft2_small/10.7639, muni_total_count = muni_large_count + muni_small_count, muni_countperm2 = muni_large_count/sitearea_m2_large+muni_small_count/sitearea_m2_small) %>% 
  select(-area_ft2_large, -area_ft2_small) %>% 
  mutate(fast.food.or.convenience.store = as.factor(fast.food.or.convenience.store), bus.stop.within.site.or.sight. = as.factor(bus.stop.within.site.or.sight.), litter.bin = as.factor(litter.bin))

van_municipal_location_temp = read.csv('Vancouver_localarea_municipalauditlocations_qgis.csv') %>% rename(siteid = id, maparea_m2 = area_m2)

### inaturalist data by municipal area
inat_all_bymonth_temp = read.csv("inaturalist\\inaturalist_censusarea.csv") %>% 
  select(mapid,name,id, observed_on, user_id) %>% 
  mutate(observed_on = as.Date(observed_on, "%m/%d/%Y")) %>% 
  rename(inat_id = id, inat_observed_on = observed_on, inat_userid = user_id) %>% 
  mutate(month = month(inat_observed_on), year = year(inat_observed_on)) %>% 
  group_by(mapid, year, month) %>% 
  summarize(inat_submissions = n(), inat_users = n_distinct(inat_userid))
inat_all_temp = read.csv("inaturalist\\inaturalist_censusarea.csv") %>% 
  select(mapid,name,id, observed_on, user_id) %>% 
  mutate(observed_on = as.Date(observed_on, "%m/%d/%Y")) %>% 
  rename(inat_id = id, inat_observed_on = observed_on, inat_userid = user_id) %>% 
  mutate(month = month(inat_observed_on), year = year(inat_observed_on)) %>% 
  group_by(mapid, year) %>% 
  summarize(inat_submissions = n(), inat_users = n_distinct(inat_userid))
### 311 Call data (from September of each year only because of how much data there is)
Sept_311_temp = read.csv("Vancouver_311data\\201709CaseLocationsDetails.csv", fileEncoding="UTF-8-BOM") %>% 
  rbind(read.csv("Vancouver_311data\\201809CaseLocationsDetails.csv", fileEncoding="UTF-8-BOM")) %>% 
  rbind(read.csv("Vancouver_311data\\201909CaseLocationsDetails.csv", fileEncoding="UTF-8-BOM")) %>% 
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
van_census_by_muniarea_temp = read.csv("Vancouver_localarea_demographics_download.csv") %>% 
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


# THE MUNICIPAL DATA WITH ALL ATTRIBUTES
#Join municipal characteristics + counts + locations with inaturalist (tallied by full year in current form ??should this by Sept only?) and 311 calls (September only )
van_municipal = van_municipal_transcribed_temp %>% 
  full_join(van_municipal_location_temp, by = "siteid") %>% 
  filter(siteid!=29 & siteid!=99) %>% 
  #left_join(inat_all_bymonth_temp, by = c("mapid", "year","month")) %>% mutate(inat_submissions = ifelse(is.na(inat_submissions),0,inat_submissions),inat_users = ifelse(is.na(inat_users),0,inat_users)) %>% 
  left_join(inat_all_temp, by = c("mapid", "year")) %>% mutate(inat_submissions = ifelse(is.na(inat_submissions),0,inat_submissions),inat_users = ifelse(is.na(inat_users),0,inat_users)) %>% 
    left_join(Sept_311_temp, by = c("mapid","year","month")) %>% 
  left_join(van_census_by_muniarea_temp, by = "mapid")

van_municipal_byarea = van_municipal %>% group_by(mapid, year) %>% summarize(muni_meancount_area = mean(muni_total_count, na.rm = T))

# Manipulate data files for proper use
## Item Counts-----
van_count = van_litterati %>% mutate(streettype_noted = ifelse(is.na(streetuse),0,1),arterial = ifelse(is.na(streetuse),NA,ifelse(streetuse == "Arterial"|streetuse =="Secondary Arterial",1,0))) %>%  group_by(year,mapid) %>% summarize(litt_count = n(), litt_users = n_distinct(username),perc_arterial = sum(arterial,na.rm = T)/sum(streettype_noted))  %>% 
  right_join(van_municipal, by = c("mapid", "year")) %>% left_join(van_municipal_byarea, by = c("mapid","year"))%>% 
  mutate(litt_count = ifelse(is.na(litt_count),0,litt_count), litt_users = ifelse(is.na(litt_users),0,litt_users),littcountperuser = ifelse(litt_count ==0,0,litt_count/litt_users), total311perpop = total_311/total_population, inatuserperpop = inat_users/total_population,litt_count_binary =ifelse(litt_count>0,1,0),inat_subperuser = ifelse(inat_users == 0,0,ifelse(inat_submissions ==0,0,inat_submissions/inat_users)), pop_density_m2 = total_population/maparea_m2)

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







## Item Types
van_type = van_litterati %>% select(year, month, time, mapid,username,  tags) %>% 
  mutate(itemmaterial = NA) %>% 
  mutate(itemmaterial = ifelse (str_detect(tags,"nylon")|str_detect(tags, "starbucksstopper")|str_detect(tags,"danactive")|str_detect(tags,"syringe")|str_detect(tags,"nintendo")|str_detect(tags,"styrofoam")|str_detect(tags,"lid")|str_detect(tags,"lighter")|str_detect(tags,"tape")|str_detect(tags,"ziploc")|str_detect(tags,"chipbag")|str_detect(tags,"wrapper")|str_detect(tags,"plastic")|str_detect(tags,"glove")|str_detect(tags,"straw")|str_detect(tags,"rubber")|str_detect(tags,"gatorade")|str_detect(tags,"foam")|str_detect(tags,"ball")|str_detect(tags,"balloon")|str_detect(tags,"doritos")|str_detect(tags,"jollyrancher")|str_detect(tags,"poopbag")|str_detect(tags,"cheetos")|str_detect(tags,"snickers")|str_detect(tags,"wrigleys")|str_detect(tags,"starburst")|str_detect(tags,"trident")|str_detect(tags,"nowandlater")|str_detect(tags,"reeces")|str_detect(tags,"pen")|str_detect(tags,"potatochips")|str_detect(tags,"polystyrene")|str_detect(tags,"waterbottle"), "plastic",
                            ifelse(str_detect(tags,"paper")|str_detect(tags,"receipt")|str_detect(tags,"cardboard")|str_detect(tags,"napkin")|str_detect(tags,"mail")|str_detect(tags,"tissue")|str_detect(tags,"coffeesleeve")|str_detect(tags,"box"),"paper",
                                   ifelse(str_detect(tags,"cig")|str_detect(tags,"marlboro")|str_detect(tags,"camel"),"plastic",
                                          ifelse(str_detect(tags,"gum"),"plastic",
                                                 ifelse(str_detect(tags,"cloth")|str_detect(tags,"fabric")|str_detect(tags,"string")|str_detect(tags,"rope"),"natural",
                                                        ifelse(str_detect(tags,"foil")|str_detect(tags,"coke")|str_detect(tags,"can")|str_detect(tags,"aluminum")|str_detect(tags,"aluminium")|str_detect(tags,"blade")|str_detect(tags,"metal")|str_detect(tags,"bottlecap"),"metal",
                                                               ifelse(str_detect(tags,"glass"),"glass",
                                                                      ifelse(str_detect(tags,"wood"),"natural",
                                                                             ifelse(tags=="","none",
                                                                                    "other")))))))))) %>% 
  mutate(itemtype = NA) %>% 
  mutate(itemtype = ifelse (str_detect(tags,"straw")|str_detect(tags, "starbucksstopper"),"straws & stirrers",
                            ifelse(str_detect(tags,"cup"), "cup",
                                   ifelse(str_detect(tags,"cardboard"),"cardboard",
                                          ifelse(str_detect(tags,"lid"),"lids",
                                                 ifelse(str_detect(tags,"spoon"),"utensils",
                                                        ifelse(str_detect(tags,"tissue")|str_detect(tags,"napkin"),"other paper",
                                                               ifelse(str_detect(tags,"wrapper")|str_detect(tags,"chipbag"),"wrapper",
                                                                      ifelse(str_detect(tags,"cig")|str_detect(tags,"marlboro")|str_detect(tags,"camel")|str_detect(tags,"lighter"),"cigarette butts/debris",
                                                                             ifelse(str_detect(tags,"gum"),"gum",
                                                                                    ifelse(str_detect(tags,"needle")|str_detect(tags,"syringe"),"needles/syringes", 
                                                                                           ifelse(str_detect(tags,"cap"), "bottle caps",
                                                                                                  ifelse(str_detect(tags,"foil"),"Aluminum/Foil Debris",
                                                                                                         ifelse(str_detect(tags,"receipt"),"Paper/Fibre Material",
                                                                                                                "other"))))))))))))))
                                                                                                  
# most prominent item materials in vancouver----
van_type %>% group_by(itemmaterial) %>% summarize(count = n(), percent_ignoringnones = count/nrow(subset(van_type,van_type$itemmaterial != "none"))) %>% mutate(percent_ignoringnones = ifelse(itemmaterial=="none",NA, percent_ignoringnones))
## Plot to compare against 2019 muni plot fig 2
van_type %>% filter(year == 2019) %>% group_by(itemmaterial) %>% summarize(count = n(), percent_ignoringnones = count/nrow(subset(van_type,van_type$itemmaterial != "none"))) %>% mutate(percent_ignoringnones = ifelse(itemmaterial=="none",NA, percent_ignoringnones))  %>% filter(itemmaterial!="none") %>% 
  ggplot(aes(x = "", y = count, fill = itemmaterial))+geom_bar(stat = "identity", width = 1)+coord_polar("y", start = 0)+theme_void()+scale_fill_manual(values = c("gray","purple", "lime green", "brown", "dark blue", "dark green"))

# most prominent item types in vancouver----
van_type %>% group_by(itemtype) %>% summarize(count = n(), percent_ignoringnones = count/nrow(subset(van_type,van_type$itemtype != "other"))) %>% mutate(percent_ignoringnones = ifelse(itemtype=="other",NA, percent_ignoringnones))





## Item Locations----
# Region with highest litterati count vs. region with highest municipal count
van_count%>% group_by(year, mapid, litt_count) %>% summarize(muni_mapid_total = sum(muni_total_count))
        #top 2 highest count 2017-- MUNI: STR & KC @179 items, litt: WE & MP at 29 & 26 items. 2018-- MUNI: RC & KC at 226 & 190. litt: MP & CBD. 2019 -- MUNI: CBD & RC, litt: WE and CBD



## Regression----
# check correlation plot
van_count_num = van_count %>% mutate(mapid_num = as.numeric(as.factor(mapid)))
van_count_correlation = round(cor(van_count_num[,c("muni_total_count","muni_large_count","muni_small_count","litt_count","litt_count_binary","litt_users", "fast.food.or.convenience.store", "bus.stop.within.site.or.sight.", "litter.bin", "total_311","total311perpop", "inat_submissions", "inat_users","inatuserperpop","year", "mapid_num","total_population", "average_age_of_the_population", "percent_commutebikeorwalk", "percent_immigrants", "totalhouseholdincome_median", "percent_nodegree")], use = "complete.obs"),2)
corrplot(van_count_correlation)

# remove percent_nodegree bc seems like it it super correlated with many things including [-] inat_subs, inat_users, litt_users, %bikeorwalk, income [+]%immigrant, age
# remove year bc super correlated with inat_submissions & inat_users (as years go on, participants of smartphone based citsci increases)
# removed mapid bc the information is largely duplicitously stored as demographic info
# use population density instead of total population
# removed percent_commutebikeorwalk bc super correlated with [+] population density, litt_count, litt_users, litter bins, 311, inat_users & [-] percent_immigrants
#Correlation take 2:
van_count_correlation2 = round(cor(van_count[,c("muni_total_count","litt_count","litt_users", "fast.food.or.convenience.store", "bus.stop.within.site.or.sight.", "litter.bin", "total_311","total311perpop", "inat_submissions", "inat_users","inatuserperpop","pop_density_m2", "average_age_of_the_population", "percent_immigrants", "totalhouseholdincome_median","percent_commutebikeorwalk", "percent_nodegree")], use = "complete.obs"),2)
corrplot(van_count_correlation2)
library(performance)

# fit model
model <- lm(muni_total_count~litt_count+litt_users + fast.food.or.convenience.store+bus.stop.within.site.or.sight.+litter.bin+total_311+total311perpop+inat_submissions+inat_users+inatuserperpop+pop_density_m2+average_age_of_the_population+percent_immigrants+totalhouseholdincome_median+percent_commutebikeorwalk+percent_nodegree, data = van_count)
model_centered <- lm(muni_total_count~I(litt_count-mean(litt_count))+I(litt_users-mean(litt_users)) + I(fast.food.or.convenience.store-mean(fast.food.or.convenience.store))+I(bus.stop.within.site.or.sight.-mean(bus.stop.within.site.or.sight.))+I(litter.bin-mean(litter.bin))+I(total_311-mean(total_311))+I(total311perpop-mean(total311perpop))+I(inat_submissions-mean(inat_submissions))+I(inat_users-mean(inat_users))+I(inatuserperpop-mean(inatuserperpop))+I(pop_density_m2-mean(pop_density_m2))+I(average_age_of_the_population-mean(average_age_of_the_population))+I(percent_immigrants-mean(percent_immigrants))+I(totalhouseholdincome_median-mean(totalhouseholdincome_median))+I(percent_commutebikeorwalk-mean(percent_commutebikeorwalk))+I(percent_nodegree-mean(percent_nodegree)), data = van_count)

# now check for multicollinearity
check_collinearity(model_centered)

# Simplest OLS model of municipal count by site
lm_municount = lm(data = van_count, muni_total_count~littcountperuser+I(litt_users/total_population)+street.type+ area+ grass.height+ as.factor(fast.food.or.convenience.store)+ as.factor(bus.stop.within.site.or.sight.)+ as.factor(litter.bin)+ I(total_311/total_population)+ inat_submissions+ I(inat_users/total_population)+ total_population+ average_age_of_the_population+ percent_commutebikeorwalk + percent_immigrants + totalhouseholdincome_median + percent_nodegree)
summary(lm_municount)
anova(glm_municount)

# poisson, fixed-effect
glm_municount = glm(data = van_count, as.integer(muni_total_count)~littcountperuser+I(litt_users/total_population)+street.type+ area+ grass.height+ fast.food.or.convenience.store+ bus.stop.within.site.or.sight.+ litter.bin+ I(total_311/total_population)+ inat_submissions+ I(inat_users/total_population)+ total_population+ average_age_of_the_population+ percent_commutebikeorwalk + percent_immigrants + totalhouseholdincome_median + percent_nodegree, family = "poisson")
summary(glm_municount)
anova(glm_municount)

# glmm poisson
    # what predicts municipal count? [litterati count/user, streettype, area, grassheight, shops, busstop, litterbin, 311calls/population, inatsubmissions, inatusers/totalpopulation, totalpopulation, age, walkers, immigrant, income, education]
    # as.integer() truncates unexplained 0.5 counts
    ##?? SHould litterati be a predictor here?
glmm_municount = glmer(data = van_count, as.integer(muni_total_count)~littcountperuser+I(litt_users/total_population)+street.type+ area+ grass.height+ fast.food.or.convenience.store+ bus.stop.within.site.or.sight.+ litter.bin+ I(total_311/total_population)+ inat_submissions+ I(inat_users/total_population)+ total_population+ average_age_of_the_population+ percent_commutebikeorwalk + percent_immigrants + totalhouseholdincome_median + percent_nodegree+(1|siteid), family = "poisson")
summary(glmm_municount)
anova(glmm_municount)
step(glmm_municount, direction = "both")

# glmm poisson, but without percent_nodegree
# what predicts municipal count? [litterati count/user, streettype, area, grassheight, shops, busstop, litterbin, 311calls/population, inatsubmissions, inatusers/totalpopulation, totalpopulation, age, walkers, immigrant, income, education]
# as.integer() truncates unexplained 0.5 counts
glmm_municount_lessdemographics = glmer(data = van_count, as.integer(muni_total_count)~littcountperuser+I(litt_users/total_population)+street.type+ area+ grass.height+ fast.food.or.convenience.store+ bus.stop.within.site.or.sight.+ litter.bin+ I(total_311/total_population)+ inat_submissions+ I(inat_users/total_population)+ total_population+ average_age_of_the_population+ percent_commutebikeorwalk + percent_immigrants + totalhouseholdincome_median +(1|siteid), family = "poisson")
summary(glmm_municount_lessdemographics)


# glmm poisson, but with variables scaled
# what predicts municipal count? [litterati count/user, streettype, area, grassheight, shops, busstop, litterbin, 311calls/population, inatsubmissions, inatusers/totalpopulation, totalpopulation, age, walkers, immigrant, income, education]
# as.integer() truncates unexplained 0.5 counts
##?? SHould litterati be a predictor here?
glmm_municount = glmer(data = van_count, as.integer(muni_total_count)~littcountperuser+I(litt_users/total_population)+street.type+ area+ grass.height+ fast.food.or.convenience.store+ bus.stop.within.site.or.sight.+ litter.bin+ I(total_311/total_population)+ inat_submissions+ I(inat_users/total_population)+ total_population+ average_age_of_the_population+ percent_commutebikeorwalk + percent_immigrants + totalhouseholdincome_median + percent_nodegree+(year|siteid), family = "poisson")
summary(glmm_municount)

# glmm poisson, but with municipal count averaged by neighborhood
# what predicts municipal count? [litterati count/user, streettype, area, grassheight, shops, busstop, litterbin, 311calls/population, inatsubmissions, inatusers/totalpopulation, totalpopulation, age, walkers, immigrant, income, education]
# as.integer() truncates unexplained 0.5 counts

glmm_municount_byarea = glmer(data = van_count, as.integer(muni_meancount_area)~I(litt_count/litt_users)+I(litt_users/total_population)+ I(total_311/total_population)+ inat_submissions+ I(inat_users/total_population)+ total_population+ average_age_of_the_population+ percent_commutebikeorwalk + percent_immigrants + totalhouseholdincome_median + percent_nodegree+(year|siteid), family = "poisson")
summary(glmm_municount_byarea)

# what predicts litterati count?
van_count_inat = van_count %>% mutate(litt_count_binary =ifelse(litt_count>0,1,0),inat_subperuser = ifelse(inat_users == 0,0,ifelse(inat_submissions ==0,0,inat_submissions/inat_users)))
glmm_littcount = glmer(data = van_count_inat, as.integer(litt_count_binary)~I(total_311/total_population)+ inat_subperuser+ I(inat_users/total_population)+ total_population+ average_age_of_the_population+ percent_commutebikeorwalk + percent_immigrants + totalhouseholdincome_median + percent_nodegree+year +(1|mapid) , family = "binomial")
summary(glmm_littcount)
    # what predicts litterati count? RESCALED
van_count_inat = van_count %>% filter(litt_count>10) %>% mutate(inat_subperuser = ifelse(inat_users == 0,0,ifelse(inat_submissions ==0,0,inat_submissions/inat_users)))
glmm_littcount = glmer(data = van_count_inat, as.integer(litt_count)~I(total_311/total_population*100)+ inat_subperuser+ I(inat_users/total_population*100)+ I(total_population/1000)+ average_age_of_the_population+ I(percent_commutebikeorwalk*100) + I(percent_immigrants*100) + I(totalhouseholdincome_median/1000) + I(percent_nodegree*100)+year +(1|mapid), family = "poisson")
summary(glmm_littcount)

van_count_inat %>%  ggplot(aes(x = total_population, y = muni_total_count))+geom_point(aes(color = mapid))+geom_smooth(se = F, method = lm)

# glmm gamma for density?
    # what predicts municipal count density?
glmm_munidensity = glmer(data = van_count, muni_countperm2~I(litt_count/litt_users)+I(litt_users/total_population)+street.type+ area+ grass.height+ fast.food.or.convenience.store+bus.stop.within.site.or.sight.+ litter.bin+ I(total_311/total_population)+ inat_submissions+ I(inat_users/total_population)+ total_population+ average_age_of_the_population+ percent_commutebikeorwalk + percent_immigrants + totalhouseholdincome_median + percent_nodegree+(1|siteid), family = Gamma(link=identity))
summary(glmm_munidensity)


ggplot(data = van_count, aes(x = muni_total_count, y = muni_meancount_area))+geom_point()



