# Municipal Audits vs. Litterati
How do crowd-sourced litter audits perform when using them to learn about patterns in urban street litter? How do municipal litter audits do for the same? And do these two non-traditional sources of information deliver data that can be useful for scientific analysis of street litter and plastic pollution dynamics in urban areas?<br><br>
This repository contains the data we collected to answer these questions, as well as the code we used for the analysis.
<br><br>

  
The associated manuscript is submitted for peer-review<br>
*Watkins L, Bonter, DN, Sullivan P, Walter MT. (Submitted). Methods for monitoring urban street litter: A comparison of munisipal audits and an app-based citizen science approach. Submitted to Marine Pollution Bulletin*
      
## Contents of this repo
*****
1. `Vancouver_analysis.R`: code used to produce analysis and plots included in our manuscript <br>
2. `Vancouver_analysis.Rproj`: R project file associated with code file. This should be downloaded into the same directory as the `.R` file and `data_...` folders to allow code to properly locate associated files. <br>
3. `data_Litterati`: Folder containing original data shared from Litterati (in `Data_from_litterati.zip`), to be used as reference, as well as processed versions (with `_qgis` in file name) used in `.R` code.
4. `data_municipalaudits`: Folder containing original `.PDF` municipal litter audit files for Vancouver (2015, 2017, 2018, 2019), San Francisco (2007, 2008, 2009, 2014), and Toronto (2012, 2016), to be used as reference, as well as the transcribed, `.csv` versions of the Vancouver audits, to be used in the `.R` code.
5. `data_Vancouver311`: Folder containing Vancouver's 311 hotline data (.csv), to be used in the `.R` code.
6. `data_iNaturalist`: Folder containing iNaturalist submissions (.csv), to be used in the `.R` code.
7. `data_demographics`: Folder containing 2016 census demographic data (.csv), to be used in the `.R` code.

**All folders contain file `data_source_url.txt` that describes each file contained therein and where the file was downloaded from or how the file was created**
<br>

## Contents of subdirectory `data_municipalaudits`
*****
This subdirectory contains the publically available litter audit `.pdf` reports for San Francisco, California ("SF"), Toronto, Canada, and Vancouver, Canada, which Lisa Watkins was able to locate by scouring the internet. `Vancouver_fullaudit` indicates these files contain appendices in addition to the report. These `full` versions were aquired by contacting the City of Vancouver. `Vancouver_municipalresponses` and 'SanJose_motivations_email' include the email responses Lisa solicited from municipal workers at each respective city to learn about the city's motivations for collecting this data and their uses for the data.<br><br>
Also in this folder are `Vancouver_*YEAR*_site_*SIZE*_count`, which is a transcription of the *large* and *small* columns in Appendix F of the `Vancouver_fullaudit` PDF reports. Similarly `Vancouver_*YEAR*_site_characteristics_transcribed.csv` is a transcription of Appendix C from `Vancouver_fullaudit` of the corresponding year.

## Contents of subdirectory `data_Litterati`
*****
This subdirectory contains a `.zip` file containing 5 `.csv` files shared with Lisa Watkins by Dick Ayers of Litterati. These files contain all available Litterati submissions for each listed city (San Francisco, San Jose, Toronto, Vancouver, and Washington DC). These cities were requested by Lisa because they contained (relatively) large amounts of Litterati submissions and also had several years of publically available litter audits that had been conducted by paid consulting firms. <br><br>Also in this folder are `litterati_nomapid_butinlimits_qgis.csv` (Vancouver litterati submissions from 2017-2019 that fell slightly outside of Vancouver municipal area), `Vancouver_litterati_withlocalareaid_qgis.csv` (Vancouver litterati submissions from 2017-2019 spatially joined in QGIS with Vancouver municipal areas), and `vancouver_litteraticensusareastreettype_qgis.csv` (Vancouver litterati submissions from 2017-2019 spatially joined in QGIS with Vancouver municipal areas and the street type, e.g. major or minor arterial where item was recorded). 

##Contents of subdirectory `data_iNaturalist`
*****
This subdirectory contains the iNaturalist submissions, available for public download from *iNaturalist.org*, occuring in the Vancouver area 2017-2019: `inaturalist_2017_2018_2019.csv`. It also contains these data spatially joined in QGIS with Vancouver local area boundaries for use in the `.R` code: `inaturalist_censusarea_qgis.csv`.

##Contents of subdirectory `data_Vancouver311`
*****
This subdirectory contains the records from Vancouver's 311 hotline, available for public download from *opendata.vancouver.ca/explore/dataset/3-1-1-contact-centre-metrics/information/*, these specifically contain only those that occurred during September of 2017, 2018, and 2019: `*YEARMONTH*CaseLocationsDetails.csv`

##Contents of subdirectory `data_demographics`
*****
This subdirectory contains the records from Vancouver's 2016 census results, available for public download from *opendata.vancouver.ca/explore/dataset/census-local-area-profiles-2016/information/*: `Vancouver_localarea_demographics_download.csv`

## Notes about this project
*****
* Goal for this project: to figure out whether these two readily available, but non-traditional datasources (Litterati citizen science app data & municipal litter audits) may be useful for answering scientifically relevant questions about plastic pollution and urban litter.
    
* Data was publically available and existing. All data utilized for this study are available in the `data_` folders of this repository. The only data not available through free download over the web were the appendices of municipal audits, which we received quickly after an email to the City of Vancouver (audit versions without appendices were available for download without contacting the city).

* Additional data manipulation was performed in QGIS. All QGIS results needed for analysis are included in the `.csv` files contained in the `data_` folders of this repository.
<br>
    
## Metadata about this project & our methods
*****
        
#### Column headers used in `data_Litterati` files

1. `field_1`: A descriptor automatically generated by QGIS
2. `id`: The Litterati submission ID
3. `lat` and `long`: the latitude and longitude of the submitted item
4. `time`: submission date and time of day
5. `tags`: Item descriptors submitted by the Litterati user
6. `url`: A link to download the associated image submitted for that item by the user
7. `challenges`: An associated event used intentionally by a user to link their submissions to an ongoing event or data drive, where applicable
8. `username`: Litterati user who submitted the record
9. `country_code`: Litterati-generated location of submission
10. `mapid`: Appended with QGIS, the Vancouver [local area](opendata.vancouver.ca/explore/dataset/local-area-boundary) abbreviation.
11. `name`: Appended with QGIS, the Vancouver [local area](opendata.vancouver.ca/explore/dataset/local-area-boundary) name.
12. `year` and `month` and `day`: year, month, and day of collection, as determined in `R` from `time` column.
13. `hblock`: Appended with QGIS, the block name where submission `lat` and `long` are located, as determined from Vancouver's [public streets database.](opendata.vancouver.ca/explore/dataset/public-streets/)
14. `streetuse`: Appended with QGIS, the street type associated with the `hblock`, as determined from Vancouver's [public streets database.](opendata.vancouver.ca/explore/dataset/public-streets/)

#### Column headers used in `data_iNaturalist` files
1. `area_m2`: The area of the associated `mapid` AKA [local area](opendata.vancouver.ca/explore/dataset/local-area-boundary), wherein the iNaturalist totals are aggregated.
2. `Municipal_*YEAR*`: The total item count from municipal audits that occurred in the associated [local area](opendata.vancouver.ca/explore/dataset/local-area-boundary) for the given *YEAR*.
3. `Litterati_*YEAR*`: The total Litterati submissions that occurred in the associated `mapid` AKA [local area](opendata.vancouver.ca/explore/dataset/local-area-boundary) for the given *YEAR*.
4. `Sept311calls_*YEAR*`: The total calls into Vancouver's 311 hotline that occurred in the associated `mapid` AKA [local area](opendata.vancouver.ca/explore/dataset/local-area-boundary) in September of the given *YEAR*.
5. `id`: iNaturalist submission ID
6. `observed_on`: Date of iNaturalist observation
7. `time_observed_at`: Date and time of iNaturalist observation
8. `user_id`: ID of the user who submitted the observation to iNaturalist
9. `created_at`: Time and Date of iNaturalist submission
10. `quality_grade`: internal iNaturalist qualitative assessment of the submission
11. `tag_list`: iNaturalist observed item description
12. `place_guess`: Address nearest to iNaturalist submission location
13. `latitude` and `longitude`: Location of iNaturalist submission
14. `scientific_name`: Observed iNaturalist item's scientific name

#### Column headers used in `data_municipalaudits` files
1. `siteid`: ID of municipal audit site, as defined by auditors
2. `street type`: Type of street where audit was located, as defined by auditors
3. `area`: Audit site's local zoning, as defined by auditors
4. `grass height`: Height of grass growing near site, as observed by auditors
5. `fast food or convenience store`: *1* if either a fast food location or convenience store were located within sight of the audit location, *0* if neither.
6. `bus stop within site or sight?`: *1* if a bus stop was within the audited site or within sight from the site. *0* if no bus stop could be seen.
7. `litter bin`: *1* if a litter bin was located within the site, *0* if no bin was located within the site.
8. `muni_large`: Count of items greater than 4in located within the audit site
9. `muni_small`: Count of items smaller than 4in located within the subsection of the audit site assessed for small litter (see methods in audit reports)
10. 'area_ft2_large': Site area surveyed

#### Column headers used in `data_demographics` files
1. `ID`: row number
2. `Variable`: Demographic category
3. `Arbutus_Ridge`: `Vancouver_CMA`: Vancouver [local areas](opendata.vancouver.ca/explore/dataset/local-area-boundary) (see: `mapid` and `name`).
*Values contained in this table are in units of numbers of people*

#### Column headers used in `data_Vancouver311` files
1. `Year`, `Month`, `Day`, `Hour`, `Minute`: Time of call to the Vancouver 311 hotline
2. `Department`, `Division`: Office within Vancouver municipal government most associated with the nature of the request or complaint
3. `Case_type`: Category of complaint or request
4. `Hundred_Block`, `Street_Name`, `Local_Area`: Location of complaint or request.
    
<br>
    
## Want to use this data?  
*****
    
*Attribution 4.0 International*<br>
The data we have used for this publication are all publically available. Please cite the original datasource, and not our publication. If instead, you are utilizing our code or analysis, the proper citation would be:<br>     
*Watkins L, Bonter DN, Sullivan PJ, Walter MT. (submitted). Methods for monitoring urban street litter: A comparison of municipal audits and an app-based citizen science approach. Marine Pollution Bulletin.
