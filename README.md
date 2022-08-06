# Litterati vs. Municipal Audits
How useful is crowd-sourced litter data at a city scale? And what about the most expensive municipal litter audits? How do they compare and do they deliver information of interest to scientific analysis of plastic pollution dynamics in urban areas?<br><br>

  
Analysis will be made available upon publication: <br>
*Watkins L, Bonter, DN, Sullivan P, Walter MT. (In prep). Methods for monitoring urban street litter: A comparison of munisipal audits and an app-based citizen science approach. Submitted to Marine Pollution Bulletin*
      
## Contents of this repo
*****
1. `Vancouver_analysis.R`: code used to produce analysis and plots included in our manuscript <br>
2. ``: code used to produce analysis and plots included in our paper published in *Science of the Total Environment* (2019). <br>
3. `Microplastics-dams.Rproj`: If running this locally, .RProj file will allow `.R` code to find `.csv` data file. Be sure to store `.Rproj` file in the same directory with `.R` and `.csv` to be able to run.
<br>

## Contents of subdirectory `data_Litterati`
*****
This subdirectory contains 5 `.csv` files shared with Lisa Watkins by Dick Ayers of Litterati. These files contain all available Litterati submissions for each listed city (San Francisco, San Jose, Toronto, Vancouver, and Washington DC). These cities were requested by Lisa because they contained (relatively) large amounts of Litterati submissions and also had several years of publically available litter audits that had been conducted by paid consulting firms.

## Contents of subdirectory `data_municipalaudits`
*****
This subdirectory contains the publically available litter audit `.pdf` reports for San Francisco, California ("SF"), Toronto, Canada, and Vancouver, Canada, which Lisa Watkins was able to locate by scouring the internet. `Vancouver_fullaudit` indicates these files contain appendices in addition to the report. These `full` versions were aquired by contacting the City of Vancouver. `Vancouver_municipalresponses` and 'SanJose_motivations_email' include the email responses Lisa solicited from municipal workers at each respective city to learn about the city's motivations for collecting this data and their uses for the data.

## Notes about this project
*****
* Goal for this project: to figure out whether these two readily available, but non-traditional datasources (Litterati citizen science app data & municipal litter audits) may be useful for answering scientifically relevant questions about plastic pollution and urban litter.
    
* Data was publically available and existing. Aquired data is included in the two `data_` subdirectories.

* Additional data manipulation was performed in QGIS.
<br>
    
## Metadata about this project & our methods
*****
        
#### Column headers of Dams_Microplastics_full.csv (UPDATE THIS TEMPLATE)   

1. `DamID` the code given to each individual dam in the study.    
+ `FR` = Flatrock, Fall Creek (42.4546706,-76.4562607)    
+ `BL` = Beebe Lake, Fall Creek (42.4519266,-76.4795704)     
+ `SD` = Sediment Dam, Six Mile Creek (42.4091457,-76.4537273)    
+ `3D` = 3rd Dam, Six Mile Creek (42.4137404,-76.5299637)     
+ `2D` = 2nd Dam, Six Mile Creek (42.4247749,-76.5444474)     
+ `1D` = 1st Dam, Six Mile Creek (42.4329239,-76.4848986)
     
2. `Weight` entered **in kg** for both surface water and sediment samples. Surface water weights are calculated based on an estimated density of 1kg/L. Where for water samples, `Weight` = Sample Volume.
     
3. `nFiber`-`nBead` The count of particles of each category found via visual inspection for a given sample.


    
<br>
    
## Want to use this data?  
*****
    
*Attribution 4.0 International*<br>
We are pleased to allow this data to be used freely in the public, with proper attribution. Please cite this data or the resulting publication:   <br>     
*Watkins L, Bonter DN, Sullivan PJ, Walter MT. (In Prep). Methods for monitoring urban street litter: A comparison of municipal audits and an app-based citizen science approach. Marine Pollution Bulletin.
