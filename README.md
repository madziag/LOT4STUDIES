 
 <h3 align="center">Lot 4 Retinoids and Valproates study scripts, Release V2.4.1</h3>
 <p align="center"> RELEASE NOTE: V2.4.1 
 <p align="center"> i) DAP-specific issues resolved (BIFAP, Danish registers) 
 <p align="center"> ii) error in pre-select filtering corrected (DAPs requested to re-run pre-select in next major script update V3.0
 <p align="center"> iii) pregnancy counts per month added; datasets of disp/presc during pregnancy now saved in g_intermediate
 <p align="center"> Next major release (V3.0) will provide final study counts for objectives 1-4
 
<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary>Table of Contents</summary>
  <ol>
    <li><a href="#Lot4">Lot 4 studies</a></li>
    <li><a href="#Scripts">Study scripts</a></li>
    <li><a href="#Getting-started">Getting started</a></li>
    <li><a href="#License">License</a></li>
    <li><a href="#Contact">Contact</a></li>
  </ol>
</details>

<!-- LOT4 -->
## Lot 4 studies

Information on the EMA-funded [Lot 4 retinoids](https://www.encepp.eu/encepp/viewResource.htm?id=31096) and [Lot 4 valproates](https://www.encepp.eu/encepp/viewResource.htm?id=36586) can be found on the EU PAS register.

<!-- SCRIPTS -->
## Study scripts

**GitHub resources for the LOT4 studies**      
The following GitHub repositories are used in the Lot 4 studies:
1.	Quality checks I [Level 1 checks](https://github.com/IMI-ConcePTION/Level-1-checks)
2.	Quality checks II [Level 2 checks](https://github.com/IMI-ConcePTION/Level-3-checks)   
3.	Quality checks III [Level 3 checks](https://github.com/IMI-ConcePTION/Level-4-checks) 
4.	Scripts for creating sets of pregnancies from study data, to be used in the analyses [ConcePTIONAlgorithmPregnancies](https://github.com/ARS-toscana/ConcePTIONAlgorithmPregnancies)
5.	The study analysis script suite

**Study analysis script suite is divided into the following parts:**   

1.	***ConcePTION pregnancy script***. This is stored alongside the LOT4_scripts in the RELEASE folder. If pregnancy information is available, this should be run first.
2.	***preselect***. This script creates a subfolder where your CDM data is stored and creates a copy of the data files, excluding male subjects, subjects outside of the eligible birthdate range, and for MEDICINES, excludes irrelevant ATC classes.
3.	***source_population_counts***. This is a generic analysis that is run for both the retinoids and valproates studies. This script 
i) creates a study population, excluding ineligiable subjects, 
ii) creates counts of the occurrence of event (clinical event, prescription/dispensing, procedure) records in the data for concept sets in the study per month, 
iii) counts the number of persons observed in the data per month
iv) plots the counts (and rates per 1000 persons per month) and saves the output locally. 
4.	***to_run_final_counts.R***.  This script does the following:
i) creates a simple baseline table, where the anchor point will be the moment an individual first is eligible to be observed during the study period. 
ii) creates counts/plots of the number/rate of retinoid/valproate prescriptions that occurred during a (possible) pregnancy per month.
iii) Next major release will provide counts/plots of the number of prevalence users, incident users, discontinuers and switchers of valproate; pregnancy tests before/after exposure and exposure during contraception coverage (the remaining final analyses)
5. 	***ITSA_scripts*** These scripts will be run by the study statisticians to conduct the time series analyses using outputs from 4ii/iii uploaded to YODA.

<!-- GETTING-STARTED -->
## Getting Started

Follow the steps below to run the scripts

### Prerequisites

R version 4.1.0 (2021-05-18)   

### Installation

1. Download the ZIP folder and extract the contents. ONLY KEEP THE RELEASE FOLDER. Discard the DEVELOPMENT folder.  
2. Copy the LOT4_scripts folder from the RELEASE folder into the same local folder as where you store the CDMInstances folder as well as Level 1-3 check script folders  
3. In the folder `LOT4_scripts`, go to the script 99_path.R and change the variable Studyname(line 6) to LOT4.     
4. Now you are ready to run the (((to_run))) files. Each of these files runs a different analysis. Run them in the order listed above. If you choose to use the subsetted files created by the preselection script, you will need to adjust your path in the 99_path.R file to find the preselect data files. 

Contact Romin (R.Pajouheshnia@uu.nl) if you need help with this.

### Additional user inputs

1. study: in the "to_run" files, select retinoid, valproate or both
2. Excel or csv output: You can select for output data files (for export) to be either .xlsx or .csv format
3. Subpopulation/Region analysis (BIFAP): In the study script "to_run" files, set the options: regions and subpopulations == T in the scripts

### Uploading results to the online research environment

After running the scripts, please upload the LOT4_scripts/g_output folder to YODA (your DAP-specific Analysis_scripts subfolder) with the following naming convention: "g_outupt_DDMMYYYY", where DDMMYYYY is the date of running the script.

IMPORTANT: Although the scripts should suppress any values below 5, please do double check this when inspecting the results, before uploading to YODA.

NO FILES FROM g_intermediate should be uploaded to YODA or shared.

<!-- LICENSE -->
## License

Distributed under the BSD 2-Clause License License. See `LICENSE` for more information.

<!-- CONTACT -->
## Contact

Romin Pajouheshnia - R.pajouheshnia@uu.nl
Ema Alsina - palsinaaer@gmail.com  
Magdalena Gamba - m.a.gamba@uu.nl
Vjola Hoxhaj - v.hoxhaj@umcutrecht.nl     
Carlos Duran Salinas - c.e.duransalinas@umcutrecht.nl
