 
 <h3 align="center">Lot 4 Retinoids and Valproates study scripts, Release V3.0</h3>
 <p align="center"> RELEASE NOTE: V3.0 - Final main analyses 
1.	All main descriptive analyses implemented for study objectives 1-4 (See protocol v1.1/SAP v2.0)
2.	Error in pre-select filtering corrected (DAPs requested to re-run pre-select in next major script update V3.0
3.	pregnancy counts per month added; datasets of disp/presc during pregnancy now saved in g_intermediate
4.	pooled analyses provided for BIFAP
5.	free text information for PHARMO and CASERTA (to be tested by each group)
6.	A new to_run file, to extract additonal information on pregnancies during which a valproate/retinoid prescription/dispensing record was detected.
7.	Adjustment to the scripts that detect diagnostic/procedure codes to correct issue with detect of codes with "."s and irrelevant codes.


<!-- IMPORTANT NOTES -->
All groups are requested to run this script as soon as possible and report to UU/UMCU and issues encountered. Once results are generated, please upload to YODA.
Following this, each group is requested to review the output using the guidance document (to be shared 10th Feb 2022).

The output of these analyses will be used in interrupted time series analyses (ITSA), run by the UMCU team. The results of the ITSA will be uploaded to the DAP-specific folders on YODA. For transparency, the ITSA script is also part of the release.

The following missing features will be implemented in the next release(s):
1.	Stratified counts (age group, indication (valproate), reason for discontinuation, dose, time on treatment, 
2.	Sensitivity analyses (different time windows for the definitions of discontinuation, and concomitant pregnancy testing & contraception with exposure)
3.	Medical observations codes (mainly relevant for pregnancy test data)
4.	DAP-specific duration of treatment estimation (as described in Annex 7 of SAP). Currently a fixed duration of 30 days (with 30 day permissible gap) is implemented for valproates and retinoids, and fixed durations for contracecptions, as described in SAP v2.0 are implemented.
5.	Additional DAP-specific specifications or sensitivity analyses (as described in Annex 7 of SAP)
6.	Correction to rate graphs (calculations and y-axis labels). These currently only show the numerator counts standardized by the denominator.
7.	Clarified flow chart xlsx/csv files in g_output/preliminary_counts


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
