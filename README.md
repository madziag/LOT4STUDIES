 
 <h3 align="center">Lot 4 Retinoids and Valproates study scripts</h3>
 <p align="center"> R scripts for the analyses of the Lot 4 retinoids and valproates studies. </p>
 
<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
    </li>
    <li>
      <a href="#level-3-checks">Level 3 checks</a>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
        <li><a href="#subpopulation">Subpopulation analysis</a></li>
        <li><a href="#uploading">Uploading results to the online research environment</a></li>
        <li><a href="#links">Data characterization study links</a></li> 
        <li><a href="#version">Current version</a></li>
      </ul>
    </li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>

<!-- Lot 4 studies -->
## Lot 4 studies

Information on the EMA-funded [Lot 4 retinoids](https://www.encepp.eu/encepp/viewResource.htm?id=31096) and [Lot 4 valproates] (https://www.encepp.eu/encepp/viewResource.htm?id=36586) can be found on the EU PAS register.

<!-- Scripts -->
## Study scripts

**GitHub resources**      
The following GitHub repositories are used in the Lot 4 studies:
**1.**	Quality checks I [Level 1 checks](https://github.com/IMI-ConcePTION/Level-1-checks)
**2.**	Quality checks II [Level 2 checks](https://github.com/IMI-ConcePTION/Level-3-checks)   
**3.**	Quality checks III [Level 3 checks](https://github.com/IMI-ConcePTION/Level-4-checks) 
**4.**  Scripts for creating sets of pregnancies from study data, to be used in the analyses [ConcePTIONAlgorithmPregnancies](https://github.com/ARS-toscana/ConcePTIONAlgorithmPregnancies)  
**5.**  The study analysis script suite    



**Study analysis script suite is divided in 7 parts:**   

1.	***preselect***. This script creates a subfolder where your CDM data is stored and creates a copy of the data files, excluding male subjects, subjects outside of the eligible birthdate range, and for MEDICINES, excludes irrelevant ATC classes.
2.	***source_population_counts***. This script i) creates a study population, excluding ineligiable subjects, ii) creates counts of the occurrence of event (clinical event or prescription/dispensing) records in the data for concept sets in the study per month, iii) counts the number of persons observed in the data per month, iv) plots the counts and saves the output locally. This is a generic analysis that is run for both the retinoids and valproates studies. In the first version of this function (25 NOV 2021), this counts records from the EVENTS and MEDICINES tables. An update is scheduled (29 NOV 2021), where records will be counted from the PROCEDURES and MEDICAL_OBSERVATIONS tables, counts will be stratified by (((meaning))) and (((origin))), rates of records (number of records per person month) will be calculated and plotted, and there will be an option to specify the format of output (.csv or .xls). 
3.	***baseline***.  This script will create a simple baseline table, where the anchor point will be the moment an individual first is eligible to be observed during the study period. There will be two version of this script, one for retinoids and one for valproates. (((Planned release date: 06 NOV 2021)))
4.	***objective1***. This script will produce the analysis results for Objective 1. There will be two version of this script, one for retinoids and one for valproates. (((Planned release date: 06 NOV 2021)))
5.	***objective2***. This script will produce the analysis results for Objective 2. There will be two version of this script, one for retinoids and one for valproates. (((Planned release date: 06 NOV 2021)))
6.	***objective3***. This script will produce the analysis results for Objective 3. There will be two version of this script, one for retinoids and one for valproates. (((Planned release date: 06 NOV 2021)))
7.	***objective4*** This script will produce the analysis results for Objective 4. There will be two version of this script, one for retinoids and one for valproates. (((Planned release date: 06 NOV 2021)))


<!-- GETTING STARTED -->
## Getting Started

Follow the steps below to run the scripts

### Prerequisites

R version 4.1.0 (2021-05-18)   

### Installation

1. Download the ZIP folder and extract the contents. ONLY KEEP THE RELEASE FOLDER. Discard the DEVELOPMENT folder.  
2. Copy the LOT4_scripts folder from the RELEASE folder into the same local folder as where you store the CDMInstances folder as well as Level 1-3 check script folders  
3. In the folder `LOT4_scripts`, go to the script 99_path.R and change the variable Studyname(line 6) to LOT4.     
4. Now you are ready to run the (((to_run))) files. Each of these files runs a different analysis, as listed above. If you have not run the preselect script yet, do this first by running the script "to_run_preselect". We ask that you run this and compare this against your CDM instance tables. You may run the following analysis scripts on either your original data OR the preselection data. If you don't wish to use the subsetted preselection files, you may delete them. If you choose to use the subsetted files created by the preselection script, you will need to adjust your path in the 99_path.R file to find the preselect data files (otherwise R will automatically detect the original CDM files. Contact Romin (R.Pajouheshnia@uu.nl) if you need help with this.
You may now run the "to_run_source_population_counts" fil
Lastly, in the "to_run" files, make sure that mask = T, so that values <5 are masked. Currently this is implemented by converting them to 5. This will be changed in an update.


### Subpopulation analysis (THIS WILL BE UPDATED FOR 29 NOV UPDATE)

A subpopulation analysis can be performed if your data has different provenance(i.e. different levels of the healthcare system such as hospital data and general practitioner data etc). This analysis helps to identify errors for each specific data sub sample. If you already know that your data quality is similar you can skip this analysis.    

To run the analysis scripts with subpopulation analysis follow the next steps:      
1. Complete the `METADATA` table accordingly. In `type_of_metadata = subpopulations` in the column `values`, add all your subpopulations of interest separated by space. Leave `tablename`, `columnname` and `other` columns empty. Example if you have hospital(HOSP) data and primary care(PC) data you will add `HOSP PC` to the `values` column.     
2. If you want to analyse the overlap between different subpopulations, add first_subpopulation-sencond_subpopulation in `type_of_metadata = subpopulations` in the column `values` Example if you look at the overlap between hospital data and primary care data add `HOSP-PC` to the `values` column.        
3. In `type_of_metadata = op_meaning_sets` in the column `values` specify each meaning set referring to a subpopulation. Separate meaning sets by space In the column `other` add the name of the subpopulation. Leave `tablename` and `columnname` empty. Example if for the primary care data you will add the meaning sets meaningsPC and meaningsPHARMA you will add in the `other` column, `PC` and in the `values` column, `meaningsPC meaningsPHARMA`.      
4. In `type_of_metadata = op_meanings_list_per_set` in the `values` column add all the meanings that should be part of a meaning set and in the `other` column add the name of the meaning set. Leave the `tablename` and `columnname` empty. Example if the meaning set `meaningsPC` contains the meanings primary_care, primary_care_2, and primary_care_3 you will add to the `values` column `primary_care primary_care_2 primary_care_3` and in the `other` column `meaningsPC`. Separate values by space.      
5. If you want to exclude a specific meaning of a CDM table from a subpopulation, add in `type_of_metadata = exclude_meaning` in the column `tablename` the name of the CDM table, in the column `other` the name of the subpopulation and in the column `values` the meanings to be excluded. Separate meanings by space. Leave the `columnname` column empty. Example of you want to exclude the meaning pc_exclude part of the `EVENTS` table from the subpopulation primary care than you will add `EVENTS` to the column `tablename`, `PC` to the `other` column and `pc_exclude` to the `values` column.    
6. You are now ready top run a subpopulation analysis.

<img src="images/metadata_example.png" alt="Logo" width="700" height="120">

### Uploading results to the online research environment

###The current version of the script is 1.0.

<!-- LICENSE -->
## License

Distributed under the BSD 2-Clause License License. See `LICENSE` for more information.

<!-- CONTACT -->
## Contact

Romin Pajouheshnia - R.pajouheshnia@uu.nl
Ema Alsina - palsinaaer@gmail.com  
Magdalena Gamba
Vjola Hoxhaj - v.hoxhaj@umcutrecht.nl     
