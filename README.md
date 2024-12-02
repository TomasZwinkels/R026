# Replication Package for "How (not) to Keep a Promise in Two Stages: The Context-Dependent Gaps Between Parties' Representational Goals and Outcomes Through Nomination and Election"

**Note**: A complete version history, including all commits and changes, can be found at [https://github.com/TomasZwinkels/R026](https://github.com/TomasZwinkels/R026), including a formatted version of this README file.

## Overview

This replication package contains all the files necessary to reproduce the results presented in the paper **"How (not) to Keep a Promise in Two Stages: The Context-Dependent Gaps Between Parties' Representational Goals and Outcomes Through Nomination and Election"**, which will be published in *The Journal of Politics*. The core of the analysis is conducted using the R script `R026.R`, which handles all data manipulation and analysis. Specific instructions related to different sections of the paper are included at the top of the `R026.R` file.

## Requirements

- **Operating System**: Any (Windows, macOS, Linux). R is operating system independent, and the results of the script are stable across different systems.
- **Software**: R version 4.4.1 (results are stable across different R versions)
- **Required R Packages**: All required packages are specified at the top of the `R026.R` script.

## File Structure

├── R026.R
├── PCC/
│   ├── POLI.csv
│   ├── PARE.csv
│   ├── RESE.csv
│   ├── MEME.csv
│   ├── PART.csv
│   ├── PARL.csv
│   ├── FACT.csv
│   ├── COMM.csv
│   ├── ELDI.csv
│   ├── ELLI.csv
│   └── ELEN.csv
├── INDA/
│   ├── DG/
│   │   └── 20200818_2020
│   └── OM/
│       └── 20210212_1433
├── NL_boynames.csv
├── NL_girlnames.csv
├── PPDB_Round2_v4.xlsx
└── PCC___codebook_V4_0_0.pdf


## File Descriptions

### Scripts

- **`R026.R`**: The central R script that performs all data manipulation and analysis for the paper. It includes extensive queries to prepare the dataframes for analysis. The required R packages are listed at the top of this script.

  - **Important Notes Regarding `R026.R`**:

    - **Double Gangers Detection**:

      - Around **line 1129**, there is a boolean variable called `runDGagain` that is set to `FALSE` by default.
      - This controls the execution of the double ganger detection process, which determines electable list positions.
      - Setting `runDGagain` to `TRUE` will run this part of the script, but **it takes a long time to execute**.
      - To save time, when `runDGagain` is `FALSE`, the script pulls the results from the file `20200818_2020` located in the `INDA/DG/` folder.
      - This file is included in the replication package to facilitate faster execution.

    - **Optimal Matching Algorithms**:

      - Around **line 2027**, there is a boolean variable called `runOMagain` that is set to `FALSE` by default.
      - This controls the execution of the Optimal Matching algorithms.
      - Running the Optimal Matching algorithms is **time-consuming**.
      - When `runOMagain` is `FALSE`, the script pulls the results from the file `20210212_1433` located in the `INDA/OM/` folder.
      - This file is included in the replication package to speed up the process.
      - You can set `runOMagain` to `TRUE` if you wish to run this part of the script again.

### Data Files

#### PCC Data Files (located in `PCC/` folder)

The analysis is performed on a relational database, and the data is provided as `.csv` files located in the `PCC/` folder. Each dataframe contains information varying on the same level and can be connected using a system of identifiers. Below is a list of all PCC data files included:

1. **`POLI.csv`**: Politician-level data containing stable characteristics. Refer to page 4 in the codebook for details.
2. **`PARE.csv`**: Parliamentary episodes data. See page 5 in the codebook.
3. **`RESE.csv`**: Resume entries of politicians. Details are on page 6 of the codebook.
4. **`MEME.csv`**: Membership episodes data. Refer to page 8 in the codebook.
5. **`PART.csv`**: Information about political parties. See page 9 in the codebook.
6. **`PARL.csv`**: Data on parliaments. Details are on page 7 of the codebook.
7. **`FACT.csv`**: Faction information within parliaments. Refer to page 10 in the codebook.
8. **`COMM.csv`**: Committees data. See page 11 in the codebook.
9. **`ELDI.csv`**: Electoral districts data. Details are on page 15 of the codebook.
10. **`ELLI.csv`**: Electoral lists data. Refer to page 13 in the codebook.
11. **`ELEN.csv`**: Electoral list entries. See page 14 in the codebook.

#### Additional Data Files

- **`NL_boynames.csv`**: A list of common Dutch male first names. This file is used in the `R026.R` script to infer the gender of politicians when the PCC data does not specify it, typically for individuals who never made it to parliament and whose gender is not specified in electoral lists.
- **`NL_girlnames.csv`**: A list of common Dutch female first names. Similar to `NL_boynames.csv`, this file helps determine the gender of politicians lacking gender information in the PCC data.
- **`PPDB_Round2_v4.xlsx`**: Contains data from the Political Party Database (PPDB), which provides additional information on political parties used in the analysis. For more details about the PPDB, please visit the [Political Party Database Project website](https://www.politicalpartydb.org/).

#### INDA Data Files

- **`INDA/DG/20200818_2020`**: This file contains precomputed results from the double ganger detection process. When `runDGagain` in the `R026.R` script is set to `FALSE`, the script uses this file to load the results, saving time by not rerunning the lengthy computation.

- **`INDA/OM/20210212_1433`**: This file contains precomputed results from the Optimal Matching algorithms. When `runOMagain` in the `R026.R` script is set to `FALSE`, the script uses this file to load the results, avoiding the time-consuming computation.

### Documentation

- **`PCC___codebook_V4_0_0.pdf`**: A detailed specification of all variables in the dataframes. It includes examples and explanations of the data structure and the relationships between different dataframes.

## Instructions for Replication

1. **Setup**

   - Install R version 4.4.1 or later. The results are stable across different R versions.
   - Install the required R packages as specified at the top of the `R026.R` script.

2. **Data Preparation**

   - Ensure the following files and folders are placed in the same directory:

     - `README.md`
     - `R026.R`
     - `PCC/` folder containing all `.csv` data files.
     - `INDA/` folder containing subfolders `DG/` and `OM/` with their respective files.
     - `NL_boynames.csv`
     - `NL_girlnames.csv`
     - `PPDB_Round2_v4.xlsx`

3. **Running the Analysis**

   - Open `R026.R` in RStudio or your preferred R environment.
   - Follow the specific instructions provided at the top of the `R026.R` script to locate analyses corresponding to the paper.
   - **Optional Settings**:

     - **Double Gangers Detection**:

       - By default, `runDGagain` is set to `FALSE` to save time.
       - If you wish to rerun the double ganger detection process, set `runDGagain` to `TRUE` around **line 1129**.
       - Be aware that this process is time-consuming.

     - **Optimal Matching Algorithms**:

       - By default, `runOMagain` is set to `FALSE`.
       - To rerun the Optimal Matching algorithms, set `runOMagain` to `TRUE` around **line 2027**.
       - This process is also time-consuming.

   - Run the script to perform the data manipulation and analysis.

## Contact Information

For any questions or issues regarding this replication package, please contact:

- **Tomas Turner-Zwinkels**
- **Email**: tomas.turner-zwinkels@tilburguniversity.edu or tzwinkels@gmail.com

---