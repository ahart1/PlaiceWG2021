# PlaiceWG2021

This repository holds code associated with work for the American Plaice 2021 Working Group ToR4. Preliminary files correspond to models fit to VPA data prior to final WG ToR2&3 data decisions. Preliminary data may not match the data used in VPA-WHAM bridge runs and these preliminary analyses should be viewed as a WHAM learning exercise. Files used in WG analysis include "WG" in the description below.

### wham_setup
A folder containing recommended fixes for the preliminary plaice model (using VPA data) and wham examples including environmental impacts on catchability. 
| File | Description |
| ------| -----------|
| Plaice_noMADMF.dat | Copy of data file to run preliminary plaice model, excludes MADMF surveys. |
| example.R | Walks through resolving selectivity problem for preliminary models, catchability examples. |
| doodles.R | Additional preliminary WHAM debugging |

### data
A folder containing ASAP input data files for WG modeling. 
| File | Description |
| ------| -----------|
| PlaiceASAP1-2019VPAINPUT.DAT | WG data file to run VPA-ASAP bridge run, also used for VPA-WHAM bridge run. Includes age-independent indices. |
| PlaiceWHAM1-2019VPAINPUT.DAT | WG data file to run VPA-WHAM bridge run, same data as the above file but indices are biomass aggregated. |
| PlaiceWHAM-2019.DAT | WG data file containing updated data following WG decisions (additional year of catch data, updated indices, updated maturity, M=0.3). |
| PlaiceWHAM-2019_selectivity.DAT | WG data file containing updated data as in above file but change selectivity settings for NEFSC surveys so selectivity-at-age estimated. Often overwritten in R WHAM model specification. |
| PlaiceWHAM-2019_VPAMaturity.DAT | WG data file containing updated data and selectivity as in above file EXCEPT for the maturity which matches that in the VPA-WHAM bridge run (PlacieWHAM1-2019VPAINPUT.DAT). |

### Other files
| File | Description |
| ------| -----------|
| Plaice.dat | Preliminary ASAP data file containing all survey and fishery data from VPA. |
| Plaice_noMADMF.dat | Preliminary ASAP data file including fishery data from VPA, excludes MADMF surveys. |
| Plaice_WHAM.Rmd | Preliminary modeling script to read in, process, and fit model to plaice VPA data. Several iterations to improve selectivity specification following recommendations in wham_setup/example.R |
| Plaice_Bridge_Run.Rmd | WG R markdown document to run VPA-WHAM bridge runs. |
| Plaice_Baseline_UpdatedData_Runs.Rmd | WG R markdown document detailing WHAM model runs using updated data. |


