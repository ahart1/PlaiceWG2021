### Self-tests

This folder contains scripts and data used to conduct self-tests for the three WG candidate runs.

| Folder          | Description                                                                                                                                                                                |
|-----------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| plots           | Contains plots included in the peer review report, mistakenly draws on only a subset of the self-test results (pull from plotData_Run29F2.rds, plotData_Run29F4.rds, plotData_Run29F5.rds) |
| plots_Corrected | Contain corrected plots with full set of self-test results (pull from plotData_Run29F2CORRECTED.rds, plotData_Run29F4CORRECTED.rds, plotData_Run29F5CORRECTED.rds)                         |

| File                                          | Description                                                                                 |
|-----------------------------------------------|---------------------------------------------------------------------------------------------|
| **Scripts for self-test analysis**            |                                                                                             |
| Self_Test_Script_tjm.R                        | Final self-test script used in WG analysis, including corrected plots beginning on line 210 |
| Self_Test_Script.Rmd                          | Early version of self-test script                                                           |
| **Full self-test simulated data for fitting** |                                                                                             |
| simdata_OMRun29F2_tjm.rds                     | Run 29F2 simulated data for self tests                                                      |
| simdata_OMRun29F4_tjm.rds                     | Run 29F4 simulated data for self tests                                                      |
| simdata_OMRun29F5_tjm.rds                     | Run 29F5 simulated data for self tests                                                      |
| **Full self-test result files**               |                                                                                             |
| reps_Run29F2.rds                              | Run 29F2 report data                                                                        |
| reps_Run29F4.rds                              | Run 29F4 report data                                                                        |
| reps_Run29F5.rds                              | Run 29F5 report data                                                                        |
| sdreps_Run29F2.rds                            | Run 29F2 sdreport data                                                                      |
| sdreps_Run29F4.rds                            | Run 29F4 sdreport data                                                                      |
| sdreps_Run29F5.rds                            | Run 29F5 sdreport data                                                                      |
| sim_seeds.rds                                 | Seeds used for simulating test data                                                         |
| **Incorrectly subsetted plot data**           |                                                                                             |
| plotData_Run29F2.rds                          | Run 29F2 incorrectly subsetted data used in peer review report plots                        |
| plotData_Run29F4.rds                          | Run 29F4 incorrectly subsetted data used in peer review report plots                        |
| plotData_Run29F5.rds                          | Run 29F5 incorrectly subsetted data used in peer review report plots                        |
| **Correctly subsetted plot data**             |                                                                                             |
| plotData_Run29F2CORRECTED.rds                 | Run 29F2 corrected plotting data with all self-test runs that converged                     |
| plotData_Run29F4CORRECTED.rds                 | Run 29F4 corrected plotting data with all self-test runs that converged                     |
| plotData_Run29F5CORRECTED.rds                 | Run 29F5 corrected plotting data with all self-test runs that converged                     |
