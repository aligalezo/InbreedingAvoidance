# InbreedingAvoidance
For reproducing main results of Galezo et al. in prep, "Mechanisms of inbreeding avoidance in a wild primate".

**Analysis Code**
- 1a_Queries.R - SQL to query raw data from Babase
- 1b_TargetRelatives.R - specify target relatives for analysis
- 2a_ClassifyPairs.R - classify pedigree relatedness of all pairs in pedigree
- 2b_IdentifyTargetPairs.R - identify opposite-sex pairs that meet inclusion criteria for analysis
- 3a_CalculateCoresidencies.R - determine dates that relatives coresided in the same social group
- 3b_CoresidencyDescriptiveStats.R - descriptive statistics on coresidency rates
- 3c_PlotCoresidencySankeyDiagrams.R - generates **Figure 1**
- 3d_PlotCoresidencyLengths.R - generates **Supplementary Figures 1 and 3**
- 4_SubfertilityCutoff.R - generates **Supplementary Figure 4**
- 5a_AnalyzeConsorts_DataPrep.R - prepare data for mate choice models
- 5b_AnalyzeConsorts_MainModel.R - runs the Main Model; generates **Table 1**
- 5c_AnalyzeConsorts_MainModel_HybridScore.R - runs the Main Model while accounting for admixture; generates **Supplementary Table 1**
- 5d_AnalyzeConsorts_SubfertilityModel.R - runs the Adolescent Model; generates **Table 2**
- 5e_AnalyzeConsorts_LodgeModel.R - runs the Lodge Model; generates **Supplementary Table 2**
- 5f_PlotPredictedProbabilities.R - generates **Figure 2**
- 6_NatalDispersal.R - generates **Supplementary Figure 2**
- 7_InbreedingOccurrence.R - assess occurrence of inbreeding

**Dryad Data and Reproducibility**
- Data/Processed/sankey_data.csv - this intermediate file can be loaded in 3c_PlotCoresidencySankeyDiagrams.R to generate the data for Figure 1. This can be converted to a Sankey diagram via https://github.com/nowthis/sankeymatic.
- Data/Processed/mate_choice_data.csv - this intermediate file can be loaded in 5b_AnalyzeConsorts_MainModel.R, 5d_AnalyzeConsorts_SubfertilityModel.R, and 5d_AnalyzeConsorts_SubfertilityModel.R to run the 3 key mate choice models and generate Tables 1, 2, and S2. These 3 files also generate 3 output files containing model results (Output/Bayesian Output/mainmodel.RData, Output/Bayesian Output/subfertmodel.RData, Output/Bayesian Output/lodgemodel.RData), which can be loaded in 5f_PlotPredictedProbabilities.R to generate Figure 2.
- Data/Raw/dispersals.csv - this file can be loaded in 6_NatalDispersal.R to generate Figure S2A.
