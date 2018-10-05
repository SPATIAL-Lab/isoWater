# watercompare

Scripts for paper on isotopic comparison of water samples including evaporation:

Bowen, G. J., Putman, A., Brooks, J. R., Bowling, D. R., Oerter, E. J., & Good, S. P. (2018). Inferring the source of evaporated waters using stable H and O isotopes. Oecologia, 187(4), 1025-1039.

watercomp.R contains functions for conducting source water analysis

manuscript_analyses.R contains functions used to conduct analyses and make figures appearing in manuscript

grid_calculations.R contains code for doing runoff-weighted precipitaiton isotope accumulations, modeling EL slopes, and prepping lake water dataset for subsequent analyses; requires tau_dem command line utilities

epa_data.csv contains lake water dataset, exported from Waterisotopes.org database, replicate measurements for individual sites averaged
