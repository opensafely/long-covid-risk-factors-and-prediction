import delimited "output\data.csv", clear

stset lcovid_surv, failure(lcovid_cens)

stci
