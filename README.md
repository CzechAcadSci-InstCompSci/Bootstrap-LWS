# Nonparametric bootstrap for estimating variability of robust regression estimators, version 1.0

The code in R software performs a nonparametric bootstrap computation of the covariance matrix for several available linear
regression estimators: S-estimators, MM-estimators, least trimmed squares, and 
least weighted squares estimators. Especially for the least weighted squares estimator,
this software allows a unique approximate computation, because the explicit formula
for the covariance matrix depends on the unknown random errors. It is the main advantage
of the presented (nonparametric) procedure that it does not rely on probabilistic
assumptions for the random regression errors.

Feel free to use or modify the code.

## Requirements

You need to install the robustbase package of R software.

Available at https://cran.r-project.org/web/packages/robustbase/index.html

## Usage

* mainBootstrap.m  Main file, which creates an artificial dataset, performs the bootstrap study, and presents the results. The complete usage is explain in the documentation within the file.

## Authors
  * Jan Kalina, The Czech Academy of Sciences, Institute of Computer Science
  * Jan Tichavský, The Czech Academy of Sciences, Institute of Computer Science
  * Nicole Tobišková, The Czech Academy of Sciences, Institute of Computer Science

## Contact

Do not hesitate to contact us (kalina@cs.cas.cz) or write an Issue.

## How to cite

Please consider citing the following:

Kalina J, Tobišková N, Tichavský J (2019). A nonparametric bootstrap comparison of variability of robust regression estimators. Proceedings 37th 
International Conference on Mathematical Methods in Economics MME 2019, University of South Bohemia in České Budějovice, České Budějovice, pp. 168-173.

Maechler M, Rousseeuw P, Croux C, Todorov V, Ruckstuhl A, Salibián-Barrera M, Verbeke T, Koller M, Conceicao ELT, di Palma MA (2019). robustbase: 
Basic Robust Statistics R package version 0.93-5. URL http://CRAN.R-project.org/package=robustbase

## Acknowledgement

This work was supported by the Czech Science Foundation grant 19-05704S.