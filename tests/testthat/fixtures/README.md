I attest that all `.Rdata` and `.rds` files in this directory are generated and in no way use real study data. They are created by running the three `.R` files in this directory in the following order:

1. `make-nb_datasets.R`
This program creates negative binomially distributed datasets (without and with covariates) that reflect a dose response.

2. `make-nb_mcmc+_objects.R`
This program fits the datasets generated in step 1 to various (individual) dose-response models using `rjags`.

3. `make-nb_bma_objects.R`
This program takes the results from step 2 to produce Bayesian Model Averaged objects/results.

Note that the files produced by `make-nb_bma_objects.R` may show up as superficially different when the code is re-executed. To verify that the content of files (produced by all three `R` programs) is effectively identical, run the following code:
```
# Load old data
pkgload::load_all() # from the root directory of the {beaver} source code
env_old <- new.env()
for (file in list.files("tests/testthat/fixtures", pattern = "\\.Rdata$", full.names = TRUE)) {
  load(file, envir = env_old)
}
for (file in list.files("tests/testthat/fixtures", pattern = "\\.rds$", full.names = TRUE)) {
  env_old[[basename(file)]] <- readRDS(file)
}

# Remove old data
unlink(file.path("tests/testthat/fixtures", c("*.Rdata", "*.rds")))

# Recompute data
source("tests/testthat/fixtures/make-nb_datasets.R")
source("tests/testthat/fixtures/make-nb_mcmc+_objects.R")
source("tests/testthat/fixtures/make-nb_bma_objects.R")

# Load new data
env_new <- new.env()
for (file in list.files("tests/testthat/fixtures", pattern = "\\.Rdata$", full.names = TRUE)) {
  load(file, envir = env_new)
}
for (file in list.files("tests/testthat/fixtures", pattern = "\\.rds$", full.names = TRUE)) {
  env_new[[basename(file)]] <- readRDS(file)
}

# Compare data
stopifnot(all(names(env_new) == names(env_old)))
for (name in names(env_new)) {
  stopifnot(identical(env_new[[name]], env_old[[name]]))
}
```

For reference:
```
> sessionInfo()
R version 4.2.2 Patched (2022-11-30 r83413)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Red Hat Enterprise Linux

Matrix products: default
BLAS/LAPACK: [redacted]

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] beaver_0.2.0-1 testthat_3.1.5

loaded via a namespace (and not attached):
 [1] pkgload_1.3.2     tidyr_1.2.1       brio_1.1.3        shiny_1.7.3      
 [5] assertthat_0.2.1  diffobj_0.3.5     remotes_2.4.2     sessioninfo_1.2.2
 [9] pillar_1.8.1      backports_1.4.1   lattice_0.20-45   glue_1.6.2       
[13] digest_0.6.30     promises_1.2.0.1  checkmate_2.1.0   colorspace_2.0-3 
[17] htmltools_0.5.4   httpuv_1.6.6      pkgconfig_2.0.3   devtools_2.4.5   
[21] purrr_0.3.5       xtable_1.8-4      scales_1.2.1      processx_3.8.0   
[25] rjags_4-13        later_1.3.0       tibble_3.1.8      generics_0.1.3   
[29] ggplot2_3.4.0     usethis_2.1.6     ellipsis_0.3.2    cachem_1.0.6     
[33] withr_2.5.0       lazyeval_0.2.2    cli_3.4.1         magrittr_2.0.3   
[37] crayon_1.5.2      mime_0.12         memoise_2.0.1     evaluate_0.18    
[41] ps_1.7.2          fs_1.5.2          fansi_1.0.3       xml2_1.3.3       
[45] pkgbuild_1.4.0    rsconnect_0.8.28  profvis_0.3.7     tools_4.2.2      
[49] prettyunits_1.1.1 cyclocomp_1.1.0   lifecycle_1.0.3   stringr_1.5.0    
[53] munsell_0.5.0     callr_3.7.3       rex_1.2.1         compiler_4.2.2   
[57] covr_3.6.1        rlang_1.0.6       grid_4.2.2        rstudioapi_0.14  
[61] htmlwidgets_1.5.4 miniUI_0.1.1.1    rmarkdown_2.18    waldo_0.4.0      
[65] yodel_1.0.0       gtable_0.3.1      DBI_1.1.3         rematch2_2.1.2   
[69] R6_2.5.1          knitr_1.41        dplyr_1.0.10      fastmap_1.1.0    
[73] utf8_1.2.2        rprojroot_2.0.3   lintr_3.0.2       desc_1.4.2       
[77] stringi_1.7.8     Rcpp_1.0.9        vctrs_0.5.1       xfun_0.35        
[81] tidyselect_1.2.0  urlchecker_1.0.1  coda_0.19-4      
```

-Hollins Showalter (hollins.showalter@gmail.com)
