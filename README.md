### What is ChangePoint? ###
Changepoint is a reference-free damage estimation for ancient metagenomic data sets. The tool, developed by Yichen Liu and Adam Rohrlach (manuscript forthcoming), assesses the deamination proportions of samples to determine whether they are truly ancient. The advantage of Changepoint is that it is reference-free and can serve as a quick check to flag samples that may not be ancient. You should still run Changepoint flagged samples through the MapDamage Authentication pipeline to check that the results corroborate with each other. Changepoint is broken down to three steps: (1) Subsampling sequences to 100K in the Terminal (Linux/bash), (2) Running Changepoint mathematical model in the Terminal (Linux/bash) with subsampled sequences, (3) Running the visualization script in R.  

### R for Changepoint ###
Changepoint needs to be run in both the Terminal (linux/bash), and the R console. In running Changepoint, you will need R version 4.3.2 (2023-10-31). To check the R version on your local drive run the following commands in the R console. If you need to download the compatiable version of R, please refer to the [R Repository](https://www.r-project.org/):
```{r}
R.Version()
```
For the full session information (versions of loaded packages, R, and other dependencies) of R running in the background of R Studio, you can run 
```{r}
sessionInfo()
```
Changepoint will not run on R versions later than 4.3.2! Be sure to check the R version before proceeding. Those using Mac iOS, you can switch versions of R if using [RSwitch](https://rud.is/rswitch/) 


