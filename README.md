
# cdspecR

![](cd_spec.png)

The goal of cdspecR is to ... 

1) automate the process of transforming raw data from circular
dichroism (CD) experiments 
2) provide high-quality, data visualization while incorporating user-friendly options for personalizing graphs 
3) streamline the data analysis process for extracting key information about the
biochemistry of the system 

## How To Install: 

``` r

#Copy and Paste The Following Code: 
if(!require(remotes)){
  install.packages("remotes")
}
remotes::install_github("jnguyen01/cdspecR")
```

## How To Use These Functions:

``` r
#Load The Package
library(cdspecR)

# Step 1 - Use importCD 
protein <- importCD() 

# [Optional] Use plotCDSpectra to see all CD Spectra on one graph. 
plotCDSpectra(protein) 

# Step 2 - Use plotCDMelt 
melt <- plotCDMelt(protein, wavelength=220)

# Step 3 - Use analyzeCDMelt 
analyze <- analyzeCDMelt(melt)

# Step 4 - use plotCDVH
vh <- plotCDVH(analyze)



```
