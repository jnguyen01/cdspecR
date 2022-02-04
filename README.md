
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

# Step 1 - Import and select folder containing desired CSV files
protein <- importCD() 

# [Optional] Overlay all melting graphs 
spectra(protein) 

# Step 2 - Generate a plot of ellipiticity as a function of temperature
melt <- plotMelt(protein, wavelength=220)

# Step 3 -  Calculate fraction unfolded and plot it as a function of temperature
analyze <- analyze(melt)

# Step 4 - Use van't Hoff equation to approximate thermodynamics parameters of the system
vh <- thermodynamics(analyze)
```
