
# cdspecR

![](cd_spec.png)

The goal of cdspecR is to ... 

1) automate the process of transforming raw data from circular
dichroism (CD) experiments 
2) provide high-quality, data visualization while incorporating user-friendly options for personalizing graphs 
3) streamline the data analysis process for extracting key information about the
biochemistry of the system 

## Installation


``` r
if(!require(remotes)){
  install.packages("remotes")
}
remotes::install_github("jnguyen01/cdspecR")
```

## Example

``` r
library(cdspecR)
## basic example code

#Import CSV Files 
protein <- importCD()  

#Optional Plot of Seeing CD Thermal Spectras 
plotCDSpectra(protein)

# Data Analysis of CD 

protein_210 <- plotCDMelt(protein, wavelength=210) 

thermodynamics <- plotCDVantHoff(protein_210, folded_temp=10, unfolded_temp=95) 


```

