## About the App

The [Polya Urn Simulator]( https://caron.shinyapps.io/Women-Men-Polya-Urns/) is introduced and discussed in the paper "Women, Men, and Polya Urns: Underrepresentation at Equal Talent in the Absence of Discrimination" by Laura Caron, Alessandra Casella, and Victoria Mooers (in progress, September 2021). Please cite the paper for results obtained using this app. 

The app runs using R and a package called Shiny, which is what turns it from regular code into an interactive web app. The following provides instructions for someone who has never used R to run the app on their own computer.

## Installing R and RStudio

R is free to download. It works best with an IDE, most commonly RStudio. This is also free to download. You will then have to install several packages to run the app. 

### Installing R

Download R from [here](http://lib.stat.cmu.edu/R/CRAN/). Follow the instructions to install it as you would any program. The app currently runs on R version 4.1.1.

### Installing RStudio

Download the free version of RStudio (RStudio Desktop) from [here](https://www.rstudio.com/products/rstudio/download/) and install it as you would with any program. It should automatically locate your R installation. 

### Installing Rtools

R needs an additional helper program for installing certain packages. Download Rtools from [here](https://cran.r-project.org/bin/windows/Rtools/) and install it as you would with any program. Sometimes, R does not recognize the installation of Rtools, so you can make sure that it works by running the following lines (assuming it installed in the default location): 

```{r eval=FALSE}

## Install the devtools package to allow you to change the path 
install.packages("devtools")

## Load the devtools package
library(devtools)

## Set the environment variable for the path to include Rtools
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:/rtools40", "C:/rtools40/usr/bin/", sep=";"))

## Check that it worked
Sys.which("make")

```

The final line tells you whether the installation was successful. If it returns the word "make" and empty quotes, it did not work. If it returns a file path, then it worked. 

### Installing Packages

We have just installed base R. R also runs with many packages to give it additional functionality. These can be installed from within the program. The app uses the R package renv to manage the other packages it needs. This package allows you to make sure you are installing the same version of the package that the app uses, i.e., to make sure that the app will not break if one of the packages releases an update that is not compatible anymore. 

Renv should install itself when you first open the app. However, just in case, open RStudio and copy the follow command into the RStudio console to install the packages you will need to run the app. 

```{r eval=FALSE}

## Install the renv package
install.packages("renv")

## Tell renv to install the rest of the packages with their correct versions
renv::restore()

```


## Getting the App from Github

Visit the Github repository [here](https://github.com/laurakcaron/Polya-Urns-Simulations). The repository has two "branches", or two versions of the app. The default is the beta version. Most users should use the branch button to change to the public release. Then, click on the green button that says Code. Download the zip file and unzip it. Alternatively, clone the repository using git.

*Note: Do not rename any of the files. The code must be named app.R in order to run.*

The code is in app.R. The other files are for the packages. 

## Running the App

You can run the app by opening the file and pressing the "Run App" button in RStudio. Alternatively, you can use the following command in an R script or in the R console. The app will open in a pop-up window. If it asks whether you want to active the project, type Y in the console and press enter.

![](https://github.com/laurakcaron/Polya-Urns-Simulations/blob/main/info/run%20app%20button.PNG?raw=true)

Please note that R requires you to use the forward slash or escape the backslash in your file paths. 

```{r eval=FALSE}

## Set your working directory to the folder that contains the app
setwd("C:/Users/user/")

## Run the app
shiny::runApp()

```

