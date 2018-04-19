# this code is about
# reading fcs files (from FACS) and converting them to one dataframe

## sourced from the facsR app

library(flowCore)
library(tidyverse)
library(scales)


### read.fcs
### this function reads a fcs file and returns the data as a tibble, adding a column with the file name #####
### the "sample" name is made of the last word before ".fcs", no spaces etc. included

# will have to modofy the read function to take 2 arguments, just like in TRACEview
read.fcs <- function(x, y) {
  df <- read.FCS(x) %>% exprs() %>% as.tibble() %>% 
    mutate(filename = y, sample = str_extract(filename, "\\w+(?=.fcs)"),
           Time = Time/600) # in minutes
  
  names(df) <- names(df) %>% gsub(pattern = "-| ", replacement = ".") # this gsub is not very stable for different machines...works for fortessa and accuri though
  return(df)
}


### process.fcs
### this function 



### logtrans.fcs
### log transform the data

logtrans.fcs <- function(x) {sign(x) * log10(abs(x))}
