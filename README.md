# facsR
quickly plot FACS data

### Usage

In this Shiny app you can upload one or many FACS files (FCS format, I have uploaded several examples in the `data` folder) and plot them. So far I have tested files from BD Accuri C6, if you have data from other machines and the app is not working please let me know, it should be pretty easy to handle this.   
The fastest way to get going is to execute this in R Studio:

`shiny::runGitHub("facsR", "angelovangel")`

then the app is going to run in your R session. There is also an Amazon cloud instance of this app, which you can access [here](http://35.176.52.165/shiny/rstudio/facsR/). Note that on the Amazon cloud version I have set a file size limit of 30 Mb, while this version has no such limit.
The libraries needed by the app are `shiny`, `shinydashboard`, `tidyverse`, `rbokeh`, `RColorBrewer`, `flowCore` and `scales`, so install them first...

Happy facsing!

