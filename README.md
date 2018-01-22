# Textonomer: Interactive Applications to Explore Textual Corpora

This directory contains two interactive Shiny application for visualizing corpora. This README focuses on the treemap application, built for Dr. Brochstein of Rice University by Alex Hayes (alexpghayes@gmail.com).

### Project Layout

```
/home             -- contains landing page Shiny app file
/treemap          -- contains treemap Shiny app and helper code
README.md         -- this file
```

To run the code you will need to edit the `data_dir` variable in `utils.r` to correspond to the path to the data on your computer.

### Notes / Issues / Todo

* Loading notification lasts for a fixed 3.5 seconds
* Data subsetting necessarily changes cell sizes across n-1 levels of an n level treemap

### Dependencies

All dependencies are on CRAN, although the treemaps look best if you use `highcharter` 0.5.0.9999+. As of July 7, 2017 this means you should install the development version from Github. Something along these lines should get you most if not all the way there:

```r
install.packages(c("shiny", "shinydashboard", "shinycssloaders" "tidyverse", "visNetwork", "devtools", "DiagrammeR", "data.tree"))
devtools::install_github("jbkunst/highcharter")
```

### Usage

From the R console:

```r
shiny::runApp('treemap')
# shiny::runApp('network_graph')
# shiny::runApp('home')  # just a landing page that links to both apps
```

### Deployment to shinyapps.io

Get the `textonomer` Shinyapps.io account info from Dr. Brochstein. On Shinyapps.io there are instructions on how set up authentication. Follow those then run:

```r
# install.packages("rsconnect")
rsconnect::deployApp('/path/to/treemap/app', account = 'textonomer')
```

### Treemap App News

* v9: set max num cells at bottom level of treemap to 750, use css loader
* v8: remove Title field and fix bug where Unknown Publisher was most frequent category but wasn't removed
* v7: aesthetic tweaks: include missing unless largest category, notify user of treemap creation, change title, add title field, some more hierarchy limitations
* v6: custom highcharter treemap wrapper with minimal JS output for speed, fixes issues with three level charts
* v5: two level serialized plot objects, menu restrictions, add word count feature
* v4: serialized highcharter plot objects, dealing with issues scaling to full dataset
* v3: query limits for top level heirarchy, generate treemap on button press, publisher and language coding cleaned up some
* v2: d3treeR --> highcharter, shiny --> shinydashboard
* v1: rough prototype up
