# hexcrawlR

This an R shiny app for generating hex terrain maps with elevation and land cover information. 
It is intended for use in tabletop role-playing games, but could likely be adapted with
ease for data visualization purposes.

To use the app, click on the map to place the center of your desired hex grid. Adjust
the sliders to your desired height and width, and press 'Generate'. 

Right now, due to storage limitations and , the rasters used to supply elevation and land 
use information are not stored in the repository. They can be found [here](https://www.ncei.noaa.gov/products/etopo-global-relief-model) and [here](https://data.apps.fao.org/map/catalog/srv/eng/catalog.search#/metadata/ba4526fd-cdbf-4028-a1bd-5a559c4bff38). To use 
them in the app, just change the directories in the opening lines of app.R.
