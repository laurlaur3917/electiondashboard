#Interactive Election Map and Trend Graphs 

##Description: 
An interactive map visualizing U.S. county voting patterns from 1916 to 2020. Users can explore vote shares by party, examine trends over time, and dive into county-level data. Each county is categorized by voting trends, with visualizations showing aggregated vote percentages by category, the location of counties in each category, and individual county's vote percentages over the years.

##Installation:
Prerequisites
[Quarto](https://quarto.org/) – To compile the dashboard
R – For data processing and visualizations
Libraries:
R: ggplot2, highcharter, sf, tidyverse, jsonlite, geojsonio, tidyr, purrr, broom 

##Download/Usage Instructions: 
1. Clone the Repository:
Download the project files by cloning the repository: git clone https://github.com/laurlaur3917/electiondashboard.git
2. Open RStudio:
Launch RStudio after cloning the repository.

3. Open the Quarto Project:
Click on File -> Open Project...
Navigate to the cloned project folder and select the .Rproj file. Opening the .Rproj file will automatically set the working directory correctly.

4. Install Required R Packages
   
5. Preview the Dashboard:
Run the following command in the R console to build and preview the dashboard: quarto preview

6. Open Provided Link In Your Browser

##Data Sources
Data is sourced from: 

1. Algara, Carlos; Sharif Amlani, 2021, "Replication Data for: Partisanship & Nationalization in American Elections: Evidence from Presidential, Senatorial, & Gubernatorial Elections in the U.S. Counties, 1872-2020", [https://doi.org/10.7910/DVN/DGUMFI](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DGUMFI), Harvard Dataverse, V1; dataverse_shareable_gubernatorial_county_returns_1865_2020.Rdata [fileName]

2. U.S. Census Bureau. (2010). gz_2010_us_050_00_5m.zip [Cartographic Boundary Files - 	Shapefile]. U.S. Census Bureau. [https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2010.html#list-tab-1556094155 ](https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2010.html#list-tab-1556094155)


##Contact: 

Lauren Gerber: laurenbgerber22@gmail.com | https://www.linkedin.com/in/lauren-bgerber/ 



