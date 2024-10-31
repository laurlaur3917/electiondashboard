#load libraries 

library(sf)
library(ggplot2)
library(tidyverse)
library(highcharter)
library(jsonlite)
library(geojsonio)

#load data 
load("data/usdata2.RData")
load("data/mapcolors.RData")

#Transform shp to geojson 
mapjson<- geojson_json(us_data2)

#read geojson in 
Myhcmap <- jsonlite::fromJSON(mapjson, simplifyVector = FALSE)


#create simpler categories, create a -100 to 100 scale for democrat % of vote to republican % of vote. 
highcharttest2<- mapcolors %>% mutate(color= case_when(dominantparty=="Republican" ~ republicancolor, dominantparty=="Democrat" ~ democratcolor), winningpercentage=ifelse(dominantparty=="Republican", republican_raw_votes_2perc*100, -democratic_raw_votes_2perc*100))

#set NA winning percentage values to 0 
highcharttest2<- highcharttest2 %>% mutate(winningpercentage= ifelse(is.na(winningpercentage), 0, winningpercentage))

#filter out values which have NA county_names, geo_id, and geometries. 
highcharttest2 <- highcharttest2 %>% 
  filter(!is.na(county_name) & !is.na(GEO_ID) & !is.na(geometry))

#prepare data for highcharter
ds<- highcharttest2 %>% group_by(GEO_ID) %>% do(item = list(
  GEO_ID = first(.$GEO_ID),
  NAME= first(.$county_name),
  sequence = .$winningpercentage,
  value = first(.$winningpercentage))) %>% 
  .$item


#create custom html label 
svg_content2<- 
  "
  <div style= 'font-family: AppleGothic; font-size: 12px; font-style:italic; font-weight:bold; transform:translateY(-30px)'>
    <svg width='210' height='120'>
      <rect x='4' y='16' width='35' height='20' stroke='black' fill='#cce0ff' stroke-width='3'/>
      <rect x='39' y='16' width='35' height='20' stroke='black' fill='#3a80ec' stroke-width='3'/>
      <rect x='74' y='16' width='35' height='20' stroke='black' fill='#0066cc' stroke-width='3'/>
      <rect x='109' y='16' width='35' height='20' stroke='black'fill='#1000ff' stroke-width='3'/>
      <rect x='144' y='16' width='35' height='20' stroke='black'fill='#0200b9' stroke-width='3'/>
      <text x='4' y='48' font-size='4' text-anchor='middle' fill='black' font-weight='bold'>50%</text>
      <text x='39' y='48' font-size='4' text-anchor='middle' fill='black' font-weight='bold' >60%</text>
      <text x='74' y='48' font-size='4' text-anchor='middle' fill='black' font-weight='bold'>70%</text>
      <text x='109' y='48' font-size='4' text-anchor='middle' fill='black' font-weight='bold'>80%</text>
      <text x='144' y='48' font-size='4' text-anchor='middle' fill='black' font-weight='bold'>90-100%</text>
    
      <text x='4' y='10' font-size='13' text-anchor='middle' fill='black' font-weight='bold' font-style='italic' font-family='Georgia, serif'>Democrat Vote %:</text>
      <rect x='4' y='73' width='35' height='20' stroke='black'fill='#ffbaba' stroke-width='3'/>
      <rect x='39' y='73' width='35' height='20' stroke='black'fill='#ff7b7b' stroke-width='3'/>
      <rect x='74' y='73' width='35' height='20' stroke='black'fill='#ff5252' stroke-width='3'/>
      <rect x='109' y='73'width='35' height='20' stroke='black'fill='#ff0000' stroke-width='3'/>
      <rect x='144' y='73'width='35' height='20' stroke='black'fill='#a70000' stroke-width='3'/>
      <text x='4' y='105' font-size='4' text-anchor='middle' fill='black' font-weight='bold'>50%</text>
      <text x='39' y='105' font-size='4' text-anchor='middle' fill='black' font-weight='bold' >60%</text>
      <text x='74' y='105' font-size='4' text-anchor='middle' fill='black' font-weight='bold'>70%</text>
      <text x='109' y='105' font-size='4' text-anchor='middle' fill='black' font-weight='bold'>80%</text>
      <text x='144' y='105' font-size='4' text-anchor='middle' fill='black' font-weight='bold'>90-100%</text>
      <text x='4' y='67' font-size='13' text-anchor='middle' fill='black' font-weight='bold' font-style='italic' font-family='Georgia, serif' >Republican Vote %:</text>
    </svg>
  </div>"

#create tooltip table to put into highcharter
x<-c("Winning Party:", "Winning Party's % of overall vote: ", "State:", "County Name:")
y<- c("{point.dominantparty}", "{point.value:,.2f}%", "{point.properties.STATE}", "{point.NAME}") 
tt<- tooltip_table(x,y)

#create election map in highcharter, with functions in javascript to populate the tooltip correctly 
electionmap<- highchart(type = "map") %>%
  hc_add_series(data= ds, mapData= Myhcmap, name= "% of vote", joinBy= "GEO_ID", showInLegend = T, borderColor="black", borderWidth=0.2) %>% 
  hc_colorAxis(dataClasses=color_classes(breaks=c(-100, -90, -80, -70, -60, -50, 0, 50, 60, 70, 80, 90, 100), 
                                         colors=c("#0200b9", "#1000ff", "#0066cc", "#3a80ec","#cce0ff","#cce0ff",  
                                                  "gray20", "#ffbaba", "#ffbaba", "#ff7b7b", "#ff5252", "#ff0000", "#a70000"))) %>%
  hc_tooltip(pointFormat=tt, useHTML=TRUE, formatter = JS("
    function() {
    var point = this.point;

    // Helper function to capitalize the first letter of each word
    function capitalizeWords(string) {
        if (!string) return '';  // Handle empty/null values safely
        return string.split(' ').map(function(word) {
            return word.charAt(0).toUpperCase() + word.slice(1).toLowerCase();
        }).join(' ');
    }

    // Determine the winning party based on the raw value (before applying abs)
    var winningParty;
    if (point.value >= 50 && point.value <= 100) {
        winningParty = 'Republican';
    } else if (point.value <= -50 && point.value >= -100) {
        winningParty = 'Democrat';
    } else {
        winningParty = 'Other';
    }

    // Use the original point format and replace the value with its absolute value
    var tooltip = this.series.tooltipOptions.pointFormat
        .replace('{point.dominantparty}', winningParty) // Replace winningParty first
        .replace('{point.value:,.2f}', Math.abs(point.value).toFixed(2)); // Then replace value

    // Manually add other properties to the tooltip
    tooltip = tooltip
        .replace('{point.NAME}', capitalizeWords(point.NAME)) // Capitalize first letter of each word in NAME
        .replace('{point.properties.STATE}', point.properties ? capitalizeWords(point.properties.STATE) : 'N/A'); // Capitalize STATE

    return tooltip;
}")) %>% hc_motion(enabled=TRUE, axisLabel="election_year", 
                   labels=c(1916, 1920, 1924, 1928, 1932, 1936, 1940, 1944, 1948, 1952, 1956, 1960, 
                            1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 
                            2004, 2008, 2012, 2016, 2020), series=0, updateInterval=50, 
                   magnet=list(round="floor", step=0.1), autoplay=TRUE, playIcon = "fa fa-play", pauseIcon = "fa fa-pause") %>% 
  hc_legend(enabled=FALSE) %>% hc_title(text= "A Century of Votes: Unpacking Electoral Shifts Across the U.S. from 1916 to Today", 
                                        style = list(color = "black", fontWeight = "bold", fontFamily="AppleGothic", fontSize="20px")) %>% 
  hc_subtitle(text="A Deep Dive into County-Level Vote Shares Across 83,561 Data Points", 
              style=list(color="black", fontFamily="AppleGothic", fontSize="15px")) %>% 
  hc_caption(text=svg_content2, useHTML=TRUE) %>% hc_chart(marginBottom=128) %>% hc_mapNavigation(enabled = TRUE)
