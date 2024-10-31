
#load libraries, turn off scientific notation 
library(highcharter)
library(dplyr)
library(tidyverse)
options(scipen=999)

#load data
load("data/mergedelectiondata.RData")

#rename states from number identifiers to actual name

mergedelectiondata<- mergedelectiondata %>% mutate(STATE= case_when(
  STATE=="01"~"Alabama", 
  STATE=="04"~"Arizona", 
  STATE=="05"~"Arkansas", 
  STATE=="06"~"California", 
  STATE=="08"~"Colorado", 
  STATE=="09"~"Connecticut",
  STATE=="10"~"Delaware",
  STATE=="11"~"Washington, D.C",
  STATE=="12"~"Florida",
  STATE=="13"~"Georgia",
  STATE=="16"~"Idaho",
  STATE=="17"~"Illinois",
  STATE=="18"~"Indiana",       
  STATE=="19"~"Iowa",      
  STATE=="20"~"Kansas",
  STATE=="21"~"Kentucky",
  STATE=="22"~"Louisiana",
  STATE=="23"~"Maine",      
  STATE=="24"~"Maryland",
  STATE=="25"~"Massachusetts",
  STATE=="26"~"Michigan",
  STATE=="27"~"Minnesota",
  STATE=="28"~"Mississippi",
  STATE=="29"~"Missouri",     
  STATE=="30"~"Montana",
  STATE=="31"~"Nebraska",       
  STATE=="32"~"Nevada",   
  STATE=="33"~"New Hampshire",       
  STATE=="34"~"New Jersey",       
  STATE=="35"~"New Mexico",        
  STATE=="36"~"New York",      
  STATE=="37"~"North Carolina",       
  STATE=="38"~"North Dakota",        
  STATE=="39"~"Ohio",        
  STATE=="40"~"Oklahoma",       
  STATE=="41"~"Oregon",        
  STATE=="42"~"Pennsylvania",       
  STATE=="44"~"Rhode Island",       
  STATE=="45"~"South Carolina",      
  STATE=="46"~"South Dakota",      
  STATE=="47"~"Tennessee",       
  STATE=="48"~"Texas",       
  STATE=="49"~"Utah",       
  STATE=="50"~"Vermont",       
  STATE=="51"~"Virginia",       
  STATE=="53"~"Washington",        
  STATE=="54"~"West Virginia",     
  STATE=="55"~"Wisconsin",       
  STATE=="56"~"Wyoming"))

#create a winning percentage category: when republicans win its positive, when democrats win its negative: 
#multiply the existing percentage categorys by 100 to get 56% rather than (0.56)
mergedelectiondata<- mergedelectiondata %>% 
  mutate(winningpercentage=ifelse(dominantparty=="Republican", republican_raw_votes_2perc*100, -democratic_raw_votes_2perc*100), 
         republican_raw_votes_2perc= republican_raw_votes_2perc*100, democratic_raw_votes_2perc=democratic_raw_votes_2perc*100)



#take the set without geometry and irrelevant columns 
withoutgeomirrel<- (mergedelectiondata[, !names(mergedelectiondata) %in% c("geometry", "republican_excess", "democrat_excess")]) 
#create official regions from states
withoutgeom1<- withoutgeomirrel %>% mutate(region= case_when(STATE %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania") ~ "Northeast", 
                                                             STATE %in% c("Indiana", "Illinois", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota") ~ "Midwest", 
                                                             STATE %in% c("Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas", "Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia", "Washington, D.C") ~ "South",
                                                             STATE %in% c("Arizona", "Colorado", "Idaho", "New Mexico", "Montana", "Utah", "Nevada", "Wyoming", "California", "Oregon", "Washington") ~ "West"))



#filter for 1964-current (the beginning of the latest "party trend/shift")
withoutgeom1<- withoutgeom1 %>% filter(election_year > 1960) 

# librarys for nested regressions: 
library(tidyr)
library(purrr)
library(broom)

#check # of election years for each county

all_combinations <- withoutgeom1 %>%
  expand(GEO_ID = unique(GEO_ID), election_year = unique(election_year))

#compares all_combinations data with withoutgeom1 data, returns rows that are not in withoutgeom1
missing_data <- all_combinations %>%
  anti_join(withoutgeom1, by = c("GEO_ID", "election_year"))

#Because of missing data: standardize election year 
withoutgeom3 <- withoutgeom1 %>%
  group_by(GEO_ID) %>%
  arrange(election_year) %>%
  mutate(year_standardized = ifelse(is.na(election_year), 
                                    NA, 
                                    row_number())) %>% ungroup()

#round the democratic and republican % to 2 decimal points. 
countysummaryy<- withoutgeom3 %>% mutate(democratic_raw_votes_2perc= round(democratic_raw_votes_2perc, 2),
                                         republican_raw_votes_2perc= round(republican_raw_votes_2perc, 2))
#drop observations with missing GEO_IDs
countysummaryy<- countysummaryy %>% filter(!is.na(GEO_ID))


#Group by GEO_ID and nest the data 
bycounty2 <- countysummaryy %>%
  arrange(GEO_ID, election_year) %>%  
  group_by(GEO_ID) %>%
  nest()  

#check that the data is correct for a county with missing election years. 
check2<- bycounty2 %>% filter(GEO_ID == "0500000US04012") %>% pull(data)



#create a function which applies a linear regression in the nested data to every GEO_ID. 
model2<- function(df){lm(republican_raw_votes_2perc~year_standardized, data=df)}
models2<- bycounty2 %>% mutate(mod=map(data, model2))
models2<- models2 %>% mutate(tidy=map(mod, broom::tidy), glance=map(mod, broom::glance), augment=map(mod, broom::augment))
models2<- models2 %>% unnest(tidy)

#filter to include only the year_standardized coefficient, save these values. 
estimates2<- models2 %>% filter(term=="year_standardized")
values2<- estimates2$estimate
values3<- as.data.frame(values2)

#check distribution of coefficients 
values3 %>% ggplot(aes(values2)) + geom_density()
#Take the quartiles of the estimates to group our counties into categories 
quantiles2 <- quantile(values2, probs = seq(0, 1, 0.25)) # Quartiles
#print(quantiles2)

#Group counties into categories based on estimates: 
estimates3 <- estimates2 %>% mutate(Category=cut(estimate, breaks=quantiles2, 
                                                 labels = c("Very Democratic->Slightly Democratic", "Slightly Democratic->Slightly Republican", "Slightly Republican->Moderately Republican", 
                                                            "Moderately Republican->Very Republican"), include.lowest = TRUE, right=TRUE))


#unnest the data
estimates3<- estimates3 %>% unnest(data)


#can't plot every county, get the voting percentage by year for each category for plotting reasons 
estimates4<- estimates3 %>% group_by(Category, year_standardized) %>% summarize(sum_republican_support=sum(republican_raw_votes, na.rm=TRUE), 
                                                                                sum_democrat_support=sum(democratic_raw_votes, na.rm=TRUE), totalvotes= sum(pres_raw_county_vote_totals_two_party), .groups = "drop")
estimates4<- estimates4 %>% mutate(avg_republican_support= sum_republican_support/totalvotes * 100, avg_democrat_support=sum_democrat_support/totalvotes * 100)
estimates4<- estimates4 %>% mutate(avg_republican_support= round(avg_republican_support, 2),
                                   avg_democrat_support= round(avg_democrat_support, 2))

#split the sets based on the category 
vtosdedm2<- estimates4 %>% filter(Category=="Very Democratic->Slightly Democratic")
sdemtosrep2<- estimates4 %>% filter(Category=="Slightly Democratic->Slightly Republican")
sreptomrep2<- estimates4 %>% filter(Category=="Slightly Republican->Moderately Republican")
mtovrep2<- estimates4 %>% filter(Category=="Moderately Republican->Very Republican")


#Simple Line Charts for each Category:
#moderately->very republican
hcmtovrep2<- hchart(mtovrep2, "spline", hcaes(x=year_standardized, y=avg_republican_support), color="red", name="Republican Support %")
hcmtovrep2<- hcmtovrep2 %>% hc_add_series(mtovrep2, type="spline", hcaes(x= year_standardized, y=avg_democrat_support), color="blue", name="Democrat Support %") %>% 
  hc_yAxis(title = list(text = "Percentage Support by Party"), labels = list(format = "{value}%"), gridLineWidth=0) %>% 
  hc_xAxis(title = list(text = "# of Election Cycles Per County Since 1964 (Starting from First Participation)")) %>% 
  hc_title(text = "Vote Share of Counties with the Highest Increases in Republican Support Over Time", align="center", style = list(color = "black", fontWeight = "bold", fontSize='13px', fontFamily="AppleGothic")) %>% hc_tooltip(shared=TRUE)

#slightly republican->moderately republican
hcsreptomrep2<- hchart(sreptomrep2, "spline", hcaes(x=year_standardized, y=avg_republican_support), color="red", name="Republican Support %")
hcsreptomrep2<- hcsreptomrep2 %>% hc_add_series(sreptomrep2, type="spline", hcaes(x= year_standardized, y=avg_democrat_support), color="blue", name="Democrat Support %") %>% 
  hc_yAxis(title = list(text = "Percentage Support by Party"), labels = list(format = "{value}%"), gridLineWidth=0) %>% 
  hc_xAxis(title = list(text = "# of Election Cycles Per County Since 1964 (Starting from First Participation)")) %>% 
  hc_title(text = "Vote Share of Counties with Moderate Increases in Republican Support Over Time", align="center", style = list(color = "black", fontWeight = "bold", fontSize='13px', fontFamily="AppleGothic")) %>% hc_tooltip(shared=TRUE)

#slightly democratic->slightly republican/swing
hcsdemtosrep2<- hchart(sdemtosrep2, "spline", hcaes(x=year_standardized, y=avg_republican_support), color="red", name="Republican Support %")
hcsdemtosrep2<- hcsdemtosrep2 %>% hc_add_series(sdemtosrep2, type="spline", hcaes(x= year_standardized, y=avg_democrat_support), color="blue", name="Democrat Support %") %>% 
  hc_yAxis(title = list(text = "Percentage Support by Party"), labels = list(format = "{value}%"), gridLineWidth=0) %>% 
  hc_xAxis(title = list(text = "# of Election Cycles Per County Since 1964 (Starting from First Participation)")) %>% 
  hc_title(text = "Vote Share of Counties with Slight Increases in Support for Both Republicans and Democrats Over Time", align="center", style = list(color = "black", fontWeight = "bold", fontSize='13px', fontFamily="AppleGothic")) %>% hc_tooltip(shared=TRUE)

#slightly democratic->very democratic
hcvtosdedm2<- hchart(vtosdedm2, "spline", hcaes(x=year_standardized, y=avg_republican_support), color="red", name="Republican Support %")
hcvtosdedm2<- hcvtosdedm2 %>% hc_add_series(vtosdedm2, type="spline", hcaes(x= year_standardized, y=avg_democrat_support), color="blue", name="Democrat Support %") %>% 
  hc_yAxis(title = list(text = "Percentage Support by Party"), labels = list(format = "{value}%"), gridLineWidth=0) %>% 
  hc_xAxis(title = list(text = "# of Election Cycles Per County Since 1964 (Starting from First Participation)")) %>% 
  hc_title(text = "Vote Share of Counties with Moderate to High Increases in Democrat Support Over Time", align="center", style = list(color = "black", fontWeight = "bold", fontSize='13px', fontFamily="AppleGothic")) %>% hc_tooltip(shared=TRUE)



##Plot where the categories are in highcharter
library(sf)
library(ggplot2)
library(tidyverse)
library(highcharter)
library(jsonlite)
library(geojsonio)
load("data/usdata2.RData")
#needs to be in a json format for highcharter
mapjson<- geojson_json(us_data2)
Myhcmap <- jsonlite::fromJSON(mapjson, simplifyVector = FALSE)

#create a tooltip table
x<-c("Average Support Across Election Years: ", "State:", "County Name:")
y<- c("{point.value:,.2f}%", "{point.properties.STATE}", "{point.properties.NAME}") #round to 2decimal points
tt<- tooltip_table(x,y)

#View(estimates3)

#Summarize the vote share per county for each category: 
#moderately->very republican
veryrepublican <- estimates3 %>% 
  mutate(GEO_ID = if_else(Category == "Moderately Republican->Very Republican", GEO_ID, NA_character_)) %>% 
  group_by(GEO_ID, county_name, STATE, Category) %>% summarize(sum_republican_support=sum(republican_raw_votes, na.rm=TRUE), 
                                                               sum_democrat_support=sum(democratic_raw_votes, na.rm=TRUE), totalvotes= sum(pres_raw_county_vote_totals_two_party), .groups = "drop") %>% 
  mutate(avg_republican_support= sum_republican_support/totalvotes * 100, avg_democrat_support=sum_democrat_support/totalvotes * 100) %>% mutate(avg_republican_support= round(avg_republican_support, 2),
                                                                                                                                                 avg_democrat_support= round(avg_democrat_support, 2))



#slightly republican->moderately republican
moderatelyrepublican <- estimates3 %>% 
  mutate(GEO_ID = if_else(Category == "Slightly Republican->Moderately Republican", GEO_ID, NA_character_)) %>% 
  group_by(GEO_ID, county_name, STATE, Category) %>% summarize(sum_republican_support=sum(republican_raw_votes, na.rm=TRUE), 
                                                               sum_democrat_support=sum(democratic_raw_votes, na.rm=TRUE), totalvotes= sum(pres_raw_county_vote_totals_two_party), .groups = "drop") %>% 
  mutate(avg_republican_support= sum_republican_support/totalvotes * 100, avg_democrat_support=sum_democrat_support/totalvotes * 100) %>% mutate(avg_republican_support= round(avg_republican_support, 2),
                                                                                                                                                 avg_democrat_support= round(avg_democrat_support, 2))


#slightly democratic->slightly republican/swing
swing <- estimates3 %>% 
  mutate(GEO_ID = if_else(Category == "Slightly Democratic->Slightly Republican", GEO_ID, NA_character_)) %>% 
  group_by(GEO_ID, county_name, STATE, Category) %>% summarize(sum_republican_support=sum(republican_raw_votes, na.rm=TRUE), 
                                                               sum_democrat_support=sum(democratic_raw_votes, na.rm=TRUE), totalvotes= sum(pres_raw_county_vote_totals_two_party), .groups = "drop") %>% 
  mutate(avg_republican_support= sum_republican_support/totalvotes * 100, avg_democrat_support=sum_democrat_support/totalvotes * 100) %>% mutate(avg_republican_support= round(avg_republican_support, 2),
                                                                                                                                                 avg_democrat_support= round(avg_democrat_support, 2)) %>% mutate(winningpercentage = if_else(avg_republican_support > 50, avg_republican_support, 
                                                                                                                                                                                                                                              if_else(avg_democrat_support > 50, -avg_democrat_support, NA_real_)))

#slightly democratic->very democratic
democrat <- estimates3 %>% 
  mutate(GEO_ID = if_else(Category == "Very Democratic->Slightly Democratic", GEO_ID, NA_character_)) %>% 
  group_by(GEO_ID, county_name, STATE, Category) %>% summarize(sum_republican_support=sum(republican_raw_votes, na.rm=TRUE), 
                                                               sum_democrat_support=sum(democratic_raw_votes, na.rm=TRUE), totalvotes= sum(pres_raw_county_vote_totals_two_party), .groups = "drop") %>% 
  mutate(avg_republican_support= sum_republican_support/totalvotes * 100, avg_democrat_support=sum_democrat_support/totalvotes * 100) %>% mutate(avg_republican_support= round(avg_republican_support, 2),
                                                                                                                                                 avg_democrat_support= round(avg_democrat_support, 2))

#very republican plot
veryrepublicanhc<- highchart(type = "map") %>%
  hc_add_series_map(df= veryrepublican, map= Myhcmap, name= "Share of Vote Across Election Years", value="avg_republican_support", joinBy= "GEO_ID", showInLegend = T, borderColor="gray", borderWidth=0.03, color="#a70000", nullColor="gray") %>% hc_colorAxis(
    min = 0,
    max = 100,
    stops = color_stops(5, c("#ffbaba", "#ff7b7b", "#ff5252", "#ff0000", "#a70000")) # Shades of red
  ) %>% hc_tooltip(pointFormat=tt, useHTML=TRUE, style = list(fontSize = "10px")) %>% hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text= "Counties with the Highest Increases in Republican Support Over Time", style = list(color = "black", fontWeight = "bold", fontFamily="AppleGothic")) %>%
  hc_subtitle(text="Based on Support Across Elections", style=list(color="black", fontFamily="Helvetica")) %>% 
  hc_legend(enabled=TRUE, y=-50, align='center') %>% hc_chart(marginBottom=100) 


#slightly republican->moderately republican
slightlytomoderaterepublicanhc<- highchart(type = "map") %>%
  hc_add_series_map(df= moderatelyrepublican, map= Myhcmap, name= "Share of Vote Across Election Years", value="avg_republican_support", joinBy= "GEO_ID", showInLegend = T, borderColor="gray", borderWidth=0.03, color="#a70000", nullColor="gray") %>% hc_colorAxis(
    min = 0,
    max = 100,
    stops = color_stops(5, c("#ffbaba", "#ff7b7b", "#ff5252", "#ff0000", "#a70000")) # Shades of red
  ) %>% hc_tooltip(pointFormat=tt, useHTML=TRUE, style = list(fontSize = "10px")) %>% hc_mapNavigation(enabled = TRUE) %>% hc_title(text= "Counties with Moderate Increases in Republican Support Over Time", style = list(color = "black", fontWeight = "bold", fontFamily="AppleGothic")) %>% hc_subtitle(text="Based on Support Across Elections", style=list(color="black", fontFamily="Helvetica")) %>%  hc_legend(enabled=TRUE, y=-50, align='center') %>% hc_chart(marginBottom=100)


#slightly democratic->slightly republican
x2<-c("Average Support Across Election Years: ", "State:", "County Name:")
y2<- c("{point.value:,.2f}%", "{point.properties.STATE}", "{point.properties.NAME}") #round to 2decimal points
tt2<- tooltip_table(x,y)

#create custom html label 
svg_content2<- 
  "
  <div style= 'font-family: AppleGothic; font-size: 12px; font-style:italic; font-weight:bold; transform:translateY(-15px);'>
    <svg width='210' height='120' >
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



swinghc<- highchart(type = "map") %>%
  hc_add_series_map(df= swing, map= Myhcmap, name= "Share of Vote Across Election Years", value="winningpercentage", joinBy= "GEO_ID", showInLegend = T, borderColor="gray", borderWidth=0.03, nullColor="gray") %>% hc_colorAxis(
    min = -100,
    max = 100,
    stops = color_stops(10, c("#0200b9", "#1000ff", "#0066cc", "#3a80ec","#cce0ff","#cce0ff","#ffbaba", "#ff7b7b", "#ff5252", "#ff0000", "#a70000"))
  ) %>% hc_tooltip(pointFormat=tt, useHTML=TRUE, formatter = JS("
    function() {
        var point = this.point;

        // Create the tooltip string with the absolute value of the winning percentage
        var tooltip = this.series.tooltipOptions.pointFormat
            .replace('{point.value:,.2f}', Math.abs(point.value).toFixed(2)); // Use absolute value

        // Capitalize the names for better readability
        tooltip = tooltip
            .replace('{point.properties.NAME}', point.properties.NAME)
            .replace('{point.properties.STATE}', point.properties ? point.properties.STATE : 'N/A'); // Capitalize STATE

        return tooltip;
    }
")) %>% hc_mapNavigation(enabled = TRUE) %>% hc_title(text= "Counties with Slight Increases in Support for Both Republicans and Democrats Over Time", style = list(color = "black", fontWeight = "bold", fontFamily="AppleGothic")) %>% hc_subtitle(text="Based on Support Across Elections", style=list(color="black", fontFamily="Helvetica")) %>%  hc_legend(enabled=FALSE) %>% hc_chart(marginBottom=100) %>% hc_caption(text=svg_content2, useHTML=TRUE)


#slightly democratic->very democratic
#very democrat 
democrathc<- highchart(type = "map") %>%
  hc_add_series_map(df= democrat, map= Myhcmap, name= "Share of Vote Across Election Years", value="avg_democrat_support", joinBy= "GEO_ID", showInLegend = T, borderColor="gray", borderWidth=0.03, color="#0200b9", nullColor="gray") %>% hc_colorAxis(
    min = 0,
    max = 100,
    stops = color_stops(5, c("#cce0ff", "#3a80ec", "#0066cc","#1000ff", "#0200b9")) # Shades of red
  ) %>% hc_tooltip(pointFormat=tt, useHTML=TRUE, style = list(fontSize = "10px")) %>% hc_mapNavigation(enabled = TRUE) %>% hc_title(text= "Counties with Moderate to High Increases in Democrat Support Over Time", style = list(color = "black", fontWeight = "bold", fontFamily="AppleGothic")) %>% hc_subtitle(text="Based on Support Across Elections", style=list(color="black", fontFamily="Helvetica")) %>%  hc_legend(enabled=TRUE, y=-50, align='center') %>% hc_chart(marginBottom=100)


