### Author: Malte Heckelen
### This still contains exploratory stuff for other visualizations, will be cleaned up and annotated later

# Imports

library(tidyverse)
library(gridExtra)
library(fmsb)
library(geonames)
library(countrycode)
library(grid)
library(ggplotify)

# Get Data 

tuesdata_df <- readr::read_csv('plastic.csv')
tuesdata_df['country'] <- sapply(tuesdata_df['country'], function(x) ifelse(x == "United Kingdom of Great Britain & Northern Ireland", 'United Kingdom', x))
population_sizes <- readr::read_csv('WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.csv') # Source:
centroids <- readr::read_csv('country_centroids_az8.csv') # Source: https://worldmap.harvard.edu/data/geonode:country_centroids_az8
tuesdata_df <- tuesdata_df %>%
  mutate(parent_company = ifelse(parent_company == 'Nestle', 'Nestlé', parent_company))

# Viz 1: Radar Charts

# What kinds of plastic waste are companies producing?  

old_plastics <- c('hdpe', 'pvc', 'ps', 'pp', 'pet', 'o', 'ldpe')
new_plastics <- c('HDPE', 'PVC', 'PS', 'PP', 'PET', 'Other', 'LDPE')
colnames(tuesdata_df) <- plyr::mapvalues(colnames(tuesdata_df), old_plastics, new_plastics)

get_radar_data <- function(company_name=NA, country_name=NA) {
  if (!is.na(company_name)) {
    filtered <- tuesdata_df %>%
      filter(parent_company == company_name)
  }
  if (!is.na(country_name)) {
    filtered <- filtered %>%
      filter(country == country_name)
  }
  sums <- summarise_all(filtered %>% 
                          replace_na(list('HDPE'=0, 'PVC'=0, 'PS'=0, 'PP'=0, 'PET'=0, 'Other'=0, 'LDPE'=0)) %>%
                          select('HDPE', 'PVC', 'PS', 'PP', 'PET', 'Other', 'LDPE'), sum)
  maxs <- rep(sum(sums), 7)
  mins <- rep(0, 7)
  return(rbind(maxs, mins, sums))
}

make_radar_chart <- function(dataset, name='Total Count') {
  pp <- par(cex=.3,family='Courier')
  radarchart(dataset,
             title=paste(c(name, ': ', dataset[1,1], ' items'), collapse=''),
             plwd = .001,
             pty = 32,
             #cglty = 1,
             cglwd = .12,
             pfcol = '#b15dba80',
             pcol = '#b15dba80')
  pp
}

company_name <- 'Nestlé'
tuesdata_df <- tuesdata_df %>%
  mutate(country = ifelse(country == 'United States of America', 'U.S.', country)) %>%
  mutate(country = ifelse(country == 'United Republic of Tanzania', 'Tanzania', country)) %>%
  mutate(country = ifelse(country == 'China, Hong Kong SAR', 'Hong Kong', country)) %>%
  mutate(parent_company = ifelse(parent_company == 'The Coca-Cola Company', 'Coca Cola', parent_company))
countries <- (tuesdata_df %>%
                drop_na(grand_total) %>%
                select(country, grand_total, parent_company) %>%
                mutate(total_waste = sum(grand_total)) %>%
                filter(parent_company != 'null' & parent_company != 'NULL' & parent_company != 'Unbranded' & parent_company != 'Grand Total' & country != 'EMPTY') %>%
                #mutate(parent_company = ifelse(nchar(parent_company) >= 25, paste(substr(parent_company, start=1, stop=25), '...', sep=''), parent_company)) %>%
                filter(parent_company == company_name) %>% 
                group_by(country) %>%
                summarise(grand_total = sum(grand_total)) %>%
                arrange(desc(grand_total)) %>% 
                select(country))$country

plt_lay <- rbind(
  c(NA,NA,1,1,1,1,NA,NA),
  c(NA,NA,1,1,1,NA,NA,NA),
  c(NA,NA,1,1,1,NA,NA,NA),
  c(NA,NA,1,1,1,NA,NA,NA),
  c(NA,NA,1,1,1,NA,NA,NA),
  c(NA,NA,1,1,1,NA,NA,NA),
  c(NA,2,2,3,3,4,4,NA),
  c(NA,2,NA,3,NA,4,NA,NA),
  c(NA,2,NA,3,NA,4,NA,NA),
  c(NA,2,NA,3,NA,4,NA,NA),
  c(NA,5,5,6,6,7,7,NA),
  c(NA,5,NA,6,NA,7,NA,NA),
  c(NA,5,NA,6,NA,7,NA,NA),
  c(NA,5,NA,6,NA,7,NA,NA),
  c(NA,8,8,9,9,10,10,NA),
  c(NA,8,NA,9,NA,10,NA,NA),
  c(NA,8,NA,9,NA,10,NA,NA),
  c(NA,8,NA,9,NA,10,NA,NA),
  c(NA,11,11,12,12,13,13,NA),
  c(NA,11,NA,12,NA,13,NA,NA),
  c(NA,11,NA,12,NA,13,NA,NA),
  c(NA,11,NA,12,NA,13,NA,NA))
full_lay <- rbind(
  c(1,1,NA,NA,NA,NA,NA,NA),
  c(2,2,NA,NA,NA,NA,NA,NA),
  c(NA,NA,3,3,3,NA,NA,NA)
)

plot_grid <- grid.arrange(grobs=list(as.grob(~make_radar_chart(get_radar_data(company_name))),
                                     as.grob(~make_radar_chart(get_radar_data(company_name, countries[1]),countries[1])),
                                     as.grob(~make_radar_chart(get_radar_data(company_name, countries[2]),countries[2])),
                                     as.grob(~make_radar_chart(get_radar_data(company_name, countries[3]),countries[3])),
                                     as.grob(~make_radar_chart(get_radar_data(company_name, countries[4]),countries[4])),
                                     as.grob(~make_radar_chart(get_radar_data(company_name, countries[5]),countries[5])),
                                     as.grob(~make_radar_chart(get_radar_data(company_name, countries[6]),countries[6])),
                                     as.grob(~make_radar_chart(get_radar_data(company_name, countries[7]),countries[7])),
                                     as.grob(~make_radar_chart(get_radar_data(company_name, countries[8]),countries[8])),
                                     as.grob(~make_radar_chart(get_radar_data(company_name, countries[9]),countries[9])),
                                     as.grob(~make_radar_chart(get_radar_data(company_name, countries[10]),countries[10])),
                                     as.grob(~make_radar_chart(get_radar_data(company_name, countries[11]),countries[11])),
                                     as.grob(~make_radar_chart(get_radar_data(company_name, countries[12]),countries[12]))
),
layout_matrix=plt_lay)

pdf('plot_nestle.pdf')

grid.arrange(main=textGrob(paste(c('The Plastic Worlds of ', company_name, ', 2019-2020\n'), collapse=''),x=.525,gp=gpar(fontsize=8,fontface='bold',fontfamily='Courier')),
             textGrob('\n\n\nData from the BFFF (Break Free From Plastic) Global Brand Audits,           \nwhere kinds of plastic items were counted for each brand family and country.\n\n\n\n\n\n',x=.52,gp=gpar(fontsize=4,fontfamily='Courier', just='left')),
             plot_grid,
             textGrob('Learn more at breakfreefromplastic.org',x=.85,gp=gpar(fontsize=4,fontfamily='Courier', just='right')),
             vp=viewport(height=.8,width=0.4),
             padding = unit(20, "mm"),
             heights=c(.01,.17,2.5,.02)
)
dev.off()

# Viz 2: World Map

# Setup (for world map viz)

options(geonamesUsername="XXXXXX")

population_sizes <- population_sizes %>%
  filter(Type == 'Country/Area') %>%
  rename('popsize' = '2020',
         'countrycode'='Country code') %>%
  select(Region_subregion_country_area, popsize, countrycode) %>%
  mutate(iso_code = countrycode::countrycode(countrycode, origin='un', destination = 'iso3c'))
# Some values were not matched unambiguously: 158, 830 (Burkina Faso, Estonia)

unique(tuesdata_df[!(tuesdata_df$country %in% population_sizes$Region_subregion_country_area),]$country)

'
[1] "Cote D_ivoire"                   "ECUADOR"                         "EMPTY"                          
[4] "Hong Kong"                       "NIGERIA"                         "Taiwan_ Republic of China (ROC)"
[7] "Tanzania"                        "Vietnam"                         "Korea"    
'

old_names <- c('Cote D_ivoire', 'ECUADOR', 'Hong Kong', 'NIGERIA', 'Korea', "Taiwan_ Republic of China (ROC)", "Vietnam", "Tanzania")
new_names <- c("Côte d'Ivoire", 'Ecuador', 'China, Hong Kong SAR', 'Nigeria', 'Republic of Korea', 'China, Taiwan Province of China', "Viet Nam", "United Republic of Tanzania")

tuesdata_df <- tuesdata_df %>% 
	dplyr::mutate(country = mapply(function(x) { plyr::mapvalues(x, old_names, new_names, warn_missing=FALSE) }, x=country)) %>%
  left_join(population_sizes %>% select(Region_subregion_country_area, countrycode, iso_code), by=c('country'="Region_subregion_country_area")) %>%
  left_join(centroids %>% select(adm0_a3_is, Longitude, Latitude), by=c('iso_code'='adm0_a3_is'))

counting_events <- tuesdata_df %>%
  filter(year == 2020) %>%
  select(num_events, Longitude, Latitude) %>%
  distinct()
counting_events['num_events'] <- as.character(counting_events$num_events)

# problem: data not normalized w respect to country size

# Which countries have most plastic waste?
library(rworldmap)

countries_2020 <- tuesdata_df %>%
  filter(year == 2020) %>%
  drop_na(grand_total) %>%
  filter(parent_company != 'Grand Total' & country != 'EMPTY') %>%
  group_by(country) %>%
  mutate(grand_total = sum(grand_total)) %>%
  select(country, grand_total, countrycode) %>%
  distinct

worldmap <- getMap(resolution = 'coarse')

plot(worldmap, col = "lightgrey", 
     fill = T, border = "white",
     #xlim = c(-180, 180), ylim = c(-90, 90),
     bg = "white",
     asp = 0, wrap=c(-180,180))

plasticmap <- joinCountryData2Map(countries_2020, 
                                  joinCode = "UN",
                                  nameJoinColumn = "countrycode")
colors = colorRampPalette(c("#b8ff91", "#ffe640", "#ff0505"))
# def. map parameters, e.g. def. colors
par(mfrow=c(2,2))
mapParams <- mapCountryData(plasticmap, 
                            mapTitle='',
                            nameColumnToPlot="grand_total",
                            oceanCol = "white",
                            catMethod = c(seq(0, 10000, 1000), seq(11000, 55000, 10000)),
                            missingCountryCol = grey(.95),
                            colourPalette = colors(15),
                            aspect = 1,
                            addLegend = F,
                            borderCol = 'white',
                            lwd=1)

# add legend and display map
do.call( addMapLegend, c( mapParams
                          , legendLabels="all"
                          , legendWidth=0.5
                          , labelFontSize=.7
                          
))

#shadowtext(counting_events$Longitude, counting_events$Latitude, counting_events$num_events, cex=.8, col='white', font=2)
text(counting_events$Longitude, counting_events$Latitude, counting_events$num_events, cex=.5, col=alpha('black', .3), font=2)
tons_per_company_2020
text(-150, 90, '<h3>World Map</h3>')
# Which companies produce most waste overall?

# countries with most plastic waste for given year

countries_2019 <- tuesdata_df %>%
  filter(year == 2020) %>%
  drop_na(grand_total) %>%
  filter(parent_company != 'null' & parent_company != 'NULL' & parent_company != 'Unbranded' & parent_company != 'Grand Total' & country != 'EMPTY') %>%
  mutate(grand_total = grand_total / popsize) %>%
  group_by(country) %>%
  mutate(brand_total = sum(grand_total)) %>%
  slice_max(brand_total, order_by=brand_total, n=10) %>%
  select(country)

companies_2019 <- tuesdata_df %>%
  filter(year == 2019) %>%
  drop_na(grand_total) %>%
  mutate(total_waste = sum(grand_total)) %>%
  filter(parent_company != 'null' & parent_company != 'NULL' & parent_company != 'Unbranded' & parent_company != 'Grand Total' & country != 'EMPTY') %>%
  #mutate(parent_company = ifelse(nchar(parent_company) >= 25, paste(substr(parent_company, start=1, stop=25), '...', sep=''), parent_company)) %>%
  mutate(country = ifelse(country %in% wasters_2019, country, 'other')) %>%
  group_by(parent_company) %>%
  summarise(brand_total = sum(grand_total)) %>%
  slice_max(brand_total, order_by=brand_total, n=15) %>%
  select(parent_company)

countries_2020 <- tuesdata_df %>%
  filter(year == 2020) %>%
  drop_na(grand_total) %>%
  filter(parent_company != 'null' & parent_company != 'NULL' & parent_company != 'Unbranded' & parent_company != 'Grand Total' & country != 'EMPTY') %>%
  group_by(country) %>%
  summarise(brand_total = sum(grand_total)) %>%
  slice_max(brand_total, order_by=brand_total, n=10) %>%
  select(country)

companies_2020 <- tuesdata_df %>%
  filter(year == 2020) %>%
  drop_na(grand_total) %>%
  mutate(total_waste = sum(grand_total)) %>%
  filter(parent_company != 'null' & parent_company != 'NULL' & parent_company != 'Unbranded' & parent_company != 'Grand Total' & country != 'EMPTY') %>%
  #mutate(parent_company = ifelse(nchar(parent_company) >= 25, paste(substr(parent_company, start=1, stop=25), '...', sep=''), parent_company)) %>%
  mutate(country = ifelse(country %in% countries_2020$country, country, 'other')) %>%
  group_by(parent_company) %>%
  summarise(brand_total = sum(grand_total)) %>%
  slice_max(brand_total, order_by=brand_total, n=15) %>%
  select(parent_company)

colourCount = length(unique(countries_2019$country))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

tons_per_company_2019 <- tuesdata_df %>%
  filter(year == 2019) %>%
  drop_na(grand_total) %>%
  select(country, grand_total, parent_company) %>%
  mutate(total_waste = sum(grand_total)) %>%
  filter(parent_company != 'null' & parent_company != 'NULL' & parent_company != 'Unbranded' & parent_company != 'Grand Total' & country != 'EMPTY') %>%
  #mutate(parent_company = ifelse(nchar(parent_company) >= 25, paste(substr(parent_company, start=1, stop=25), '...', sep=''), parent_company)) %>%
  filter(parent_company %in% companies_2019$parent_company) %>%
  mutate(country = ifelse(country %in% countries_2019$country, country, 'other')) %>%
  group_by(parent_company, country) %>%
  summarise(brand_total = sum(grand_total)) %>%
  arrange(brand_total) %>%
  ggplot(aes(x=reorder(parent_company, brand_total), y=brand_total, fill=country)) +
  geom_bar(position='stack', stat='identity') +
  scale_fill_brewer(palette="Paired") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

tons_per_company_2020 <- tuesdata_df %>%
  filter(year == 2020) %>%
  drop_na(grand_total) %>%
  select(country, grand_total, parent_company) %>%
  mutate(total_waste = sum(grand_total)) %>%
  filter(parent_company != 'null' & parent_company != 'NULL' & parent_company != 'Unbranded' & parent_company != 'Grand Total' & country != 'EMPTY') %>%
  #mutate(parent_company = ifelse(nchar(parent_company) >= 25, paste(substr(parent_company, start=1, stop=25), '...', sep=''), parent_company)) %>%
  filter(parent_company %in% companies_2020$parent_company) %>%
  mutate(country = ifelse(country %in% countries_2020$country, country, 'other')) %>%
  group_by(parent_company, country) %>%
  summarise(brand_total = sum(grand_total)) %>%
  arrange(brand_total) %>%
  ggplot(aes(x=reorder(parent_company, brand_total), y=brand_total, fill=country)) +
  geom_bar(position='stack', stat='identity') +
  scale_fill_brewer(palette="Paired") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

grid.arrange(
  tons_per_company_2019,
  tons_per_company_2020, 
  nrow=2
)