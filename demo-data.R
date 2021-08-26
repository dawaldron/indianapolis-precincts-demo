setwd('C:/Users/dwald/Documents/GitHub/indianapolis-precincts-demo')
library(data.table)
library(magrittr)
library(foreign)

# summarize building countby precinct, block group
dt_precXbg <- fread('precinct_bg.csv') %>%
  .[, .(buildings = .N), .(precinct = PRECINCT, blockgrp = as.character(GEOID))] %>%
  .[, pctOfPrecinct := buildings / sum(buildings), precinct] %>%
  .[, pctOfBlockgrp := buildings / sum(buildings), blockgrp]

# precinct area from shapefile
dt_prec <- read.dbf('Precincts/Precincts.dbf', as.is = TRUE) %>%
  data.table() %>%
  .[, .(precinct = PRECINCT, sqmi = SHAPEAREA / 2589988.1103)]

# education data from ACS
dt_educ <- fread('ACS_17_5YR_B15003.csv') %>%
  .[,
    .(blockgrp = as.character(GEO.id2),
      pctColl = (HD01_VD21 + HD01_VD22 + HD01_VD23 + HD01_VD24 + HD01_VD25)/HD01_VD01)]

# race data from ACS
dt_race <- fread('ACS_17_5YR_B03002.csv') %>%
  .[,
    .(blockgrp = as.character(GEO.id2),
      pop = HD01_VD01,
      pctWNH = HD01_VD03/HD01_VD01)]

# combine data and calculate weighted summary by precinct
dt_demo <- dt_educ[dt_race, on = 'blockgrp'] %>%
  dt_precXbg[, .(precinct, blockgrp, pctOfPrecinct)][., on = 'blockgrp'] %>%
  .[,
    .('Population' = sum(pop),
      '% with college degree' = sum(pctColl * pctOfPrecinct),
      '% white, non-Hispanic' = sum(pctWNH * pctOfPrecinct)),
    .(precinct)] %>%
  dt_prec[., on = 'precinct'] %>%
  .[, `:=`('Population density' = Population / sqmi,
           Population = NULL,
           sqmi = NULL)]


fwrite(dt_demo, 'demo.csv')

# long version of data
dt_demo2 <- dt_demo %>%
  melt(id.var = 'precinct',
       variable.name = 'measure',
       variable.factor = FALSE)

fwrite(dt_demo2, 'demo-long.csv')
