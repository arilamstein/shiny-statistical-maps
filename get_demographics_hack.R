library(acs)

get_acs_data_2 = function(tableId, map, endyear, span = 5, column_idx = 1)
{
  acs.data = acs.fetch(geography = choroplethr:::make_geo(map), table.number = tableId, 
                       col.names = "pretty", endyear = endyear, span = span, key = "f8b2a6df01479981aef39577b3c4466f5a4c8274")
  title = acs.data@acs.colnames[column_idx]
  df = choroplethr:::convert_acs_obj_to_df(map, acs.data, column_idx)
  list(df = df, title = title)
}

get_state_demographics_2 = function(endyear, span)
{  
  state_geo = acs::geo.make(state = "*")
  race.data = acs::acs.fetch(geography    = state_geo, 
                             table.number = "B03002", 
                             col.names    = "pretty", 
                             endyear      = endyear, 
                             span         = span,
                             key          = "f8b2a6df01479981aef39577b3c4466f5a4c8274")
  
  # convert to a data.frame 
  df_race = data.frame(region                   = as.character(tolower(acs::geography(race.data)$NAME)),  
                       total_population         = as.numeric(acs::estimate(race.data[,1])),
                       white_alone_not_hispanic = as.numeric(acs::estimate(race.data[,3])),
                       black_alone_not_hispanic = as.numeric(acs::estimate(race.data[,4])),
                       asian_alone_not_hispanic = as.numeric(acs::estimate(race.data[,6])),
                       hispanic_all_races       = as.numeric(acs::estimate(race.data[,12])))
  
  df_race$region = as.character(df_race$region) # no idea why, but it's a factor before this line
  
  df_race$percent_white    = round(df_race$white_alone_not_hispanic / df_race$total_population * 100)
  df_race$percent_black    = round(df_race$black_alone_not_hispanic / df_race$total_population * 100)
  df_race$percent_asian    = round(df_race$asian_alone_not_hispanic / df_race$total_population * 100)
  df_race$percent_hispanic = round(df_race$hispanic_all_races       / df_race$total_population * 100)
  
  df_race = df_race[, c("region", "total_population", "percent_white", "percent_black", "percent_asian", "percent_hispanic")]
  
  # per capita income 
  df_income = get_acs_data_2("B19301", "state", endyear=endyear, span=span)[[1]]  
  colnames(df_income)[[2]] = "per_capita_income"
  
  # median rent
  df_rent = get_acs_data_2("B25058", "state", endyear=endyear, span=span)[[1]]  
  colnames(df_rent)[[2]] = "median_rent"
  
  # median age
  df_age = get_acs_data_2("B01002", "state", endyear=endyear, span=span, column_idx=1)[[1]]  
  colnames(df_age)[[2]] = "median_age"
  
  df_demographics = merge(df_race        , df_income, all.x=TRUE)
  df_demographics = merge(df_demographics, df_rent  , all.x=TRUE)  
  df_demographics = merge(df_demographics, df_age   , all.x=TRUE)
  
  # remove the regions (such as zips in Puerto Rico) that are not on my map.
  data(state.regions, package="choroplethrMaps", envir=environment())
  df_demographics = df_demographics[df_demographics$region %in% state.regions$region, ]
  
  df_demographics
}

get_county_demographics_2 = function(endyear=2013, span=5)
{  
  county_geo = geo.make(state = "*", county = "*")
  race.data = acs::acs.fetch(geography    = county_geo, 
                             table.number = "B03002", 
                             col.names    = "pretty", 
                             endyear      = endyear, 
                             span         = span,
                             key          = "f8b2a6df01479981aef39577b3c4466f5a4c8274")
  
  race.data@geography$fips = paste(as.character(race.data@geography$state), 
                                   race.data@geography$county, 
                                   sep = "")
  
  # choroplethr requires county fips to be numeric (i.e. no leading 0)
  race.data@geography$fips = as.numeric(race.data@geography$fips)
  
  # convert to a data.frame 
  df_race = data.frame(region                   = race.data@geography$fips,
                       total_population         = as.numeric(acs::estimate(race.data[,1])),
                       white_alone_not_hispanic = as.numeric(acs::estimate(race.data[,3])),
                       black_alone_not_hispanic = as.numeric(acs::estimate(race.data[,4])),
                       asian_alone_not_hispanic = as.numeric(acs::estimate(race.data[,6])),
                       hispanic_all_races       = as.numeric(acs::estimate(race.data[,12])))
  
  df_race$percent_white    = round(df_race$white_alone_not_hispanic / df_race$total_population * 100)
  df_race$percent_black    = round(df_race$black_alone_not_hispanic / df_race$total_population * 100)
  df_race$percent_asian    = round(df_race$asian_alone_not_hispanic / df_race$total_population * 100)
  df_race$percent_hispanic = round(df_race$hispanic_all_races       / df_race$total_population * 100)
  
  df_race = df_race[, c("region", "total_population", "percent_white", "percent_black", "percent_asian", "percent_hispanic")]
  
  # per capita income 
  df_income = get_acs_data_2("B19301", "county", endyear=endyear, span=span)[[1]]  
  colnames(df_income)[[2]] = "per_capita_income"
  
  # median rent
  df_rent = get_acs_data_2("B25058", "county", endyear=endyear, span=span)[[1]]  
  colnames(df_rent)[[2]] = "median_rent"
  
  # median age
  df_age = get_acs_data_2("B01002", "county", endyear=endyear, span=span, column_idx=1)[[1]]  
  colnames(df_age)[[2]] = "median_age"
  
  df_demographics = merge(df_race        , df_income, all.x=TRUE)
  df_demographics = merge(df_demographics, df_rent  , all.x=TRUE)  
  df_demographics = merge(df_demographics, df_age   , all.x=TRUE)
  
  # remove the regions (such as counties in Puerto Rico) that are not on my map.
  data(county.regions, package="choroplethrMaps", envir=environment())
  df_demographics = df_demographics[df_demographics$region %in% county.regions$region, ]
  
  df_demographics
}