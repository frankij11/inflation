library(tidyverse)

#load jic
jic<-NULL
try({
  jic <- read.csv('../data/inflation.csv')
  })
if(is.null(jic) ){jic <- read.csv('data/inflation.csv')}

#clean
jic <- jic %>% gather(key="Year", value="Value",-(Version:Indice)) %>%
	pivot_wider(names_from=Type, values_from=Value)
jic$Year <- substring(jic$Year, 2) %>% as.numeric()

#get tags
jic_ver <- jic %>% select(Version) %>% unique() %>% c()
jic_indices <- jic %>% select(Indice) %>% unique() %>% as.list()
jic_service <- list(Navy = "n", USMC="m", DOD = "d")
jic_tags <- jic %>% select(tags) %>% unique()



change_BY <- function(BY=2020, inflation_table=jic){

  jic.by <- inflation_table %>% filter(Year==BY) %>% mutate(Raw.BY = Raw) %>% select(-Raw, -Weighted, -Year)
  df <- inflation_table %>% left_join(jic.by) %>% 
	mutate(Raw = Raw/Raw.BY,
		Weighted = 1 / Raw.BY * Weighted) %>%
	select(-Raw.BY)
 
  return(df)	
}

inflation <- function (Index, FromYR, ToYR, Cost,from_type ="BY", to_type = "BY",inflation_table = NULL){
  if(is.null(inflation_table)){inflation_table <- jic }
  
  #'nToYR = ToYR
  #If(ToYR > max(jic$Year)){ToYR <- max(jic$Year)}
  df = data.frame(Index = Index,FromYR = FromYR, ToYR =ToYR, Cost=Cost)
  
  df_from = df %>% left_join(inflation_table, by=c("Index"="Indice", "FromYR" = "Year"))
  df_to = df %>% left_join(inflation_table, by=c("Index"="Indice", "ToYR" = "Year"))

	
  div_by <- switch(from_type, BY = df_from$Raw, TY = df_from$Weighted)
  mult_by <- switch(to_type, BY = df_to$Raw, TY = df_to$Weighted)
  
  result = (df$Cost / div_by) * mult_by
  #result = result[[1]]
  
  return(result)
  
}

BYtoBY <- function(Index, FromYR, ToYR, Cost, inflation_table = NULL){
  return(inflation(Index,FromYR,ToYR, Cost, "BY", "BY", inflation_table)) 
}

BYtoTY <- function(Index, FromYR, ToYR, Cost, inflation_table = NULL){
  return(inflation(Index,FromYR,ToYR, Cost, "BY", "TY", inflation_table)) 
}
TYtoBY <- function(Index, FromYR, ToYR, Cost, inflation_table = NULL){
  return(inflation(Index,FromYR,ToYR, Cost, "TY", "BY", inflation_table)) 
}
TYtoTY <- function(Index, FromYR, ToYR, Cost, inflation_table = NULL){
  return(inflation(Index,FromYR,ToYR, Cost, "TY", "TY", inflation_table)) 
}
