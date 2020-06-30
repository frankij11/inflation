library(tidyverse)
source("scripts/inflation.R")

#jic <- read.csv('data/inflation.csv')
#jic <- jic %>% gather(key="Year", value="Value",-(Version:Indice))
#jic$Year <- substring(jic$Year, 2) %>% as.numeric()

jic.raw <- jic

jic_ver <- jic %>% select(Version) %>% unique() %>% c()
jic_indices <- jic %>% select(Indice) %>% unique() %>% as.list()
jic_service <- list(Navy = "n", USMC="m", DOD = "d")
jic_tags <- jic %>% select(tags) %>% unique()