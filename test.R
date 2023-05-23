library(tidyverse)
library(here)

clean_table <- function(table) {
  d <- table %>% 
    filter(st == 53 & cnty %in% c('033', '035', '053', '061')) %>% 
    pivot_longer(cols = str_subset(colnames(table), "^T.*"), 
                 names_to = 'header', 
                 values_to = 'value') %>% 
    mutate(table = str_extract(header, "^T\\d*(?=_)"),
           type = str_extract(header, "(?<=_)\\w{3}"),
           sort = str_extract(header, "\\d+$"))
  
  return(d)
}

dfs <- list()

csv <- paste0('Table', 1:5, '.csv')

for(i in 1:length(csv)) {
  t <- read_csv(here('data', '050', csv[i]))
  ct <- clean_table(t)
  dfs[[csv[i]]] <- ct
}

# for(i in csv) {
#   t <- read_csv(here('data', '050', c))
#   ct <- clean_table(t)
#   dfs[[c]] <- ct
# }
