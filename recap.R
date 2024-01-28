setwd("C:/Users/ADMIN/Desktop/projo/ineos")

tool_exec = function(in_params, out_params) {
  
 
  ##LOAD UP REQUIRED PACKAGES
  arc.progress_label('Loading required R packages...')
  arc.progress_pos(25)
  pkgs = c('dplyr')  #Give list of all required packages
  load_pkgs(pkgs)
  
  
  terrorDB <- arc.open(source_data)
  
  # Inspect the terrorDB data set:
  class(terrorDB)
  terrorDB
  View(terrorDB)
  
  terrorDB_df = arc.select(terrorDB, fields = '*')
  View(terrorDB_df)

  
  # terror_fields = c('Recruitment', 'Country')
  # terrorDB_df = arc.select(terrorDB, fields = terror_fields)
  # View(terrorDB_df)
  
  # You can also create an sp object from the ArcGIS data frame using the
  # arc.data2sp() function.
 
  ### GROUP BY MEAN
  summarized_df = terrorDB_df %>%
    group_by(Country) %>%
    summarize_each(c('mean', 'sum'), Recruitment) %>%
    right_join(terrorDB_df) %>%
    select(mean, sum)
  
  summarized_df
  # Write summary statistics to table
  if (!is.null(stats_table) && stats_table != 'NA') {
    arc.write(stats_table, summarized_df)
  }
  
  terrorDB_df$mean_terror = summarized_df$mean
  terrorDB_df$total_terror = summarized_df$sum
  
  View(terrorDB_df)
  
  # output_fc = 'C:/Users/ADMIN/Desktop/TerroristDB.gdb/TerrorStats_grouped'
  # arc.write(output_fc, terrorDB_df)
}



# Install and load all packages provided from a character vector
load_pkgs = function(pkgs) {
  new_pkgs = pkgs[!(pkgs %in% installed.packages()[ , 'Package'])]
  if (length(new_pkgs) > 0) install.packages(new_pkgs)
  invisible(lapply(pkgs, function(x)
    suppressMessages(library(x, character.only = TRUE)))
  )
}



# Test tool in standalone R
library(arcgisbinding)
arc.check_product()
source_data = 'C:/Users/ADMIN/Desktop/TerroristDB.gdb/YesTerrorEvent'
response_var = 'Terror_Event'
P1 = 'Recruitment'
P2 = 'Planning'
P3 = 'Preparatory'


stats_table = 'C:/Users/ADMIN/Desktop/TerroristDB.gdb/summary_stats'
anova_table = 'C:/Users/ADMIN/Desktop/TerroristDB.gdb/anova_results'
