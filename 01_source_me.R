#libraries-------------------------------
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(conflicted)
#functions-------------------
make_plot <- function(tbbl, group_var, var1, var2){
  tbbl|>
    ggplot(aes({{  var1  }},{{  var2  }}))+
    geom_point()+
    labs(title=paste0({{  group_var  }},": ", substitute(var1), " vs. ", substitute(var2)))
}

iris <- read_excel(here("data","iris.xlsx"))|> #here allows you to use relative file paths so project is portable
  clean_names()|>
  group_by(species)|>
  nest()|>
  mutate(width_plot=map2(data, species, make_plot, petal_width, sepal_width),
         length_plot=map2(data, species, make_plot, petal_length, sepal_length),
         sepal_plot=map2(data, species, make_plot, sepal_width, sepal_length),
         petal_plot=map2(data, species, make_plot, petal_width, petal_length)
         )|>
  select(-data)|>
  pivot_longer(cols=-species)

write_rds(iris, here("out","iris.rds"))



