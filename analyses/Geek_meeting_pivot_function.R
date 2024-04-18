#savefiles 
# saveRDS(isl_subset, "data/derived-data/data1_pivot_functions.RDS")
# saveRDS(status, "data/derived-data/data2_pivot_functions.RDS")

# load datasets
data1 <- readRDS("data/derived-data/data1_pivot_functions.RDS")
data2 <- readRDS("data/derived-data/data2_pivot_functions.RDS")

# package tidyr, encompassed in tidyverse
library(tidyverse)


###### Pivot_wider ###########

# pivot_wider: transform the content of 2 variables into several columns
# from a dataset with many rows and few columns to many colums and few rows
# previous spread() function in base r 

#### Example 1 ------

# obtain an occurrence matrix
# with islands in rows
# species in columns

head(data1)

occ <- data1 %>%
  mutate(occ = 1)  %>%
  distinct(geo_entity, work_species, occ) %>%
  pivot_wider(
    names_from = "work_species", 
    values_from = "occ", 
    values_fill = 0
  ) %>%
  column_to_rownames("geo_entity")


#### Example 2 ------

# we have the number of exotic and native species per island
ggplot(data2, aes(y = Island_name, x = n, fill = Status))+
  geom_bar(stat = "identity")+
  facet_wrap(vars(Archip), scales = "free")

# what is the proportion of native vs exotic?
# => need to create a column for each status

status_w <- data2 %>% 
  pivot_wider(names_from = Status, 
              values_from = n) %>%
  mutate(naturalized = if_else(is.na(naturalized), 0, naturalized),
         `non-native` = if_else(is.na(`non-native`), 0, `non-native`)) %>%
  mutate(exotic = naturalized + `non-native`) %>%
  mutate(prop_exo = exotic/native)

# now we can see the relationship between the number of native and number of exotic
ggplot(status_w, aes(x=native, y = exotic))+
  geom_point(aes(color = Archip), size = 3)+
  geom_smooth(method = lm)

# look at the proportion for each island because now 
# each line is an island
ggplot(status_w, aes(x=Archip, y = prop_exo))+
  geom_boxplot()+
  geom_point(aes(color = Archip), alpha = .5, size = 3, position = "jitter")+
  geom_hline(yintercept = 1, lty=2)


###### Pivot_longer ###########


# pivot_longer: transform several columns into two variables
# decrease the number of columns and increase the number of rows 
# (my favourite way of storing data)
# previous gather() function


#### Example 1 ------

# I want to add species characteristics to occurrences

names <- colnames(occ)

occ_l <- occ %>%
  rownames_to_column("island") %>%
  pivot_longer(
    cols = !island, # remove the column you don't want to consider
    names_to = "species",
    values_to = "presence"
  )

# now you can join the table by island or by species
# with other characteristics


#### Example 2 ------

colnames(status_w)

status_l1 <- status_w %>%
  pivot_longer(
    cols = native:unknown,
    names_to = "Status",
    values_to = "Number",
    values_drop_na = TRUE
  )

colnames(status_w) <- c(
  "Archip", "Island_name", "status_native", "naturalized", 
  "non-native", "status_unknown" , "status_exotic", "prop_exo" )

# you have column names with a pattern (status_*)
# you want all those columns combined in one variable

status_l2 <- status_w %>%
  pivot_longer(
    cols = starts_with("status"),
    names_to = "Status",
    names_prefix = "st", # try with "st"
    values_to = "Number",
    # values_drop_na = TRUE
  )

# other columns are duplicated, so you don't lose the information


