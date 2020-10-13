# Stats 506, F20
# Activity, week 6
#
#
# Author: Chuwen Li (chuwenli@umich.edu)
# Updated: October 13, 2020
# 79: -------------------------------------------------------------------------

# library: ---------------------------------------------------------------
library(tidyverse)

# data: ---------------------------------------------------------------
if ( TRUE ) {
  # run for interactive testing but not when sourced
  path = './'
  file1 = sprintf('%snhanes_demo.csv', path)
  file2 = sprintf('%snhanes_ohxden.csv', path)
  demo = read.table(file1, sep = ',', header = TRUE)
  ohxden = read.table(file2, sep = ',', header = TRUE)
}

# part 1: modify data---------------------------------------------------------
ohxden_1 = ohxden %>% 
  select(SEQN, OHDDESTS)
demo = merge(demo, ohxden_1, by = 'SEQN')

clean_data = demo %>%
  select(id = SEQN, 
         gender = RIAGENDR,
         age = RIDAGEYR, 
         college = DMDEDUC2, 
         exam_status = RIDSTATR, 
         ohx_status = OHDDESTS) %>% 
  mutate(under_20 = ifelse(age < 20, 'Yes', 'No'),
         college = ifelse(college == 4 | college == 5, 
                          'some college/college graduate', 
                          'No college/<20'), 
         ohx = ifelse(exam_status == 2 & ohx_status == 1, 
                      'complete', 'missing' ),
         gender = ifelse(gender == 1, 'Male', 'Female')) %>%
  filter(exam_status == 2)
  

# part 2: construct table-----------------------------------------------------
table = clean_data %>%
  mutate(college = ifelse(is.na(college), 
                          'No college/<20', college)) %>% 
  count(under_20, gender, college, ohx) %>% 
  spread(key = ohx, value = n) %>% 
  mutate(c_pct = complete / sum(complete + missing),
         m_pct = 1-c_pct,
         complete = sprintf('%4.0f (%4.2f %%)', complete, c_pct*100),
         missing = sprintf('%4.0f (%4.2f %%)', missing, m_pct*100)) %>% 
  select(-c(c_pct, m_pct))

top_header = c(1, 1, 1, 2)
names(top_header) = c(' ', ' ', ' ', 'Tooth Wear')

table = table %>% 
  knitr::kable(format = 'html') %>%
  kableExtra::kable_styling('striped', full_width = TRUE) %>% 
  kableExtra::add_header_above(header = top_header) %>%
  cat(., file = "nhanes_table.html")

# 79: -------------------------------------------------------------------------


