# Stats 506, F20
# Activity, week 6
#
#
# Author: Chuwen Li (chuwenli@umich.edu)
# Updated: October 13, 2020
# 79: -------------------------------------------------------------------------

# library: --------------------------------------------------------------------
library(tidyverse)

# data: -----------------------------------------------------------------------
if ( TRUE ) {
  path = './'
  file1 = sprintf('%snhanes_demo.csv', path)
  file2 = sprintf('%snhanes_ohxden.csv', path)
  demo = read.table(file1, sep = ',', header = TRUE)
  ohxden = read.table(file2, sep = ',', header = TRUE)
}

# part 1: modify data---------------------------------------------------------
demo =  demo %>% 
  left_join(transmute(ohx, SEQN, OHDDESTS), by = 'SEQN')

clean_data = demo %>%
  select(id = SEQN, 
         gender = RIAGENDR,
         age = RIDAGEYR,
         college = DMDEDUC2, 
         exam_status = RIDSTATR, 
         ohx_status = OHDDESTS) %>% 
  mutate(under_20 = ifelse(age < 20, '<20', '20+'),
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
  mutate(c_pct = complete / (complete + missing),
         m_pct = 1 - c_pct,
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

# part 2*: construct table-----------------------------------------------------
reform_table = function(df, df_col){
  # summarize a data frame by given column names
  # Inputs: 
  #  df - a data frame
  #  df_col - a character vector specifying the column variable(s)
  # Outputs: 
  #   A reformatted data frame
  
  st_list = list()

  for (i in 1:length(df_col)){
    st_list[[i]] = df %>% 
      count(df[[df_col[i]]], ohx) %>% 
      spread(key = ohx, value = n) %>% 
      mutate(c_pct = complete / (complete + missing),
             m_pct = 1 - c_pct)
    
    st_list[[i]] = st_list[[i]] %>% 
      mutate(
        # calculate p value
        p_value = chisq.test(as.matrix(st_list[[i]][, -c(1, 4:5)]))$p.value, 
        p_value = ifelse(p_value < 0.001, 'p < 0.001', 
                         sprintf('p = %4.3f', p_value)), 
        p_value = ifelse(row_number() == 1, p_value, ''),
        # change number format
        complete = sprintf('%s (%4.1f %%)', 
                           format(complete, big.mark=','), 
                           c_pct*100),
        missing = sprintf('%s (%4.1f %%)', 
                          format(missing, big.mark=','), 
                          m_pct*100),
        var = df_col[i]) %>%
      rename(level = colnames(st_list[[i]])[1] ) %>% 
      select(var, level, complete, missing, p_value)
  }
  new_df = bind_rows(st_list)
  
  return(new_df)
}

# clean NAs
clean_data = clean_data %>%
  mutate(college = ifelse(is.na(college), 'No college/<20', college),
         ohx = ifelse(is.na(ohx), 'missing', ohx))

# construct table
table = reform_table(clean_data[, -6], c('under_20', 'gender', 'college'))

top_header = c(1, 1, 2, 1)
names(top_header) = c(' ', ' ', 'Dentition Exam', ' ')

table = table %>% 
  knitr::kable(format = 'html') %>%
  kableExtra::kable_styling('striped', full_width = TRUE) %>% 
  kableExtra::add_header_above(header = top_header) %>%
  cat(., file = "nhanes_table.html")

# 79: -------------------------------------------------------------------------