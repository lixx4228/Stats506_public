# Week 10, data.table / SQL / dplyr activity
# 
# The examples below use the Lahman data from the SQL notes. Each example 
# provides a snippet of data.table, SQL, or dplyr code for you to translate 
# into the other two languages. 
#
# For each question, fill in the missing dplyr, SQL, or data.table 
# translations.
#
# Author: Group 3
# Updated: November 17, 2020
# 79: -------------------------------------------------------------------------

## Setup ######################################################################
# Packages: -------------------------------------------------------------------
library(tidyverse)
library(dbplyr)
library(Lahman)
library(data.table)

# Create a local SQLlite database of the Lahman data: -------------------------
#! If this fails, run: install.packages('RSQLite')
lahman = lahman_sqlite()

# Copy the batting table to memory as a tibble: -------------------------------
batting_tbl = lahman %>% 
  tbl("BATTING") %>% 
  collect()
class(batting_tbl)

# Convert the copy in memory to a data.table: ---------------------------------
batting_dt = as.data.table(batting_tbl)
class(batting_dt)

## Question 1 #################################################################

# data.table (select in j)
batting_dt[, .(playerID, yearID, league = lgID, stint)]  

# SQL
query = 
'SELECT playerID, yearID, lgID as league, stint
  FROM batting
'
lahman %>% tbl(sql(query))

# dplyr
batting_tbl %>% 
  transmute(playerID,
            yearID,
            league = lgID,
            stint)

## Question 2 #################################################################
# compute the batting average = H / AB for each row (stint)

# SQL
query = 
'
SELECT playerID, yearID, lgID as league,
       (Cast (H as Float) /  Cast (AB as Float) ) as avg
FROM BATTING
'
lahman %>% tbl(sql(query)) %>% collect() 

# Note: R will handle the conversion of integers to numeric. 


# data.table (compute in "j")
batting_dt[, .(avg = sum(H) / sum(AB)), 
           .(playerID, yearID, league = lgID)] 

# dplyr 
batting_dt %>% 
  transmute(playerID,
            yearID,
            league = lgID,
            avg = H/AB)


## Question 3 #################################################################
# compute the maximum HBP in a "stint" (player season with a single team)

# dplyr
batting_tbl %>%
  summarize(max_HBP = max(HBP, na.rm = TRUE))

# data.table (summarize in "j")
batting_dt[!is.na(HBP), .(max_HBP = max(HBP))] 

# SQL
query = 
'
select max(HBP) as max_HBP 
  from batting
  where HBP is not null
'
lahman %>% tbl(sql(query)) %>% collect()

## Question 4 #################################################################
# compute the lifetime batting average for each player

# data.table, (group with "by")
batting_dt[ , .(avg = sum(H) / sum(AB)), by = .(playerID)]

# SQL, cast integers to numeric following example in question 2
query = 
'
select playerID, (cast(sum(H) as Float)/cast(sum(AB) as Float)) as avg
  from batting
  group by playerID
'
lahman %>% tbl(sql(query)) %>% collect()

# dplyr
batting_tbl %>% 
  group_by(playerID) %>% 
  summarise(avg = sum(H)/sum(AB))

## Question 5 #################################################################
# select rows for year 2016

# dplyr: filter
batting_tbl %>%
  filter(yearID == 2016) %>%
  select(playerID, HBP)

# data.table (subset in i)
batting_dt[yearID == 2016, .(playerID, HBP)]

# SQL
query = 
'
select playerID, HBP
  from batting
  where yearID == 2016
  
'
lahman %>% tbl(sql(query)) 

## Question 6 #################################################################
# find players with more than 400 total HR from 2000-2019 and display in 
# descending order

#SQL: nested anonymous table, "HAVING"
query = 
'
SELECT *
FROM (
 SELECT playerID, sum(HR) as HR
 FROM BATTING
 WHERE yearID > 1999
 GROUP BY playerID
) 
WHERE HR > 400
ORDER BY -HR
'
lahman %>% tbl(sql(query)) %>% collect()

# data.table (use %>% with .[], or chaining dt[][])
batting_dt[yearID > 1999, .(HR = sum(HR)), .(playerID)] %>% 
  .[order(-HR)] %>%
  .[HR > 400]

# dplyr
batting_tbl %>% 
  filter(yearID > 1999) %>% 
  select(playerID, HR) %>% 
  group_by(playerID) %>% 
  summarise(HR = sum(HR)) %>% 
  filter(HR > 400) %>% 
  arrange(-HR)


## Question 7 #################################################################
# find the number of "20-20" seasons with 20+ SB and 20+ HR by a 
# player (across all stints) in each year, 2000-2019 since 2000

# SQL: 20-20
query = 
  '
SELECT yearID, COUNT(playerID) as N
FROM (
 SELECT playerID, yearID, sum(SB) as SB, sum(HR) as HR
 FROM BATTING
 WHERE yearID > 1999
 GROUP BY playerID, yearID
 HAVING SB > 19 AND HR > 19
)
GROUP BY yearID
'
lahman %>% tbl(sql(query)) %>% collect()

# data.table: 20-20
batting_dt[yearID > 1999 & SB > 19 & HR > 19,
           .(SB = sum(SB), HR = sum(HR)), 
           by = .(yearID, playerID)][, .N, .(yearID)]

# dplyr
batting_tbl %>% 
  filter(yearID > 1999) %>% 
  group_by(yearID, playerID) %>% 
  filter(SB > 19, HR > 19) %>%
  summarise(SB = sum(SB), HR = sum(HR)) %>% 
  group_by(yearID) %>% 
  summarise(N = n())


# SQL: 20-20 (Modified)
query = 
'
SELECT yearID, COUNT(playerID) as N
FROM (
 SELECT playerID, yearID, sum(SB) as SB, sum(HR) as HR
 FROM BATTING
 WHERE yearID > 1999
 GROUP BY playerID, yearID
)
where SB > 19 AND HR > 19
GROUP BY yearID
'
lahman %>% tbl(sql(query)) %>% collect()

# data.table: 20-20
batting_dt[yearID > 1999,
           .(SB = sum(SB), HR = sum(HR)), 
           by = .(yearID, playerID)][SB > 19 & HR > 19, .N, .(yearID)]

# dplyr
batting_tbl %>% 
  filter(yearID > 1999) %>% 
  group_by(yearID, playerID) %>% 
  summarise(SB = sum(SB), HR = sum(HR)) %>% 
  filter(SB > 19, HR > 19) %>% 
  group_by(yearID) %>% 
  summarise(N = n())


# 79: -------------------------------------------------------------------------

