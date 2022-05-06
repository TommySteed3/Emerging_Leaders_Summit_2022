###############################################################################
# Import Libraries

library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)



###############################################################################
# IMPORT DATA
setwd("C:/Tommy/Emerging")
df.mort1 <- fread('Data.csv')
df.mort2 <- fread('Data2.csv')


###############################################################################
###   INITIAL DATA CLEANING
df.mort <- rbind(df.mort1, df.mort2) # Combine 2 separate datasets into 1
colnames(df.mort) # Look at column names - spaces could cause issues

df.mort <- df.mort %>%
  clean_names()  #remove spaces in column names and make lower case

colnames(df.mort)

##  SIMPLIFY OUR DATASET: USE 2015VBT
df.mort$expected.count <- df.mort$expected_death_qx2015vbt_by_policy
df.mort$expected.face <- df.mort$expected_death_qx2015vbt_by_amount

colnames(df.mort)

## DROP COLUMNS WE DON'T NEED
df.mort <- df.mort %>%
  select(-c(23:32))

colnames(df.mort)

## PRELIMINARY DATA CHECK

colnames(df.mort) # Look at column names ; much cleaner now with our fix
dim(df.mort) # Dimensions of dataframe (rows x columns) 1,548,574 rows by 32 columns
str(df.mort) # What are the datatypes for each column ; numbers integers and characters - will probably need to adjust some of these
summary(df.mort) #summary stats ; many columns have NA values ; check for out of bounds

table(df.mort$insurance_plan) #what are the total rows for each of the levels of insurance_plan - Perm and Term seem to have most

## EXPORT YOUR CLEAN DATASET - THIS WILL BE USED IN OUR ANALYSIS GOING FORWARD
## DO NOT WANT TO HAVE TO REPEAT THE PRIOR TASKS - BUT SAVE THIS CODE FOR AUDIT TRAIL

fwrite(df.mort, 'Final_Data.csv')

###############################################################################
# PROTOTYPE DATA VISUALIZATIONS 

# GROUP DATA, SUMMARIZE A METRIC, CREATE NEW METRICS : EXTREMELY COMMON TASK
df.agg <- df.mort %>%
  group_by(attained_age) %>%
  summarise(actual.count = sum(number_of_deaths),
            expected.count = sum(expected.count),
            actual.face = sum(death_claim_amount),
            expected.face = sum(expected.face)) %>%
  mutate(ae.count = actual.count/expected.count,
         ae.face = actual.face/expected.face)


# VISUALIZE OUR RESULTS

g <- ggplot(df.agg, aes(x = attained_age, y = ae.count)) + geom_point() + geom_line()
ggplotly(g)


h <- ggplot(df.agg, aes(x = attained_age, y = ae.face)) + geom_point() + geom_line()
ggplotly(h)



##  CONVERT DATA FROM WIDE TO LONG IN ORDER TO COMBINE THE ABOVE 2 GRAPHS INTO 1:
library(tidyr)
df.agg.long <- df.agg %>%
  gather(Basis, a.to.e, ae.count:ae.face)


i <- ggplot(df.agg.long,aes(x = attained_age, y = a.to.e, group = Basis, color = Basis)) + geom_point() + geom_line()
ggplotly(i)


##  IF WE WANT TO ITERATE THROUGH VARIOUS CUTS - USE SHINY...