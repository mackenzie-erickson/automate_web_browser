
rm(list = ls())

library(tidyverse)
library(stringr)

ncsl_raw <- get({load("~/Box/Diffusion/Data/NCSL/Scraped_Data/AllStates_DataFrame.RData")})
rm(all_states_df)


# To do:
#
# Need to get the associated bills out and have NA otherwise (in NCSL_Strings_to_df.R)


############################
#
# Data wrangling
#
############################


#
# Create variables 
#



ncsl_clean <- ncsl_raw %>% 
  
  # Party info
  mutate(repb_authors = str_count(author, pattern = "\\(R\\)"), # count of R (incl. sponsor)
         dem_authors = str_count(author, "\\(D\\)") + str_count(author, "\\(DFL\\)") + str_count(author, "\\(P\\)"), # DFL (minnesota) and P (vermont) are both assc. with Dem party
         lead_author_affiliation = str_extract(author, "\\(\\w{1,3}\\)"),
        
          # Author info
         additional_authors = str_extract(author, "(?<=Additional Authors:).*"), # Extract anything that follows 'additional authors'
         lead_author = ifelse(test = str_detect(author, "Additional Authors:") == TRUE, # if there are additional authors...
                              yes = str_extract(author, ".*(?=Additional Authors:)"), # ...extract anything before 'additional authors"
                              no = str_extract(author, ".*")),  # ...else extract everything
         
         # Bill Status info
         status_clean = str_match(status, "Pending|Enacted|Adopted|Failed|Vetoed|To Governor"), # extract status, else NA
         status_details = str_replace(status, "Pending - |Enacted - |Adopted - |Failed - |Vetoed - |To Governor - ", ""), # extract details
         
         # Date Last Action info
         date_last_action_clean = str_extract(date_last_action, "\\d{1,2}\\/\\d{1,2}\\/\\d{4}"), # extract date
         date_last_action_details = str_remove(date_last_action, "\\d{1,2}\\/\\d{1,2}\\/\\d{4}( - )?") # extract details
  )

         


# DFL is the Minnesota Democratic Farmer's Party (associated with Dem) - switch lead author to Dem
# P is the Vermont Progressive Party (associated with Dem) - switch lead author to Dem
ncsl_clean$lead_author_affiliation <- ifelse(ncsl_clean$lead_author_affiliation == "(DFL)", "(D)", ncsl_clean$lead_author_affiliation)
ncsl_clean$lead_author_affiliation <- ifelse(ncsl_clean$lead_author_affiliation == "(P)", "(D)", ncsl_clean$lead_author_affiliation)



# Rename and reorder
ncsl <- ncsl_clean %>% 
  
  rename(status_full = status, # Retain the full status info from raw data
         date_last_action_full = date_last_action, # Retain full date_last_action info from raw data
         status = status_clean, # Rename clean_status to just 'status'
         date_last_action = date_last_action_clean, # Rename date_clean to just 'date'
         authors_full = author,
         state_name = statename, # Rename just b/c I like it better
         state_abb = stateabb, # ^
         bill = legislation, # ^
         bill_title = legislation_name) %>% # ^
  
  select(state_name, state_abb, bill, year, bill_title, status, status_details, date_last_action, date_last_action_details,
         lead_author, additional_authors, topic, lead_author_affiliation, repb_authors, dem_authors, summary, history, everything())


# Convert year from factor to integer
ncsl$year <- as.integer(as.character(ncsl$year))



#######################
# Clean topics (separate into individual vectors)
######################


# Split each topic into its own column
ncsl <- separate(data = ncsl, 
                 col = topic, 
                 into = c("topic_1", "topic_2", "topic_3", 
                          "topic_4", "topic_5", "topic_6"), 
                 sep = ",")

# Clean Topics Function - Create
# (Remove repeated information from subsequent topics)
split_topic_func <- function(column){
  sub(pattern = ".*-", replacement = "", x = column)
}

# Clean Topics Function - Apply
ncsl <- ncsl %>% mutate_at(.vars = c("topic_2", "topic_3", 
                                     "topic_4", "topic_5", "topic_6"),
                           .funs = split_topic_func)

# Set to factors
ncsl <- ncsl %>% mutate_at(vars(starts_with("topic")), .funs = as.factor)



#####################################
# Committee survival - 
  # Create dummy
#####################################

# Died (0) if status_details contains "Committee" + status = "Failed"
# Survived (1) otherwise
ncsl <- ncsl %>% mutate(survived = ifelse(test = str_detect(status_details, ".*Committee.*") == TRUE,
                                           yes = 0, # died
                                           no = 1)) # survived



# Save data
saveRDS(ncsl, "~/Box/Diffusion/Data/NCSL//Bill_Data_Formatted.RData", version = 2)






##################################
# State legislative control
##################################

######
# Parse from NCSL pdfs
#####

# Libraries
library(rJava)
  # Note: Legacy Java 6 is required. Instructions to allow installation: https://www.harrisgeospatial.com/Support/Self-Help-Tools/Help-Articles/Help-Articles-Detail/ArtMID/10220/ArticleID/23780/macOS-Catalina-1015-ENVIIDL-and-Legacy-Java-6-Dependencies
library(tabulizerjars)
library(tabulizer)


# Function - scrape/wrangle party info
#######################################################################

parse_party_info <- function(file){

# Scrape the table data
table_scrape <- extract_tables(file = file, 
                       method = "decide",
                       output = "data.frame")

# Extract the table (first list object)
table_raw <- table_scrape %>% pluck(1) %>% as_tibble()


#
# Column values - correct
#


table_x <- table_raw %>% 
  # Remove first row
  slice(-1) %>% 
  # Remove rows after the 50 states (leaving wiggle room for 2013 which has two first rows)
  slice(-(52:nrow(.))) %>% 
# Missing values - change to NA
  mutate_all(list(~na_if(.,""))) %>% # missing strings
  mutate_all(list(~na_if(., "N/A"))) %>% # character N/A
  mutate_all(list(~na_if(., "-"))) %>% # dash (Nebraska)
  mutate_all(list(~gsub(pattern = "Unicameral", 
                        replacement = NA, 
                        x = ., 
                        ignore.case = TRUE))) %>%  # Unicameral instead of number
  # Remove asterisks
  mutate_all(list(~gsub(pattern = "\\*", 
                        replacement = "", 
                        x = .)))



} # End of parse_party_function
  
#################################################################  

#
# Files
#
#
# Parse party info - apply
party_2020 <- parse_party_info("~/Box/Diffusion/Data/NCSL/Leg_Control/Legis_Control_2020.pdf")
party_2019 <- parse_party_info("~/Box/Diffusion/Data/NCSL/Leg_Control/Legis_Control_2019.pdf")
party_2018 <- parse_party_info("~/Box/Diffusion/Data/NCSL/Leg_Control/Legis_Control_2018.pdf")
party_2017 <- parse_party_info("~/Box/Diffusion/Data/NCSL/Leg_Control/Legis_Control_2017.pdf")
party_2016 <- parse_party_info("~/Box/Diffusion/Data/NCSL/Leg_Control/Legis_Control_2016.pdf")
party_2015 <- parse_party_info("~/Box/Diffusion/Data/NCSL/Leg_Control/Legis_Control_2015.pdf")
party_2014 <- parse_party_info("~/Box/Diffusion/Data/NCSL/Leg_Control/Legis_Control_2014.pdf")
party_2013 <- parse_party_info("~/Box/Diffusion/Data/NCSL/Leg_Control/Legis_Control_2013.pdf")
party_2012 <- parse_party_info("~/Box/Diffusion/Data/NCSL/Leg_Control/Legis_Control_2012.pdf")
party_2011 <- parse_party_info("~/Box/Diffusion/Data/NCSL/Leg_Control/Legis_Control_2011.pdf")
party_2010 <- parse_party_info("~/Box/Diffusion/Data/NCSL/Leg_Control/Legis_Control_2010.pdf")
party_2009 <- parse_party_info("~/Box/Diffusion/Data/NCSL/Leg_Control/Legis_Control_2009.pdf")



# Vector of correct names
col_names_correct <- c("State", "Total_Seats", "Total_Senate", "Senate_Dem",
                       "Senate_Rep", "Senate_Other", "Total_House", "House_Dem", "House_Rep",
                       "House_Other", "Legis_Control", "Gov_Party", "State_Control")

##############################################
# Correct data.frames as necessary
  # Separate columns and apply correct names
  # Remove padding row from bottom (except for 2013)
############################################

# 2020
party_2020_correct <- party_2020 %>% 
  slice(-51) %>% # Remove last row
  separate(col = "Total.House", # Separate columns
           into = c("House1", "House2"),
           sep = " ") %>% 
  mutate_all(list(~na_if(., "N/A"))) %>% # Change "N/A" to NA
  select_if(~ !all(is.na(.))) %>% # Remove  columns of all NA
  set_names(col_names_correct) %>% # Set new names
  mutate(Year = "2020") # Add year column

# 2019
party_2019_correct <- party_2019 %>% 
  slice(-51) %>%
  separate(col = "Total.House",
           into = c("House1", "House2"),
           sep = " ") %>% 
  mutate_all(list(~na_if(., "N/A"))) %>%
  select_if(~ !all(is.na(.))) %>% 
  set_names(col_names_correct) %>% 
  mutate(Year = "2019")

# 2018
party_2018_correct <- party_2018 %>% 
  slice(-51) %>%
  separate(col = "Total.House",
           into = c("House1", "House2"),
           sep = " ") %>% 
  separate(col = "Legis..Gov.",
           into = c("Legis_Control", "Gov_Party"),
           sep = " ",
           extra = "merge")

# Fix - LA didn't populate correctly
party_2018_correct[which(party_2018_correct$X == "Louisiana"), 
           which(colnames(party_2018_correct) == "Gov_Party")] <- "Dem"

party_2018_correct[which(party_2018_correct$X == "Louisiana"), 
                   which(colnames(party_2018_correct) == "State")] <- "Divided"
  
party_2018_correct <- party_2018_correct %>% 
  mutate_all(list(~na_if(., "N/A"))) %>%
  select_if(~ !all(is.na(.))) %>% 
  set_names(col_names_correct) %>% 
  mutate(Year = "2018")


# 2017
party_2017_correct <- party_2017 %>% 
  slice(-51) %>%
  separate(col = "Total.House",
              into = c("House1", "House2"),
              sep = " ") %>% 
  separate(col = "Legis..Gov.",
           into = c("Legis_Control", "Gov_Party"),
           sep = " ",
           extra = "merge")
# Fix - LA again
party_2017_correct[which(party_2017_correct$X == "Louisiana"), 
                   which(colnames(party_2017_correct) == "Gov_Party")] <- "Dem"
  
party_2017_correct <- party_2017_correct %>% 
  mutate_all(list(~na_if(., "N/A"))) %>% 
  select_if(~ !all(is.na(.))) %>% 
  set_names(col_names_correct) %>% 
  mutate(Year = "2017")
  

# 2016
party_2016_correct <- party_2016 %>% 
  slice(-51) %>%
  separate(col = "Total.Total",
           into = c("Total_Seats", "Total_Senate"),
           sep = " ") %>% 
  separate(col = "Senate.Senate",
           into = c("Senate_Dem", "Senate_Rep"),
           sep = " ") %>% 
  separate(col = "House.House",
           into = c("House_Dem", "House_Rep"),
           sep = " ") %>% 
  separate(col = "Legis..Gov.",
           into = c("Legis_Control", "Gov_Party"),
           sep = " ",
           extra = "merge")

# Fix - LA 
party_2016_correct[which(party_2016_correct$X == "Louisiana"), 
                   which(colnames(party_2016_correct) == "Gov_Party")] <- "Rep"

party_2016_correct <- party_2016_correct %>% 
  mutate_all(list(~na_if(., "N/A"))) %>% 
  select_if(~ !all(is.na(.))) %>% 
  set_names(col_names_correct) %>% 
  mutate(Year = "2016")


# 2015
party_2015_correct <- party_2015 %>% 
  slice(-51) %>%
  separate(col = "Total.Total",
           into = c("Total_Seats", "Total_Senate"),
           sep = " ") %>% 
  separate(col = "Senate.1",
           into = c("Senate_Dem", "Senate_Rep"),
           sep = " ") %>% 
  separate(col = "House.House",
           into = c("House_Dem", "House_Rep"),
           sep = " ")

# Fix - LA 
party_2015_correct[which(party_2015_correct$X == "Louisiana"), 
                   which(colnames(party_2015_correct) == "Gov.")] <- "Rep"

party_2015_correct <- party_2015_correct %>% 
  mutate_all(list(~na_if(., "N/A"))) %>% 
  select_if(~ !all(is.na(.))) %>% 
  set_names(col_names_correct) %>% 
  mutate(Year = "2015")

# 2014
party_2014_correct <- party_2014 %>% 
  slice(-51) %>%
  separate(col = "Senate.Senate",
           into = c("Senate_Dem", "Senate_Rep"),
           sep = " ") %>% 
  separate(col = "House.House",
           into = c("House_Dem", "House_Rep"),
           sep = " ")

# Fix - remove "X.3" column of all "e"
party_2014_correct <- party_2014_correct %>% select(!"X.3")

party_2014_correct <- party_2014_correct %>% 
  mutate_all(list(~na_if(., "N/A"))) %>% 
  select_if(~ !all(is.na(.))) %>% 
  set_names(col_names_correct) %>% 
  mutate(Year = "2014")


# 2013

# Remove "e" from 3rd column
party_2013$X.As.of.January.31..2013. <-
  party_2013$X.As.of.January.31..2013. %>% 
  gsub(pattern = " e ", replacement = " ", x = .)

party_2013_correct <- party_2013 %>% 
  # slice(-51) %>%
  separate(col = 3,
           into = c("Legis_Control", "Gov_Party", "State_Control"),
           sep = " ") %>% 
   separate(col = 2,
           into = c("Total_Seats", "Total_Senate", "Senate_Dem", "Senate_Rep",
                    "Senate_Other", "Total_House", "House_Dem", "House_Rep",
                    "House_Other"),
           sep = " ")

party_2013_correct <- party_2013_correct %>% 
  slice(-1) %>% 
  mutate_all(list(~na_if(., "N/A"))) %>% 
  select_if(~ !all(is.na(.))) %>% 
  set_names(col_names_correct) %>% 
  mutate(Year = "2013")


# 2012
party_2012_correct <- party_2012 %>% 
  slice(-51) %>%
  separate(col = "Senate.Senate",
           into = c("Senate_Dem", "Senate_Rep"),
           sep = " ") %>% 
  separate(col = "House.House",
           into = c("House_Dem", "House_Rep"),
           sep = " ")


party_2012_correct <- party_2012_correct %>% 
  mutate_all(list(~na_if(., "N/A"))) %>% 
  select_if(~ !all(is.na(.))) %>% 
  set_names(col_names_correct) %>% 
  mutate(Year = "2012")


# 2011
party_2011_correct <- party_2011 %>% 
  slice(-51) %>%
  separate(col = "Senate.Senate",
           into = c("Senate_Dem", "Senate_Rep"),
           sep = " ") %>% 
  separate(col = "House.House",
           into = c("House_Dem", "House_Rep"),
           sep = " ")


party_2011_correct <- party_2011_correct %>% 
  mutate_all(list(~na_if(., "N/A"))) %>% 
  select_if(~ !all(is.na(.))) %>% 
  set_names(col_names_correct) %>% 
  mutate(Year = "2011")


# 2010
party_2010_correct <- party_2010 %>% 
  slice(-51) %>%
  separate(col = "Senate.Senate",
           into = c("Senate_Dem", "Senate_Rep"),
           sep = " ") %>% 
  separate(col = "House.House",
           into = c("House_Dem", "House_Rep"),
           sep = " ")


party_2010_correct <- party_2010_correct %>% 
  mutate_all(list(~na_if(., "N/A"))) %>% 
  select_if(~ !all(is.na(.))) %>% 
  set_names(col_names_correct) %>% 
  mutate(Year = "2010")


# 2009
party_2009_correct <- party_2009 %>% 
  slice(-51) %>%
  separate(col = "Senate.Senate",
           into = c("Senate_Dem", "Senate_Rep"),
           sep = " ") %>% 
  separate(col = "Total.House",
           into = c("House_Dem", "House_Rep"),
           sep = " ")


party_2009_correct <- party_2009_correct %>% 
  mutate_all(list(~na_if(., "N/A"))) %>% 
  select_if(~ !all(is.na(.))) %>% 
  set_names(col_names_correct) %>% 
  mutate(Year = "2009")



#########################
# Combine party info into one dataset
#########################


party_allStates_2009_2020 <- 
  bind_rows(party_2009_correct, party_2010_correct, party_2011_correct,
            party_2012_correct, party_2013_correct, party_2014_correct,
            party_2015_correct, party_2016_correct, party_2017_correct,
            party_2018_correct, party_2019_correct, party_2020_correct)



# Replace "Split" term with "Divided"
party_allStates_2009_2020$Legis_Control <- 
  ifelse(party_allStates_2009_2010$Legis_Control == "Split",
         "Divided",
         party_allStates_2009_2010$Legis_Control)

# Convert numbers to numeric
party_allStates_2009_2020 <- party_allStates_2009_2020 %>% 
  mutate_at(.vars = c("Total_Seats", "Total_Senate", "Senate_Dem", "Senate_Rep",
                      "Total_House", "House_Dem", "House_Rep", "Year"), 
            .funs = as.numeric)

# Rearrange
party_allStates_2009_2020 <- party_allStates_2009_2020 %>% 
  select(State, Year, everything()) %>% 
  arrange(State, Year)

# # Save to GitHub 
# library(writexl)
# write_xlsx(party_allStates_2009_2020, "~/Box/GitHub/state_politics/Data//State_Party_Composition.xlsx")




####################################
# Controls
####################################

# State affluance
# Renewable potential, fossil fuel endowment
  #	Fossil fuel - Considered fixed, and therefore just used FE for model 1
  # Wind power potential, biomass potential, solar potential (data sources in Carley 2009) Words of caution: they aren't truly measuring potential because that could be effected by technology/political feasibility/etc


















