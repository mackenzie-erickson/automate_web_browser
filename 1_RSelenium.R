
# Scrape data from NCLS website

library(tidyverse)
library(httr)
library(rvest) 
library(RSelenium)
library(stringr)

# devtools::install_github("ropensci/RSelenium")


set.seed(11111) 

#
# set up remote driver
#

remDR <- RSelenium::rsDriver(port = 4555L, 
                             browser = "chrome",
                             chromever = "80.0.3987.16")

# if you want to open a new window
# (rsDriver should open one automatically)
# remDR$client$open()

# Sometimes if the port is full, it's useful to close the driver and re-do it. 
# There's code in here to close the driver
# If it doens't work - do garbage collection - gc()

# Rename for convience
# remDR <- remDR$client


# # Navigate to page
# remDR$client$navigate("https://www.ncsl.org/research/energy/energy-legislation-tracking-database.aspx")
# 
# # # Navigate around page
# # webElem <- remDR$client$findElement("css", "body")
# # webElem$sendKeysToElement(list(key = "end"))
# 
# #
# # Select topics
# #
# 
# # I don't want all the topics, but I want most of them. 
# # So first, I'll select all, and then unclick the ones I don't want
#   # Select all
# all_topics <- remDR$client$findElement(using = "id", value = "dnn_ctr85406_StateNetDB_ckBxAllTopics")
# all_topics$clickElement()
# 
# 
#   # Select all I don't want
# 
# # Vector of unwanted topic ids
# dont_want_vec <- c("dnn_ctr85406_StateNetDB_ckBxTopics_4", # electric grid
#                    "dnn_ctr85406_StateNetDB_ckBxTopics_7", # energy security
#                    "dnn_ctr85406_StateNetDB_ckBxTopics_14", # nuclear waste
#                    "dnn_ctr85406_StateNetDB_ckBxTopics_15", # nuclear facilities
#                    "dnn_ctr85406_StateNetDB_ckBxTopics_20", # transportation
#                    "dnn_ctr85406_StateNetDB_ckBxTopics_22" # utility regulation
# )
# 
# # Find and click element - Function
# find_and_click <- function(using, value){
#   x <- remDR$client$findElement(using, value)
#   x$clickElement()
# }
# 
# 
# # Apply find_and_click to unwanted topic vector
# i <- 1
# for (i in dont_want_vec) {
#   find_and_click("id", i)
# }
# 
# 
# # Select all years
# 
# year_All <- remDR$client$findElement(using = 'xpath', value = "/html/body/form/div[3]/div[3]/div/div[2]/main/div[1]/div/div[3]/div/div[4]/div/div[1]/div[7]/div[2]/select/option[1]")
# year_All$clickElement()
# 
# 
# # Select state
# 
# state_alabama <- remDR$client$findElement(using = "name", value = "dnn$ctr85406$StateNetDB$ckBxStates$0")
# state_alabama$clickElement()
# 
# 
# 
# # Click search
# 
# find_and_click("name", "dnn$ctr85406$StateNetDB$btnSearch")
# Sys.sleep(5)
# 
# 
# #
# # Scrape contents
# #
# 
# # (in rvest you would use read_html, but here you use getPageSource)
# page_source <- remDR$client$getPageSource() # once that's done scrape the html
# 
# # close driver
# remDR$client$close()
# 
# 
# 
# #
# # Clean HTML
# #
# 
# # Turn page source into html
# source_xml <- read_html(page_source[[1]])
# 
# # Get all nodes and subnodes
# ncsl_nodes <-
#   source_xml %>% 
#   html_nodes("div#dnn_ctr85406_StateNetDB_linkList")
# 
# 
# 
# # Store just the text
# ncsl_text <- ncsl_nodes %>% html_text()
# 
# 
# 
# save(ncsl_text, file = "~/Box/Diffusion/Data/NCSL/Alabama.RData")
# 






  



# HTML path names - list

state_path_names <- list(
  Alabama = "dnn$ctr85406$StateNetDB$ckBxStates$0",
  Alaska = "dnn$ctr85406$StateNetDB$ckBxStates$1",  
  Arizona = "dnn$ctr85406$StateNetDB$ckBxStates$2",     
  Arkansas = "dnn$ctr85406$StateNetDB$ckBxStates$3",   
  California = "dnn$ctr85406$StateNetDB$ckBxStates$4",
  Colorado = "dnn$ctr85406$StateNetDB$ckBxStates$5",     
  Connecticut = "dnn$ctr85406$StateNetDB$ckBxStates$6",    
  Delaware = "dnn$ctr85406$StateNetDB$ckBxStates$7",
  Florida = "dnn$ctr85406$StateNetDB$ckBxStates$9",
  Georgia = "dnn$ctr85406$StateNetDB$ckBxStates$10",    
  Hawaii = "dnn$ctr85406$StateNetDB$ckBxStates$11",
  Idaho = "dnn$ctr85406$StateNetDB$ckBxStates$12",
  Illinois = "dnn$ctr85406$StateNetDB$ckBxStates$13",    
  Indiana = "dnn$ctr85406$StateNetDB$ckBxStates$14",
  Iowa = "dnn$ctr85406$StateNetDB$ckBxStates$15",
  Kansas = "dnn$ctr85406$StateNetDB$ckBxStates$16",      
  Kentucky = "dnn$ctr85406$StateNetDB$ckBxStates$17",
  Louisiana = "dnn$ctr85406$StateNetDB$ckBxStates$18",
  Maine = "dnn$ctr85406$StateNetDB$ckBxStates$19",      
  Maryland = "dnn$ctr85406$StateNetDB$ckBxStates$20",
  Massachusetts = "dnn$ctr85406$StateNetDB$ckBxStates$21",
  Michigan = "dnn$ctr85406$StateNetDB$ckBxStates$22",    
  Minnesota = "dnn$ctr85406$StateNetDB$ckBxStates$23",
  Mississippi = "dnn$ctr85406$StateNetDB$ckBxStates$24",
  Missouri = "dnn$ctr85406$StateNetDB$ckBxStates$25",
  Montana = "dnn$ctr85406$StateNetDB$ckBxStates$26",
  Nebraska = "dnn$ctr85406$StateNetDB$ckBxStates$27",
  Nevada = "dnn$ctr85406$StateNetDB$ckBxStates$28",
  New_Hampshire = "dnn$ctr85406$StateNetDB$ckBxStates$29",
  New_Jersey = "dnn$ctr85406$StateNetDB$ckBxStates$30",
  New_Mexico = "dnn$ctr85406$StateNetDB$ckBxStates$31",  
  New_York = "dnn$ctr85406$StateNetDB$ckBxStates$32",      
  North_Carolina = "dnn$ctr85406$StateNetDB$ckBxStates$33",
  North_Dakota = "dnn$ctr85406$StateNetDB$ckBxStates$34",
  Ohio = "dnn$ctr85406$StateNetDB$ckBxStates$35", 
  Oklahoma = "dnn$ctr85406$StateNetDB$ckBxStates$36",   
  Oregon = "dnn$ctr85406$StateNetDB$ckBxStates$37",
  Pennsylvania = "dnn$ctr85406$StateNetDB$ckBxStates$38", 
  Rhode_Island = "dnn$ctr85406$StateNetDB$ckBxStates$39",
  South_Carolina = "dnn$ctr85406$StateNetDB$ckBxStates$40",
  South_Dakota = "dnn$ctr85406$StateNetDB$ckBxStates$41", 
  Tennessee = "dnn$ctr85406$StateNetDB$ckBxStates$42", 
  Texas = "dnn$ctr85406$StateNetDB$ckBxStates$43",     
  Utah = "dnn$ctr85406$StateNetDB$ckBxStates$44",         
  Vermont = "dnn$ctr85406$StateNetDB$ckBxStates$45",
  Virginia = "dnn$ctr85406$StateNetDB$ckBxStates$46",      
  Washington = "dnn$ctr85406$StateNetDB$ckBxStates$47",   
  West_Virginia = "dnn$ctr85406$StateNetDB$ckBxStates$48",
  Wisconsin = "dnn$ctr85406$StateNetDB$ckBxStates$49",  
  Wyoming = "dnn$ctr85406$StateNetDB$ckBxStates$50"
)



# 
# Scrape - Function
#

# Function to select specific topics, all years, and one specific state
# x = state path name

scrape_func <- function(x){
  
  # Navigate to page
  remDR$client$navigate("https://www.ncsl.org/research/energy/energy-legislation-tracking-database.aspx")
  
  
  #
  # Select topics
  #
  
  # I don't want all the topics, but I want most of them. 
  # So first, I'll select all, and then unclick the ones I don't want
  # Select all
  all_topics <- remDR$client$findElement(using = "id", value = "dnn_ctr85406_StateNetDB_ckBxAllTopics")
  all_topics$clickElement()
  
  
  # Select all I don't want
  
  # Vector of unwanted topic ids
  dont_want_vec <- c("dnn_ctr85406_StateNetDB_ckBxTopics_4", # electric grid
                     "dnn_ctr85406_StateNetDB_ckBxTopics_7", # energy security
                     "dnn_ctr85406_StateNetDB_ckBxTopics_14", # nuclear waste
                     "dnn_ctr85406_StateNetDB_ckBxTopics_15", # nuclear facilities
                     "dnn_ctr85406_StateNetDB_ckBxTopics_20", # transportation
                     "dnn_ctr85406_StateNetDB_ckBxTopics_22" # utility regulation
  )
  
  # Find and click element - Function
  find_and_click <- function(using, value){
    x <- remDR$client$findElement(using, value)
    x$clickElement()
  }
  
  
  # Apply find_and_click to unwanted topic vector
  i <- 1
  for (i in dont_want_vec) {
    find_and_click("id", i)
  }
  
  
  # Select all years
  
  year_All <- remDR$client$findElement(using = 'xpath', value = "/html/body/form/div[3]/div[3]/div/div[2]/main/div[1]/div/div[3]/div/div[4]/div/div[1]/div[7]/div[2]/select/option[1]")
  year_All$clickElement()
  
  
  # Select state (path name = x)
  
  state <- remDR$client$findElement(using = "name", value = x)
  state$clickElement()
  
  
  
  # Click search
  
  find_and_click("name", "dnn$ctr85406$StateNetDB$btnSearch")
  Sys.sleep(5)
  
  
  #
  # Scrape contents
  #
  
  # (in rvest you would use read_html, but here you use getPageSource)
  page_source <- remDR$client$getPageSource() # once that's done scrape the html
  

  
  #
  # Clean HTML
  #
  
  # Turn page source into html
  source_xml <- read_html(page_source[[1]])
  
  # Get all nodes and subnodes
  ncsl_nodes <-
    source_xml %>% 
    html_nodes("div#dnn_ctr85406_StateNetDB_linkList")
  
  
  
  # Store just the text
  ncsl_text <- ncsl_nodes %>% html_text()

  return(ncsl_text)
  
} # End function







#
# Scrape function - Apply to all state path names
#

# Create named vector to save the html text into

ncsl_allStates <- vector("character", length = length(state.name))
names(ncsl_allStates) <- state.name

# Apply function
nscl_allStates <- lapply(state_path_names, scrape_func)





# close driver
remDR$client$close()

# Save data
save(nscl_allStates, file = "~/Box/Diffusion/Data/NCSL/All_States.RData") 








#
# START HERE (Don't need to re-run remote driver)
#
#
# Attempt to clean/string-split
#


load("~/Box/Diffusion/Data/NCSL/Alabama.RData")

massivetext <- ncsl_text[[1]]
# str_length(massivetext) #201,812

# get rid of extra spaces
massivetext <- gsub(pattern = "[ ]+", replacement = " ", massivetext)
# str_length(massivetext) #154,889


# get a mini test string
small_text <- str_sub(massivetext, 1, 1200)



# Where the pattern starts and stop
str_locate_all(string = massivetext, "\n \n AL") # 178: matches number of bills


bill_list <- str_split(massivetext, "\n \n AL")

# "[:upper:]{2}\\s[:uppper:]{1,3}\\s[1234567890]"
# look for 2 uppercase letters, \s means space, then again for upper case letter between 1-3 of them (if you only had 1 and comma, it would be 1 or more)
# then another space, then anything that's in the brackets (i.e. a number)

bill_vector <- unlist(bill_list)

length(bill_vector)
bill_vector[3]

bill_df <- as_tibble(bill_vector, names = "Bill")
head(bill_df)
dim(bill_df)
names(bill_df) <- "Bill"


bill_df_testsplit <- separate(bill_df, col = Bill, into = "Year",
                              sep = "[^[:upper:]{1,3}[:space:][:digit:]{1,3}\n[:digit]{4}]+",
                              extra = "merge", fill = "right")
text_bill <- bill_df[2, ]

separate(text_bill, into = )

str_locate(text_bill, pattern = "[:upper:][:space:][:digit:]")

text_bill[,2:4]

separate(bill_df, col = Bill, into = "Year", sep = "[:upper:][:space:][:digit:]", extra = "merge", fill = "right")


grep("[ABCDEFGHIJKLMNOPQRSTUVWXYZ] \d{2}", text_bill$Bill[2], perl = TRUE)







