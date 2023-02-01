rm(list = ls())

library(tidyverse)
# data(state.name)
# data(state.abb)

state.df <- data.frame(name = state.name, 
                       abb = state.abb)


state.df$name <- as.character(state.df$name)
state.df$abb <- as.character(state.df$abb)
# all_states <- get({load("C:/Users/jared/Downloads/All_States.RData")})
all_states <- get({load("~/Box/Diffusion/Data/NCSL/All_States.RData")})
# state_1 <- all_states[[2]]


summarize_state_data <- function(data)
{
  new_data <- trimws(unlist(strsplit(data, split = "\n")))
  new_data <- new_data[new_data != ""]
  state_name <- new_data[1]
  new_data <- new_data[-1]
  
  state_flag = NULL
  i = 0
  while(is.null(state_flag))
  {
    i = i + 1
    if (grepl(state.df$name[i], state_name, ignore.case = T))
    {
      state_flag = state.df$name[i]
      state_abbrev = state.df$abb[i]
    }
  }
  
  
  
  legislation      <- new_data[toupper(substr(new_data, 1, 3)) == paste0(state_abbrev, " ")] 
  year             <- new_data[grepl("^[[:digit:]]+$", new_data) & nchar(new_data) == 4] 
  name_leg         <- new_data[which(grepl("^[[:digit:]]+$", new_data) & nchar(new_data) == 4) + 1]
  status           <- new_data[which(grepl("status:", substr(new_data, 1, 7), ignore.case = T)) + 1]
  date_last_action <- new_data[which(grepl("date of last action:", substr(new_data, 1, 20), ignore.case = T)) + 1]
  author           <- new_data[which(grepl("author:", substr(new_data, 1, 7), ignore.case = T)) + 1]
  topic            <- new_data[which(grepl("topics:", substr(new_data, 1, 7), ignore.case = T)) + 1]
  leg_summary      <- new_data[which((grepl("CompanionSummary:|Summary:", new_data, ignore.case = T) | grepl("summary:", substr(new_data, 1, 8), ignore.case = T))) + 1]
  history          <- new_data[which(grepl("history:", substr(new_data, 1, 8), ignore.case = T)) + 1]
  
  
  if (length(legislation) == length(year) & 
      length(legislation) == length(name_leg) & 
      length(legislation) == length(status) & 
      length(legislation) == length(date_last_action) & 
      length(legislation) == length(author) & 
      length(legislation) == length(date_last_action) & 
      length(legislation) == length(author) & 
      length(legislation) == length(topic) & 
      length(legislation) == length(leg_summary) & 
      length(legislation) == length(history))
  {
    return.item <- data.frame(legislation = legislation,
                        year = year,
                        legislation_name = name_leg, 
                        status = status,
                        date_last_action = date_last_action,
                        author = author,
                        topic = topic,
                        summary = leg_summary,
                        history = history)  
  } else {
    return.item <- list(legislation = legislation,
                              year = year,
                              legislation_name = name_leg, 
                              status = status,
                              date_last_action = date_last_action,
                              author = author,
                              topic = topic,
                              summary = leg_summary,
                              history = history)
    cat(paste0("\r Check state ", state_flag, "."))
  }
  
  
  return(return.item)
  
}  





# Create data - apply function



Alabama <- 
  summarize_state_data(all_states[[1]]) %>% 
  mutate(stateabb = state.abb[[1]],
         statename = state.name[[1]])


Alaska <- 
  summarize_state_data(all_states[[2]]) %>% 
  mutate(stateabb = state.abb[[2]],
         statename = state.name[[2]])

Arizona <- 
  summarize_state_data(all_states[[3]]) %>% 
  mutate(stateabb = state.abb[[3]],
         statename = state.name[[3]])

Arkansas <- 
  summarize_state_data(all_states[[4]]) %>% 
  mutate(stateabb = state.abb[[4]],
         statename = state.name[[4]])

California <- 
  summarize_state_data(all_states[[5]]) %>% 
  mutate(stateabb = state.abb[[5]],
         statename = state.name[[5]])

Colorado <- 
  summarize_state_data(all_states[[6]]) %>% 
  mutate(stateabb = state.abb[[6]],
         statename = state.name[[6]])

Connecticut <- 
  summarize_state_data(all_states[[7]]) %>% 
  mutate(stateabb = state.abb[[7]],
         statename = state.name[[7]])

Delaware <- 
  summarize_state_data(all_states[[8]]) %>% 
  mutate(stateabb = state.abb[[8]],
         statename = state.name[[8]])

Florida <- 
  summarize_state_data(all_states[[9]]) %>% 
  mutate(stateabb = state.abb[[9]],
         statename = state.name[[9]])

Georgia <- 
  summarize_state_data(all_states[[10]]) %>% 
  mutate(stateabb = state.abb[[10]],
         statename = state.name[[10]])


# Problem: RSelenium didn't pull
# Hawaii <- 
#   summarize_state_data(all_states[[11]]) %>% 
#   mutate(stateabb = state.abb[[11]],
#          statename = state.name[[11]])

Idaho <- 
  summarize_state_data(all_states[[12]]) %>% 
  mutate(stateabb = state.abb[[12]],
         statename = state.name[[12]])

Illinois <- 
  summarize_state_data(all_states[[13]]) %>% 
  mutate(stateabb = state.abb[[13]],
         statename = state.name[[13]])

Indiana <- 
  summarize_state_data(all_states[[14]]) %>% 
  mutate(stateabb = state.abb[[14]],
         statename = state.name[[14]])

Iowa <- 
  summarize_state_data(all_states[[15]]) %>% 
  mutate(stateabb = state.abb[[15]],
         statename = state.name[[15]])

Kansas <- 
  summarize_state_data(all_states[[16]]) %>% 
  mutate(stateabb = state.abb[[16]],
         statename = state.name[[16]])

Kentucky <- 
  summarize_state_data(all_states[[17]]) %>% 
  mutate(stateabb = state.abb[[17]],
         statename = state.name[[17]])

Louisiana <- 
  summarize_state_data(all_states[[18]]) %>% 
  mutate(stateabb = state.abb[[18]],
         statename = state.name[[18]])

Maine <- 
  summarize_state_data(all_states[[19]]) %>% 
  mutate(stateabb = state.abb[[19]],
         statename = state.name[[19]])

Maryland <- 
  summarize_state_data(all_states[[20]]) %>% 
  mutate(stateabb = state.abb[[20]],
         statename = state.name[[20]])

Massachusetts <- 
  summarize_state_data(all_states[[21]]) %>% 
  mutate(stateabb = state.abb[[21]],
         statename = state.name[[21]])

Michigan <- 
  summarize_state_data(all_states[[22]]) %>% 
  mutate(stateabb = state.abb[[22]],
         statename = state.name[[22]])

Minnesota <- 
  summarize_state_data(all_states[[23]]) %>% 
  mutate(stateabb = state.abb[[23]],
         statename = state.name[[23]])

Mississippi <- 
  summarize_state_data(all_states[[24]]) %>% 
  mutate(stateabb = state.abb[[24]],
         statename = state.name[[24]])

Missouri <- 
  summarize_state_data(all_states[[25]]) %>% 
  mutate(stateabb = state.abb[[25]],
         statename = state.name[[25]])

Montana <- 
  summarize_state_data(all_states[[26]]) %>% 
  mutate(stateabb = state.abb[[26]],
         statename = state.name[[26]])

Nebraska <- 
  summarize_state_data(all_states[[27]]) %>% 
  mutate(stateabb = state.abb[[27]],
         statename = state.name[[27]])

Nevada <- 
  summarize_state_data(all_states[[28]]) %>% 
  mutate(stateabb = state.abb[[28]],
         statename = state.name[[28]])

New_Hampshire <- 
  summarize_state_data(all_states[[29]]) %>% 
  mutate(stateabb = state.abb[[29]],
         statename = state.name[[29]])

New_Jersey <- 
  summarize_state_data(all_states[[30]]) %>% 
  mutate(stateabb = state.abb[[30]],
         statename = state.name[[30]])

New_Mexico <- 
  summarize_state_data(all_states[[31]]) %>% 
  mutate(stateabb = state.abb[[31]],
         statename = state.name[[31]])


# Problem: Function didn't work
# New_York <- 
#   summarize_state_data(all_states[[32]]) %>% 
#   mutate(stateabb = state.abb[[32]],
#          statename = state.name[[32]])


North_Carolina <- 
  summarize_state_data(all_states[[33]]) %>% 
  mutate(stateabb = state.abb[[33]],
         statename = state.name[[33]])

North_Dakota <- 
  summarize_state_data(all_states[[34]]) %>% 
  mutate(stateabb = state.abb[[34]],
         statename = state.name[[34]])

Ohio <- 
  summarize_state_data(all_states[[35]]) %>% 
  mutate(stateabb = state.abb[[35]],
         statename = state.name[[35]])

Oklahoma <- 
  summarize_state_data(all_states[[36]]) %>% 
  mutate(stateabb = state.abb[[36]],
         statename = state.name[[36]])

Oregon <- 
  summarize_state_data(all_states[[37]]) %>% 
  mutate(stateabb = state.abb[[37]],
         statename = state.name[[37]])

Pennsylvania <- 
  summarize_state_data(all_states[[38]]) %>% 
  mutate(stateabb = state.abb[[38]],
         statename = state.name[[38]])

Rhode_Island <- 
  summarize_state_data(all_states[[39]]) %>% 
  mutate(stateabb = state.abb[[39]],
         statename = state.name[[39]])

South_Carolina <- 
  summarize_state_data(all_states[[40]]) %>% 
  mutate(stateabb = state.abb[[40]],
         statename = state.name[[40]])

South_Dakota <- 
  summarize_state_data(all_states[[41]]) %>% 
  mutate(stateabb = state.abb[[41]],
         statename = state.name[[41]])

Tennessee <- 
  summarize_state_data(all_states[[42]]) %>% 
  mutate(stateabb = state.abb[[42]],
         statename = state.name[[42]])

Texas <- 
  summarize_state_data(all_states[[43]]) %>% 
  mutate(stateabb = state.abb[[43]],
         statename = state.name[[43]])

Utah <- 
  summarize_state_data(all_states[[44]]) %>% 
  mutate(stateabb = state.abb[[44]],
         statename = state.name[[44]])

Vermont <- 
  summarize_state_data(all_states[[45]]) %>% 
  mutate(stateabb = state.abb[[45]],
         statename = state.name[[45]])


# Problem: Function didn't work (returns "Check state" warning)
# Virginia <- 
#   summarize_state_data(all_states[[46]]) %>% 
#   mutate(stateabb = state.abb[[46]],
#          statename = state.name[[46]])

Washington <- 
  summarize_state_data(all_states[[47]]) %>% 
  mutate(stateabb = state.abb[[47]],
         statename = state.name[[47]])


# Problem: Function didn't work
# West_Virginia <- 
#   summarize_state_data(all_states[[48]]) %>% 
#   mutate(stateabb = state.abb[[48]],
#          statename = state.name[[48]])

Wisconsin <- 
  summarize_state_data(all_states[[49]]) %>% 
  mutate(stateabb = state.abb[[49]],
         statename = state.name[[49]])

Wyoming <- 
  summarize_state_data(all_states[[50]]) %>% 
  mutate(stateabb = state.abb[[50]],
         statename = state.name[[50]])







# Attempt to fix bad data


data <- all_states[[48]] #WV
data <- all_states[[which(state.abb == "PA")]]


# Clean whitespace

first_half_func <- function(data){
  new_data <- trimws(unlist(strsplit(data, split = "\n")))
  new_data <- new_data[new_data != ""]
  state_name <- new_data[1]
  new_data <- new_data[-1]
  return(new_data)
}



# IN BETWEEN - Need to manually set the state name and abbreviation



# Rest of function
second_half_func <- function(new_data){
  
  legislation      <- new_data[toupper(substr(new_data, 1, 3)) == paste0(state_abbrev, " ")]
  year             <- new_data[grepl("^[[:digit:]]+$", new_data) & nchar(new_data) == 4] 
  name_leg         <- new_data[which(grepl("^[[:digit:]]+$", new_data) & nchar(new_data) == 4) + 1]
  status           <- new_data[which(grepl("status:", substr(new_data, 1, 7), ignore.case = T)) + 1]
  date_last_action <- new_data[which(grepl("date of last action:", substr(new_data, 1, 20), ignore.case = T)) + 1]
  author           <- new_data[which(grepl("author:", substr(new_data, 1, 7), ignore.case = T)) + 1]
  topic            <- new_data[which(grepl("topics:", substr(new_data, 1, 7), ignore.case = T)) + 1]
  leg_summary      <- new_data[which((grepl("CompanionSummary:|Summary:", new_data, ignore.case = T) | grepl("summary:", substr(new_data, 1, 8), ignore.case = T))) + 1]
  history          <- new_data[which(grepl("history:", substr(new_data, 1, 8), ignore.case = T)) + 1]
  
  
  
  if (length(legislation) == length(year) & 
      length(legislation) == length(name_leg) & 
      length(legislation) == length(status) & 
      length(legislation) == length(date_last_action) & 
      length(legislation) == length(author) & 
      length(legislation) == length(date_last_action) & 
      length(legislation) == length(author) & 
      length(legislation) == length(topic) & 
      length(legislation) == length(leg_summary) & 
      length(legislation) == length(history))
  {
    return.item <- data.frame(legislation = legislation,
                              year = year,
                              legislation_name = name_leg, 
                              status = status,
                              date_last_action = date_last_action,
                              author = author,
                              topic = topic,
                              summary = leg_summary,
                              history = history)  
  } else {
    return.item <- list(legislation = legislation,
                        year = year,
                        legislation_name = name_leg, 
                        status = status,
                        date_last_action = date_last_action,
                        author = author,
                        topic = topic,
                        summary = leg_summary,
                        history = history)
    cat(paste0("\r Check state ", state_flag, "."))
  }
  
  
  return(return.item)
  
}  





#
# Apply functions separately for states with bad data
#

#
# West Virginia
#

# Parse and clean
WV_new_data <- first_half_func(all_states[[48]])

# Manually set state name
state_flag = state.df$name[48] 
state_abbrev = state.df$abb[48]

# Make variables
West_Virginia <- second_half_func(WV_new_data) %>% 
  mutate(stateabb = state.abb[[48]],
         statename = state.name[[48]])



#
# Virginia
#

# Parse and clean
VA_new_data <- first_half_func(all_states[[46]])

# Maually edit
# Last bill is missing a lot of details and throwing an error - remove the bill
VA_new_data <- VA_new_data[-seq(15121,length(VA_new_data), 1)]

# Manually set state name
state_flag = state.df$name[46] 
state_abbrev = state.df$abb[46]

# Make variables
Virginia <- second_half_func(VA_new_data)  %>% 
  mutate(stateabb = state.abb[[46]],
         statename = state.name[[46]])



#
# New York - NY is weird. Have to make a separate function for it.
#

new_data <- trimws(unlist(strsplit(all_states$New_York, split = "\n")))
new_data <- new_data[new_data != ""]
legislation      <- new_data[toupper(substr(new_data, 1, 3)) == paste0("NY", " ")] 
legislation      <- legislation[grepl("state|power", legislation, ignore.case = T) == F]
year             <- new_data[grepl("^[[:digit:]]+$", new_data) & nchar(new_data) == 4] 
name_leg         <- new_data[which(grepl("^[[:digit:]]+$", new_data) & nchar(new_data) == 4) + 1]
status           <- new_data[which(grepl("status:", substr(new_data, 1, 7), ignore.case = T)) + 1]
date_last_action <- new_data[which(grepl("date of last action:", substr(new_data, 1, 20), ignore.case = T)) + 1]
author           <- new_data[which(grepl("author:", substr(new_data, 1, 7), ignore.case = T)) + 1]
topic            <- new_data[which(grepl("topics:", substr(new_data, 1, 7), ignore.case = T)) + 1]
leg_summary      <- c() 

for(i in 1:length(new_data))
{
  if (new_data[i] %in% legislation)
  {
    if(grepl("click for history", new_data[i + 12], ignore.case = F) == F)
    {
      leg_summary[i] <- new_data[i + 12]
    }
  }
}
leg_summary <- leg_summary[!is.na(leg_summary)]
history          <- new_data[which(grepl("history:", substr(new_data, 1, 8), ignore.case = T)) + 1]


if (length(legislation) == length(year) & 
    length(legislation) == length(name_leg) & 
    length(legislation) == length(status) & 
    length(legislation) == length(date_last_action) & 
    length(legislation) == length(author) & 
    length(legislation) == length(date_last_action) & 
    length(legislation) == length(author) & 
    length(legislation) == length(topic) & 
    length(legislation) == length(leg_summary) & 
    length(legislation) == length(history))
{
  ny.data <- data.frame(legislation = legislation,
                        year = year,
                        legislation_name = name_leg, 
                        status = status,
                        date_last_action = date_last_action,
                        author = author,
                        topic = topic,
                        summary = leg_summary,
                        history = history)  
} else {
  return.item <- list(legislation = legislation,
                      year = year,
                      legislation_name = name_leg, 
                      status = status,
                      date_last_action = date_last_action,
                      author = author,
                      topic = topic,
                      summary = leg_summary,
                      history = history)
  cat(paste0("\r Check state ", state_flag, "."))
}


# Extract cleaned df
New_York <- ny.data %>% 
  mutate(stateabb = state.abb[[32]],
         statename = state.name[[32]])







#
# All States - DataFrame - Create
#

all_states_df <- rbind(Alabama, Alaska, Arizona, Arkansas, California, Colorado,
                       Connecticut, Delaware, Florida, Georgia, Idaho,
                       Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine,
                       Maryland, Massachusetts, Michigan, Minnesota, Mississippi,
                       Missouri, Montana, Nebraska, Nevada, New_Hampshire, New_Jersey,
                       New_Mexico, New_York, North_Carolina, North_Dakota, Ohio,
                       Oklahoma, Oregon, Pennsylvania, Rhode_Island, South_Carolina,
                       South_Dakota, Tennessee, Texas, Utah, Vermont, Virginia,
                       Washington, West_Virginia, Wisconsin, Wyoming)

# Not found: Hawaii (problem with Selenium)



# Save data
save(all_states_df, file = "~/Box/Diffusion/Data/NCSL/AllStates_DataFrame.RData")





