
rm(list = ls())

library(tidyverse)
library(stringr)

ncsl_original <- readRDS("~/Box/Diffusion/Data/NCSL/Bill_Data_Formatted.RData")
party <- readxl::read_xlsx(path = "~/Box/GitHub/state_politics/Data/State_Party_Composition.xlsx")


# Merge party data
ncsl <- left_join(ncsl_original, party, by = c("state_name" = "State",
                                            "year" = "Year"))

# Reorder for convenience
ncsl <- ncsl %>% 
  select(state_name, state_abb, year, status, survived, Legis_Control, 
         Gov_Party, State_Control, everything())


# Make new vars - House Control and Senate Control
ncsl <- ncsl %>% 
  mutate(House_Control = ifelse(House_Dem > House_Rep, "Dem",
                                ifelse(House_Rep > House_Dem, "Rep",
                                       ifelse(House_Dem == House_Rep, "Hung",
                                              NA))),
         Senate_Control = ifelse(Senate_Dem > Senate_Rep, "Dem",
                                ifelse(Senate_Rep > Senate_Dem, "Rep",
                                       ifelse(Senate_Dem == Senate_Rep, "Hung",
                                              NA))))

#
# Did bills that die, die in Senate committee or House committee?
#

# Create mutate_when() function
mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}

# Save copy of original
ncsl_original2 <- ncsl

# Apply mutate_when function
ncsl <- ncsl %>% 
  mutate_when(
    (survived == 0 & grepl(".*Senate.*", status_details, ignore.case = TRUE) == TRUE), # survived = 0 and "senate" in status_details
    list(died_in = "Senate"),
    (survived == 0 & grepl(".*House*", status_details, ignore.case = TRUE) == TRUE), # survived = 0 and "house" in status_details
    list(died_in = "House")
    )



#
# Create factor for bi-partisan coalition
#

# Total authors - create
ncsl <- ncsl %>% 
  mutate()

# "Bipartisan (1) if bill is 20% sponsored by both parties

ncsl <- ncsl %>% 
  mutate(bipart_sponsor = ifelse(repb_authors => , 1,
                            ifelse(dem_authors == 0, "Rep",
                                   ifelse(dem_authors > 0 & repb_authors > 0, "Bi",
                                          NA))))


# What percentage of bills are bipartisan?
library(ggplot2)
ggplot(ncsl) +
  geom_bar(aes(x = coalition))

# Summarize data - Coalition by Year
coalition_year <- ncsl %>% 
  group_by(year, coalition) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

# Plot
ggplot(coalition_year, aes(x = factor(year), y = perc*100, fill = factor(coalition))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Year", y = "percent", fill = "Coalition", title = "Sponsoring coalition") +
  theme_minimal(base_size = 14)

# Summarize data - Coalition by State
coalition_state <- ncsl %>% 
  group_by(state_name, coalition) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

# Plot
ggplot(coalition_state, aes(x = factor(state_name), y = perc*100, fill = factor(coalition))) +
  geom_bar(stat="identity", width = .6) +
  labs(x = "Year", y = "percent", fill = "Coalition", title = "Sponsoring coalition") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x=element_text(angle=45, hjust=.6))



# What percentage of bills that die in committee are bipartisan?

# Summarize data - die in
coalition_died_in <- ncsl %>% 
  group_by(died_in, coalition) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))

# Plot
ggplot(coalition_died_in, aes(x = factor(died_in), y = perc*100, fill = factor(coalition))) +
  geom_bar(stat="identity", width = .6) +
  labs(x = "Year", y = "percent", fill = "Coalition", title = "Sponsoring coalition") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x=element_text(angle=45, hjust=.6))


# Are these statistically significant?



library(survival)
nscl2 <- ncsl %>% 
  mutate(bill_survival = ifelse(status = "Adopted" | status = "Enacted", "Adopted",
                                ifelse(status = Fa))
         
# Flag = bill death (0/1)
           
           
           
           
    factor(levels = 0, 1, 2, 3),
         labels = c("proposed", "floor", "pass_floor", "pass"))



# ###########################
# # State-level Variables
# ###########################
# 
# # Load Correlates of State Policy dataset (from IPPSR at Michigan State)
# 
# ippsr <- read_csv("~/Box/Diffusion/Data/IPPSR/correlatesofstatepolicy.csv")
# 
# 
# #
# # Remove irrelevant variables - setup
# #
# 
# # Keep: Demographics/population; Econ/fiscal policy; Gov; Elections; Public opinion
#       # and policy scores
# 
# # Remove: Criminal...(Education; Healthcare; Welfare)...Discrimination
# first_crime_var <- which(names(ippsr) == "amber")
# last_discrim_var <- which(names(ippsr) == "w_race_disc_public_accom2")
# 
# # Keep: Environment
# 
# # Remove: Drug...Gun Control
# first_drug_var <- which(names(ippsr) == "comsrvdy")
# last_gun_var <- which(names(ippsr) == "bwait")
# 
# # Keep: Labor...(Transportation)...Regulatory policy
# 
# 
# # Remove irrelevant variables - Apply
# ippsr_reduced <- 
#   ippsr[ ,-c(
#     (first_crime_var : last_discrim_var), 
#     (first_drug_var : last_gun_var)
#     )]
# 
# 
# 
# ##############################
# # Select variables for text analysis of the summaries
# #############################
# 
# 
# state_vars <- ippsr_reduced %>% select(st, stateno, state, state_fips, poptotal,
#                          pctwhite, year, evangelical_pop, utility_revenue,
#                          gini_coef, region, taxes, pctwomenearn, # pct women earn compared to men
#                          unemployment, asevt, # revenues from severace taxes
#                          termlim, # did state adopt leg. term limits (0/1)
#                          prez_election_year, govname1, gub_election,
#                          gub_election_regime, # year of next gub election
#                          new_gov_b, # if new (non-incumbent) gov into office
#                          term_length, # of governors
#                          years_left_in_term, # of governor
#                          years_left_before_limit, # of governor
#                          femgov, # Fem governor
#                          govparty_a, 
#                          years_since_other_party, # since gub changed party
#                          sen_elections_this_year, sen_dem_in_sess, sen_rep_in_sess,
#                          sen_dem_prop_all, sen_rep_prop_all,
#                          sen_cont_alt, # ******* Party in contol of Sen. (1 Dem, 0.5 split) ******
#                          hs_cont_alt, # ***** Party in control of House ****
#                          leg_cont, # Dem's power of legislatur
#                          split_leg, # ******* Is there a split leg.?
#                          divided_gov, # Is there divided gov (0 = same party controls all 3 branches)
#                          # Ideological medians (-1 liberal)
#                          hou_chamber, sen_chamber, hou_rep, hou_dem,
#                          sen_dem, sen_rep,
#                          legprofscore # leg professionalism ('79, '96, '03)
#                           )
# 
# merged_set <- left_join(ncsl, state_vars, by = c("state_name" = "state", 
#                                                  "year" = "year"))


########################################################################################
########################################################################################
# NLP
# Relationship between party sponsorship and STM and passage conditional on control of the leg.


library(quanteda)
# library(readtext) # To read in diff types of text data
# library(quanteda.textmodels) # Used for scaling and classification
library(spacyr) # For part-of-speech tagging & entity recognition
library(newsmap) # To classify documents based on "seed words" in dictionaries

# quanteda_options("threads" = 10) # Can change how many threads are used for parralelised functions
  # Default is 2, but if you have enough RAM, increasing the threads massive reduced the time

#
# Prepare dataframe
#

  # Change summary variable to character
ncsl <- ncsl %>% mutate(summary = as.character(summary),
  # make a unique doc ID for each document
                        docID = paste(seq(1:nrow(ncsl)), year, bill, sep = "_"))

#
# Create corpus
#
bill_corp <- corpus(ncsl,
                    docid_field = "docID",
                    text_field = "summary",
                    )

#
# Corpus Subsets - Create
#
    
  # Climate Change
climate_corp <- corpus_subset(bill_corp, str_detect(topic_1, "Climate Change"))

  # Renewable Energy
renew_corp <- corpus_subset(bill_corp, str_detect(topic_1, "Renewable Energy"))

  # Fossil Energy
fossil_corp <- corpus_subset(bill_corp, str_detect(topic_1, "Fossil Energy|Hydraulic Fracturing"))

  # List for easier manipulation
corp_list <- list(climate = climate_corp,
                  renew = renew_corp,
                  fossil = fossil_corp)

#
# Tokens - Construct
#

toks_climate <- tokens(climate_corp, remove_punct = TRUE)
toks_renew <- tokens(renew_corp, remove_punct = TRUE)
toks_fossil <- tokens(fossil_corp, remove_punct = TRUE)
# Put all in a list for easier manipulation
toks_list <- list(climate = toks_climate,
                  renew = toks_renew,
                  fossil = toks_fossil)


# # Key word in context (kwic) - See how keywords are used
# kw_congress <- kwic(toks_climate, pattern = "congress")
# kwic(toks_climate, pattern = "support*")
# kwic(toks_climate, pattern = "greenhouse")
# kwic(toks_fossil, pattern = c("emission*", "reduc*")) # Multiple key words
# kwic(toks_fossil, pattern = phrase("United States")) # Multi-word expressions


#
# Remove stopwords
#

# Save unedited version
toks_with_stopwords <- toks_list

# Remove stopwords and numbers
toks_list <- lapply(toks_with_stopwords, tokens_select, 
                           pattern = c(stopwords(), "\\d"),
                           valuetype = "regex",
                           selection = "remove", 
                           padding = TRUE) # padding keeps the original positions - so can do collocation analysis
                                          # (Collocation = words that oc-occur more often than expected by change (i.e. phrases))




########################################
# Compound multi-word expressions
########################################


# Compounds Function - Create
# (Select 2-3 word compounds above a certain threshold (z > 3))

compound_func <- function(toks){
  
    # DataFrame of accepted compounds - Create
  accepted_compounds <- 
    textstat_collocations(toks, 
                          size = c(2:3), # 2-3 words
                          min_count = 10, # min of 10 instances
                          tolower = FALSE) %>% # required
    filter(z > 3) %>% # very low probability of occuring by chance
    select(collocation) # just the phrases
  
    # Use dictionary of accepted compounds to update the corpus
  toks_with_compounds <- 
    tokens_compound(toks, 
                    dictionary(list(comps = accepted_compounds$collocation)), # Must be in list form
                    join = TRUE)
  
  return(toks_with_compounds)
  
  } # End compound_func





# Compound function - Apply

# Retain copy of old toks_list
toks_list_without_compounds <- toks_list

# Toks list with compounds
toks_list <- lapply(toks_list, compound_func)




###################################################
# Document-Feature Matrix
###################################################

# Need to stem-words**

renew_punc_df <- kwic(toks_with_stopwords$renew, "[:punct:]", valuetype = "regex")
renew_punc <- renew_punc_df$keyword[1:1000]
renew_punc_tok <- renew_punc %>% tokens()
renew_stemmed <- renew_punc_tok %>% tokens_wordstem()

samp <- sample(renew_stemmed)
renew_punc_tok[[which(samp)]]


dfm_list <- lapply(toks_list, dfm)

print(dfm_list$fossil)

dfm()




# Look for OH HB 6 (2019)
ncsl <- ncsl %>% mutate(state_name = as.factor(state_name),
                        state_abb = as.factor(state_abb),
                        bill = as.character(bill))

ncsl %>% filter(year == 2019 & state_abb == "OH" & bill == "OH H 6")




names(ncsl)
range(ncsl$repb_authors)
range(ncsl$dem_authors)
bipart <- filter(ncsl, repb_authors > 0 & dem_authors > 0)
table(bipart$topic_1)
bi_clim <- bipart %>% filter(topic_1 %in% c("Climate Change", "Climate Change - Carbon Capture and Sequestration", 
                                            "Climate Change - Emissions Reduction", "Financing Energy Efficiency and Renewable Energy", 
                                            "Green Jobs", "Renewable Energy", "Renewable Energy - Solar", "Renewable Energy - Wind"))

# Change bills pending older than 2018 to failed
bi_clim <- bi_clim %>% mutate(status = ifelse((status == "Pending" & year > 2018), "Failed", status))
bi_clim <- bi_clim %>% mutate(status = ifelse((status == "To Governor" & year > 2018), "Failed", 
                                              ifelse(status == "Vetoed", "Failed", ifelse(status == "Enacted", "Adopted", status))))
  # Feed into ggplot
ggplot(data = bi_clim) +
  geom_bar(aes(status))


ggplot(bi_clim) +
  geom_jitter(aes(x = dem_authors, y = repb_authors, color = status), alpha = 0.3)

bi_clim %>% select(state_abb, year, bill_title, dem_authors, repb_authors) %>% 
  sample_n(5)
 photo

 dim(ncsl)


