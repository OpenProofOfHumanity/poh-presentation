# Setup
library(ghql)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(ggdark)
library(extrafont)
library(httr)
library(readxl)
library(writexl)

# Optional setup for fonts - requires installation of the Fira Sans font
# https://fonts.google.com/specimen/Fira+Sans+Condensed

#font_import() # run once
loadfonts(device = "win") # look for instructions for other OSs

# Attention! **********************************
# Need to load functions at the end of this script 

# Constants
APIsnapshot <- "https://hub.snapshot.org/graphql"
api_url <- "https://api.thegraph.com/subgraphs/name/kleros/proof-of-humanity-mainnet"


# Getting all PoH proposals data from snapshot ----
proposals_all <- getsnapshot_all("https://hub.snapshot.org/graphql")$proposals

# Getting challenge data from Kleros graph ----
challenges.all <- get.challenges.challenger_and_challenged(first = 1000, skip = 0, api_url)

# Plotting challenge reasons over time ----

chall_dates <- challenges.all %>% group_by(reason) %>% 
  mutate(creationDate = floor_date(creationTime, unit = "week"),
         year = year(creationTime)) %>% count(creationDate)

ggplot(chall_dates, aes(x = as.Date(creationDate), y = n, fill = reason)) + 
  geom_bar(stat = "identity") + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") + 
  dark_theme_classic() + 
  ggtitle("Challenges timeline",
          subtitle = "Data updated in Oct 2022") +
  theme(axis.text = element_text(size = 15),
        text=element_text(family="Fira Sans Condensed", face = "bold")) +
  xlab("Date") + ylab("# of weekly challenges") +
  scale_fill_manual(values=c("#63535B",'#53917E',"#6D1A36", '#faba01',"#F72C25"))

# Plotting registrations over time ----
# allsubs <- getRegistry() uncomment and run if you want the latest registry data

allsubs <- read_rds("allsubs.RDS") # cached version

allsubs$profiles.creation_time <- as.POSIXct(allsubs$profiles.creation_time)
timeline <- allsubs %>% 
  group_by(profiles.creation_time) %>%
  count() %>% mutate(cumsum = cumsum(n))

ggplot(timeline, aes(x = profiles.creation_time, y = n)) + 
  geom_line(color = '#F72C25', size = 1) + 
  geom_smooth(color = "#53917E") + 
  dark_theme_bw() + 
  ggtitle("Total submitted profiles",
          subtitle = "Data updated in Oct 2022") +
  theme(axis.text = element_text(size = 15),
        text=element_text(family="Fira Sans Condensed", face = "bold")) +
  xlab("Date") + ylab("total registration submissions") 

# A manually edited version of proposals_all is loaded from an excel file to 
# remove non-hips
proposals_all$start <- as.POSIXct(proposals_all$start, origin="1970-01-01")
proposals_all <- proposals_all %>%  filter(start < "2022-09-24")

write_xlsx(proposals_all, "all proposals out.xlsx")

# re-load the file after classifying non-hips
proposals_manual <- readxl::read_excel("proposalsbytype.xlsx")

proposals_all$type <- proposals_manual$type



# Filtering out HIPs
only_hips <- proposals_all %>% filter(type != "not-hip") 

# Plotting voter participation in HIP voting ----
ggplot(only_hips, aes(x = start, y = scores_total)) + 
  geom_point(color = "#faba01") + 
  geom_smooth(color = "gray") + dark_theme_classic() +
  ggtitle("Voter participation in Proof of Humanity Snapshot",
          subtitle = "Data updated in Oct 2022") +
  theme(axis.text = element_text(size = 20),
        text=element_text(family="Fira Sans Condensed", face = "bold")) +
  xlab("Date") + ylab("Total voting power")

# Gets the amount of choices on the proposal
n_options <- NULL

for (i in seq_along(1:nrow(only_hips))) {
  n_options[i] <- length(only_hips$choices[[i]])  
  
}

# Proposals with the results tabulated (takes a while)
props_yn <- unnest_wider(only_hips, c(choices, scores), names_repair = "unique")
# Adds number of options
props_yn$n_options <- n_options

# Exporting for further manual editions
write_xlsx(props_yn, "hips and votes.xlsx")

# What changes fromo the exported to the edited version???? **********
# HIP 20 signalling had 3 options and could not be coerced to accept reject
# HIP 56 two options were merged into one
# Options were reversed to match columns in the following hips
# HIP 39 binding
# HIP 34 binding

# Import back the edited xlsx

prop_ed <- readxl::read_excel("hips and votes edited.xlsx")

prop_ed <- prop_ed %>% mutate(proportion = accept_count / (accept_count + reject_count))

# Filtering by phase
ppp_f3 <- prop_ed %>% filter(Phase == 3)
ppp_f2 <- prop_ed %>% filter(Phase == 2)


# Optional: a dataframe with voter results ----

ids <- ppp_f3$id
allvotes <- data.frame()

allvotes <- getvotes(APIsnapshot, proposal = ids[1])$votes

for (id in 2:length(ids)) {
  print(ids[id])
  data <- getvotes(APIsnapshot, proposal = ids[id])$votes
  allvotes <- full_join(allvotes, data)
}


allvotes_simple <- allvotes %>% 
  mutate(proposal = proposal$id) %>% 
  select(voter, vp, proposal, choice)

# Plot that matches registrations and UBI price ----

allsubsdaily <-  allsubs %>%
  mutate(
    profiles.creation_time = as.Date(profiles.creation_time),
    thedate = floor_date(profiles.creation_time, unit = "day")) %>%
  count(thedate) %>% 
  mutate(dato = "Daily registrations")


dailyUBIUSD <- get.dailyUBIUSD()
UBIUSD <- dailyUBIUSD %>% rename(n = priceUSD, thedate = date) %>% mutate(dato ="Daily UBI/USB price")

ubi_and_registration <- left_join(UBIUSD,allsubsdaily, by = "thedate") %>% filter(thedate > "2021-08-01")

ylim.prim <- c(0, 0.5)   # in this example, ubi
ylim.sec <- c(0, 206)    # in this example, registrations

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1] # there was a bug here


# Unsmoothed version
ggplot(ubi_and_registration, aes(thedate, n.x)) +
  geom_line(color = '#53917E', size = 0.8) +
  geom_line(aes(y = a + n.y*b), color = '#faba01') +
  scale_y_continuous("Daily UBI/USD", sec.axis = sec_axis(~ (. - a)/b, name = "Daily registrations")) + 
  dark_theme_bw() + 
  ggtitle("Daily UBI price and Registrations",
          subtitle = "Data from August 2021 - present") +
  theme(axis.text = element_text(size = 15),
        text=element_text(family="Fira Sans Condensed", face = "bold")) +
  xlab("Date") + ylab("accumulated registration submissions") +
  scale_fill_manual(values=c("#63535B",'#53917E',"#6D1A36", '#faba01',"#F72C25"))

# Smoothed version
ggplot(ubi_and_registration, aes(thedate, n.x)) +
  geom_smooth(color = '#53917E', size = 0.8, span = .01) +
  geom_smooth(aes(y = a + n.y*b), color = '#faba01', span = .01) +
  scale_y_continuous("Daily UBI/USD", sec.axis = sec_axis(~ (. - a)/b, name = "Daily registrations")) + 
  dark_theme_bw() + 
  ggtitle("Daily UBI price and Registrations",
          subtitle = "Data from August 2021 - present") +
  theme(axis.text = element_text(size = 15),
        text=element_text(family="Fira Sans Condensed", face = "bold")) +
  xlab("Date") + ylab("accumulated registration submissions") +
  scale_fill_manual(values=c("#63535B",'#53917E',"#6D1A36", '#faba01',"#F72C25"))

# Voters support for HIPs ----

# Pass and reject for hips
ppp_f3.simple <- ppp_f3 %>% 
  mutate(proposers = NA) %>% select(HIP, title, proposers, accept_count, reject_count, scores_total)


# Pivot longer 
p3spl <- ppp_f3.simple %>% 
  pivot_longer(cols = 4:5, 
               names_to = "decision", 
               values_to = "n") %>% 
  mutate(decision = as.factor(decision))


# Plotting 
p3spl$decision <- factor(p3spl$decision, levels =c("reject_count","accept_count"))


p3spl %>% filter(scores_total > 400) %>% 
  ggplot(aes(reorder(title, scores_total),
             y = n, 
             fill = decision,
             label = n)) + 
  geom_bar(stat ="identity", position = "fill") + 
  coord_flip()  + 
  dark_theme_bw(base_size = 20) + 
  scale_fill_manual(values=c('purple', '#faba01'),
                    labels=c('Reject', 'Accept')) +
  xlab("") + ylab("Proportion of approval") + 
  geom_abline(slope=0, intercept=0.5,  col = "red",lty=2, size =1) +
  geom_text(size = 6, 
            position = position_fill(vjust = 0.5),
            color = "#63535B")

# Plotting Phase 2 approval by authorship ----
ppp_f2.simple <- ppp_f2 %>% 
  mutate(proposers = NA) %>% select(HIP, title, proposers, accept_count, reject_count, scores_total)

# Exporting to Excel for manual editing
write_xlsx(ppp_f2.simple, 'f2 votes raw.xlsx')

# Importing the excel back
pf2se <- readxl::read_xlsx("f2 votes edited.xlsx") %>% 
  filter(proposers != "not-hip") %>% 
  mutate(proportion_approval = accept_count / scores_total)

pf2se %>% group_by(proposers) %>% 
  summarize(mean_approval_perc = mean(proportion_approval, na.rm = TRUE) %>% 
              round(2)) %>%  
  ggplot(aes(x = proposers, 
             y = mean_approval_perc,
             label = scales::percent(mean_approval_perc))) + 
  geom_bar(stat = "identity", aes(fill = proposers)) +
  dark_theme_bw() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) + 
  scale_fill_manual(values=c('#faba01','purple')) + 
  theme(axis.text = element_text(size = 15),
        text=element_text(family="Fira Sans Condensed", face = "bold"),
        legend.position = "none") +
  xlab("") + ylab("Approval rate in Phase 2") + 
  ggtitle("Mean approval rate by proposal origin") +
  geom_text(size = 6, 
            vjust = 2,
            color = "#4D4047")

# Challenge reasons by category ----
# Importing a manual survey of challenges up to dispute #1000
challenge_reasons_by_cat <- read_rds(file = "challenge reasons by category.RDS")

ggplot(challenge_reasons_by_cat, 
       aes(x = reorder(reason, n), y = n)) +
  geom_bar(stat = "identity",fill = "purple") +
  coord_flip() +
  dark_theme_bw() + 
  ggtitle("Challenge reasons by category",
          subtitle = "Data from a sample of the first 408 challenges") +
  theme(axis.text = element_text(size = 15),
        text=element_text(family="Fira Sans Condensed", face = "bold")) +
  ylab("# of challenges") + xlab("") 


# Load functions before running ----
getsnapshot_all <- function(api_url) {
  myquery <- 'query {
  proposals (
    first: 100,
    skip: 0,
    where: {
      space_in: ["poh.eth"]      
    },
    orderBy: "created",
    orderDirection: desc
  ) {
    id
    title
    body
    choices
    start
    end
    snapshot
    state
    scores
    scores_by_strategy
    scores_total
    scores_updated
    author
    space {
      id
      name
    }
  }
}'
  con <- GraphqlClient$new(url = api_url)
  qry <- Query$new()
  
  qry$query('votes', myquery)
  (x <- con$exec(qry$queries$votes))
  output <- jsonlite::fromJSON(x)$data
  return(output)
  
}

get.challenges.challenger_and_challenged <- function(first,skip, api_url) {
  con <- GraphqlClient$new(url = api_url)
  myquery <- paste0("{
  challenges(where: {disputeID_not: null}
  first:",
                    first,
                    ", skip:",
                    skip," orderBy:disputeID,
             orderDirection:desc) {
    disputeID
    reason
    challenger
    requester
    ruling
    creationTime
  }
}")
  
  
  qry <- Query$new() 
  qry$query('challenges', myquery)
  
  x <- con$exec(qry$queries$challenges)
  y <- jsonlite::fromJSON(x)$data$challenges
  output <- y %>% 
    mutate(challenged = requester,
           challengedURL = paste0("https://proof-of-humanity.netlify.app/profile/",
                                  requester)) %>%
    select(disputeID, reason, challenger, challenged, creationTime, ruling,challengedURL) %>%
    mutate(disputeID = as.numeric(disputeID),
           creationTime = as_datetime(as.numeric(creationTime)))
  
  return(output)
}

getRegistry <- function(writexlsx = FALSE) {
  registry <- data.frame()
  myurl <- "https://api.poh.dev/profiles?order_by=creation_time&order_direction=asc&include_unregistered=true"
  
  r <- GET(url = myurl)
  message("Getting ", myurl)
  s <- content(r, as = "text", encoding = "UTF-8")
  t <- as.data.frame(fromJSON(s,flatten = TRUE))
  registry <-  bind_rows(registry, t)
  
  while(fromJSON(s,flatten = TRUE)$meta$has_more){
    r <- GET(url = myurl)
    message("Getting ", myurl)
    s <- content(r, as = "text", encoding = "UTF-8")
    t <- as.data.frame(fromJSON(s,flatten = TRUE))
    registry <-  bind_rows(registry, t)
    myurl <- fromJSON(s,flatten = TRUE)$meta$next_url
    Sys.sleep(0.01)
  }
  
  return(registry)
}

getvotes <- function(api_url, proposal) {
  myquery <- paste0("{
  votes(first: 1000 where: {proposal: \"",proposal,"\"}) {
    id
    voter
    vp
    vp_by_strategy
    vp_state
    created
    proposal {
      id
    }
    choice

  }
}")
  con <- GraphqlClient$new(url = api_url)
  qry <- Query$new()
  
  qry$query('votes', myquery)
  (x <- con$exec(qry$queries$votes))
  output <- jsonlite::fromJSON(x)$data
  return(output)
  
}

get.dailyUBIUSD <- function() {
  url_api <-
    "https://api.thegraph.com/subgraphs/name/uniswap/uniswap-v2"
  
  con <- GraphqlClient$new(url = url_api)
  
  dailyUBIUSD <- data.frame()
  
  qry <- Query$new()
  theQuery <- paste0(
    '
    {
    tokenDayDatas(first:1000,skip:0,where: {token: "0xdd1ad9a21ce722c151a836373babe42c868ce9a4"} ){
    date
    priceUSD
      }
    }
    '
  )
  
  qry$query('tokenDayDatas', theQuery)
  
  (x <- con$exec(qry$queries$tokenDayDatas))
  pre_transform <- jsonlite::fromJSON(x)$data$tokenDayDatas
  dailyUBIUSD <- rbind(dailyUBIUSD,
                       pre_transform %>%
                         mutate(
                           date = as.POSIXct(as.numeric(date), origin = "1970-01-01", tz = "UTC"),
                           priceUSD = as.numeric(priceUSD)
                         ))
  Sys.sleep(0.1)
  
  return(dailyUBIUSD)
}
