
## libraries

library(downloader) # makes downloading from https simple
library(readxl) # imparting excel files
library(tidyr) #
library(reshape2)
library(ggplot2)
library(gganimate)


## download and import data

download("https://freedomhouse.org/sites/default/files/FOTP2015%20Detailed%20Data%20and%20Subscores%201980-2015.xlsx", dest="fh_raw.xlsx", mode = "wb") 

# read in the contents of the "Global" sheet
fh_raw <- read_excel("fh_raw.xlsx", sheet = "Global", na = "N/A", skip = 4) %>%
  .[c(1:210), c(1,103:172)] %>%
  data.frame() 

# add a variable name  for the first column
names(fh_raw)[1] <- "country"


## tidying the data

fh_clean <- melt(fh_raw, id.vars = c("country")) %>% 
  # add variables for the year collected and the year reported
  mutate(year.collected = rep(c(2001:2014), each = 5*210),
         year.reported = year.collected + 1) %>%
  rename(metric = variable, result = value) %>% 
  # delete the extra info from the variable names using gsub and use tolower so our variable names are standardized
  mutate(metric = tolower(gsub("\\..*", "", metric))) %>%
  # and finally spread it back out a bit so we have the subscores, final score and rating for each year-country combo
  spread(metric, result) %>%
  # use mutate_each to convert the scores to numeric
  mutate_each(funs(as.numeric), a, b, c, score)


## create a quick gif showing the change in distribution from 2001 to 2014

p <- ggplot(fh_clean, aes(x = score, frame = factor(year.collected))) +
  geom_density(fill = "#445869", color = "white", size = 1) +
  labs(y = "",
       x = "Freedom of the Press Score\nHigher Score = Greater Repression") +
  theme(plot.title = element_text(size = 24, hjust = 1, face = "bold"), 
        axis.title = element_text(size = 18, face = "bold"), 
        axis.text = element_text(size = 14)) 

gg_animate(p, "fotp_2001_2014.gif", interval = .3)
