library(dataRetrieval)
library(smwrBase)
library(tidyverse)
library(lubridate)
library(patchwork)

### Function for plotting years
same_year <- function(x) {
  year(x) <- 2000
  x
}

### Gather and prepare USGS hydro station data
# siteNo <- "12419000" Spokane River at Post Falls

pCode <- "00060" # river discharge Q
start.date <- "2019-01-01"
end.date <- "2021-04-30"

# Get daily Q
hydro.sjr <- readNWISdv(siteNumbers = "12415135", # SJR a Ramsdell 
                    parameterCd = pCode, 
                    startDate = start.date, 
                    endDate = end.date) %>% 
  renameNWISColumns() %>% 
  mutate(year=year(Date))

hydro.cda <- readNWISdv(siteNumbers = "12413860", # CDAR at Harrison 
                        parameterCd = pCode, 
                        startDate = start.date, 
                        endDate = end.date) %>% 
  renameNWISColumns() %>% 
  mutate(year = year(Date))

# combine
hydro <- bind_rows("sjr@ramsdell" = hydro.sjr, 
                   "cdar@harrison" = hydro.cda, 
                   .id = "site")

# Plot hydro
p.cfs <- ggplot(hydro, aes(x = same_year(Date), y = Flow, 
                  color = as.factor(year),
                  linetype = as.factor(year))) +
  geom_path() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = NULL, y = "Discharge CFS") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8),
                     labels = scales::comma_format()) +
  facet_grid(rows = vars(site)) +
  scale_color_discrete(name = "Year") +
  scale_linetype_discrete(name = "Year")

# Get CDA lake level
lklv <- readNWISdata(service = "dv", 
                     sites = "12415500", 
                     parameterCd = "00065", # lake level gauge height
                     startDate = start.date, 
                     endDate = end.date) %>% 
  renameNWISColumns(.) %>% 
  mutate(lklv = GH_32400 + 2100, #Avista Vert. Datum
         date = date(dateTime))

# Plot lake level
p.lklv <- ggplot(lklv, aes(x = same_year(date), y = lklv, 
                 color = as.factor(year(date)),
                 linetype = as.factor(year(date)))) +
  geom_path() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = NULL, y = "CDA Lake Level ft ASL") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_color_discrete(name = "Year") +
  scale_linetype_discrete(name = "Year")

# Plot combined figure
p.cfs/p.lklv
