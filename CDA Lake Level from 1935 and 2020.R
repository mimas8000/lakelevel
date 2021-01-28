library(dataRetrieval)
library(tidyverse)
library(lubridate)

### Function for plotting years
same_year <- function(x) {
  year(x) <- 2000
  x
}

### Get Lake Level Data
CDALk_siteNo <- "12415500" # CDA Lake gauge
pCode <- "00065" ### Gage Height

start.date35 <- "1933-01-01"
end.date35 <- "1935-12-31"

start.date20 <- "2020-01-01"
end.date20 <- "2020-12-31"


CDALk35 <- readNWISdata(siteNumbers = CDALk_siteNo, 
                      parameterCd = pCode, 
                      startDate = start.date35,
                      endDate = end.date35, 
                      service = "dv") %>% 
  renameNWISColumns() %>% 
  tibble() %>% 
  mutate(Elev = GH_32400 + 2100,
         Year = as.factor(year(dateTime)))

CDALk20 <- readNWISdata(siteNumbers = CDALk_siteNo, 
                        parameterCd = pCode, 
                        startDate = start.date20,
                        endDate = end.date20, 
                        service = "dv") %>% 
  renameNWISColumns() %>% 
  tibble() %>% 
  mutate(Elev = GH_32400 + 2100,
         Year = as.factor(year(dateTime)))

CDALk <- bind_rows(CDALk35, CDALk20)

### Plot Time Series of Lake Level
ggplot(CDALk, aes(x=same_year(dateTime), y=Elev, color=Year)) + 
  geom_line() + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  labs(x = NULL, y = "CDA Lake Elevation ft AMSL Avista vert. datum") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))

### Get SJR Level Data
SJR_siteNo <- "12415070" # SJR gauge at St Maries, ID

sjr35 <- readNWISdata(siteNumbers = SJR_siteNo, 
                    parameterCd = pCode, 
                    startDate = start.date35,
                    endDate = end.date35, 
                    service = "dv") %>% 
  renameNWISColumns() %>% 
  tibble() %>% 
  mutate(Elev = GH + 2142,
         Year = as.factor(year(dateTime)))

sjr20 <- readNWISdata(siteNumbers = SJR_siteNo, 
                    parameterCd = pCode, 
                    startDate = start.date20,
                    endDate = end.date20, 
                    service = "dv") %>% 
  renameNWISColumns() %>% 
  tibble() %>% 
  mutate(Elev = GH + 2141.2,
         Year = as.factor(year(dateTime)))

sjr <- bind_rows(sjr35, sjr20)

### Plot Time Series of SJR Level
ggplot(sjr, aes(x=same_year(dateTime), y=Elev, color=Year)) + 
  geom_line() + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  labs(x = NULL, y = "SJR Elevation ft AMSL Avista vert. datum") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n=10))

### combine CDA Lk and SJR
CDALk <- CDALk %>% 
  rename(GH = GH_32400,
         GH_cd = GH_32400_cd)

GH <- bind_rows("CDALk" = CDALk, 
                "SJR" = sjr,
                .id = "Site")

# Plot both sites together
ggplot(GH, aes(dateTime, Elev, color = Site, linetype = Site)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")

GH2 <- left_join(CDALk, 
                 sjr, 
                 by = "dateTime",
                 suffix = c(".CDALk", ".SJR"))

cor.GH2 <- cor.test(GH2$Elev.CDALk, GH2$Elev.SJR)

ggplot(GH2, aes(Elev.CDALk, Elev.SJR)) +
  geom_smooth(method = lm) +
  geom_point(aes(color = factor(year(dateTime))), alpha = 0.2) +
  labs(x = "CDA Lake Elev Avista VD",
       y = "SJR Elev Avista VD",
       color = "Year") +
  annotate("text", 
           label = paste("R =", round(cor.GH2$estimate, 2)), 
           x = 2123, 
           y = 2180) +
  facet_wrap(vars(factor(year(dateTime))))



#  
