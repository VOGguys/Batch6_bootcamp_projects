library(nycflights13)
View(flights)
View(airlines)
View(airports)

## review dataframe structure
glimpse(flights)
glimpse(airlines)
glimpse(airports)

## check missing value

sum(complete.cases(flights)/nrow(flights))*100

## clean data

flights_clean <- drop_na(flights)

flights_clean %>% head(10)

## filter

flights_clean %>%
  group_by(month) %>%
  summarise(n=n())

flights_clean %>%
  filter(month==2) %>% 
  count(carrier) %>%
  arrange(-n) %>%
  head(5) %>%
  left_join(airlines,by="carrier")

## HW01 # 5 Questions about flights dataset

# Questions 1 # what season have more flight 
season_flights <-flights_clean  %>%
  mutate(season = case_when(
    month == 1 ~ "winter",
    month == 2 ~ "winter",
    month == 3 ~ "sprin",
    month == 4 ~ "sprin",
    month == 5 ~ "sprin",
    month == 6 ~ "summer",
    month == 7 ~ "summer",
    month == 8 ~ "summer",
    month == 9 ~ "autumn",
    month == 10 ~ "autumn",
    month == 11 ~ "autumn",
    month == 12 ~ "winter")) %>%
  count(season) %>%
  arrange(desc(n)) 

season_flights

# result: summer season is the most of flight are 84124 flights.

## Questions 2 # what month of summer is most flight

most_month <-flights_clean %>%
  filter(month %in% c(6,7,8)) %>%
  count(month) %>%
  arrange(desc(n))

most_month

# result: In September most of flights are 28756 flights.

## Questions 3 # What top 5 airline have more flight and distance summary stat ?

most_air_dis <-flights_clean %>%
  group_by(carrier) %>%
  summarise(num_flights = n(),
            sum_dist = sum(distance),
            mean_dist = mean(distance),
            sd_dist = sd(distance))%>% 
  inner_join(airlines,by= "carrier") %>%
  arrange(desc(num_flights)) %>%
  head(5)

View(most_air_dis)

# result: United Airlines has most number of flight and distance.

## Questions 4 # Which top 10 airlines had heights number of delayed departures?

flights_clean %>%
  group_by(carrier) %>%
  filter(dep_delay > 0) %>%
  summarize(num_delay= n()) %>%
  mutate(deleyed_ratio = num_delay / n()) %>%
  inner_join(airlines,by="carrier") %>%
  select(airlines_name = name,
         carrier,
         num_delay) %>%
  arrange(desc(num_delay))%>%
  head(10)
  
# result: for Question 3 United airline has most delay time  because them have most flights also that its make them to some flight delay.

## Questions 5 # where airport most of arrive from NYC in 2013 years

top_airport <-flights_clean %>%
  group_by(dest) %>%
  count(n()) %>%
  inner_join(airports,by=c("dest"="faa"))%>%
  select(airports_name = name,
         dest,
         munber_flights = n,
         locations = tzone) %>%
  arrange(desc(munber_flights))

top_airport %>%
  head(5)

# result: the most of airports to have arrived in Hartsfield Jackson Atlanta is a most flight from NYC in 2013.
