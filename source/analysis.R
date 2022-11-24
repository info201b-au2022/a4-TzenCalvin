# Makes ggplot axis not use scientific notation.
options(scipen = 999)
# Allows addition of commas in ggplot axis.
library(scales)
library(tidyverse)
# Stops displaying an error message that doesn't break the program.
options(dplyr.summarise.inform = FALSE)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- Data Summary
#----------------------------------------------------------------------------#
# This returns the incarceration data as a dataframe and retrieves the data
# locally from my machine in order to speed up the process.
incarcerations <- get_data()

# This creates a sub-dataframe that will calculate the proportion of jail 
# population compared to total population for black and white people.
black_white_incarcerations_2018 <- incarcerations %>%
  filter(year == 2018) %>%
  summarize(
    total_black_population = sum(black_pop_15to64, na.rm = TRUE), 
    total_white_population = sum(white_pop_15to64, na.rm = TRUE),
    total_black_jail = sum(black_jail_pop, na.rm = TRUE),
    total_white_jail = sum(white_jail_pop, na.rm = TRUE),
    black_prop_jail = round(total_black_jail/total_black_population, 5),
    white_prop_jail = round(total_white_jail/total_white_population, 5)
    )

# This creates as sub-dataframe that will calculate the average jail population 
# rate of black and white people.
pop_growth_rates <- incarcerations %>%
  summarize(
    avg_black_pop_rate = round(mean(black_jail_pop_rate, na.rm = TRUE), 1),
    avg_white_pop_rate = round(mean(white_jail_pop_rate, na.rm = TRUE), 1)
  )

# This creates as sub-dataframe that will calculate the highest yearly average 
# jail population rate of black people and what year it occurred in.
most_black_pop_rate <- incarcerations %>%
  group_by(year) %>%
  summarize(
    avg_black_pop_rate = round(mean(black_jail_pop_rate, na.rm = TRUE), 2)
  ) %>%
  na.omit() %>%
  filter(avg_black_pop_rate == max(avg_black_pop_rate))

# Returns the proportion of jail population compared to total population for 
# black people
black_prop_jail <- black_white_incarcerations_2018 %>%
  pull(black_prop_jail)

# Returns the proportion of jail population compared to total population for 
# white people
white_prop_jail <- black_white_incarcerations_2018 %>%
  pull(white_prop_jail)

# Returns the average jail population rate of black people.
avg_black_pop_rate <- pop_growth_rates %>%
  pull(avg_black_pop_rate)

# Returns the average jail population rate of white people.
avg_white_pop_rate <- pop_growth_rates %>%
  pull(avg_white_pop_rate)

# Returns which year had the highest yearly average jail population rate of 
# black people.
most_black_rate_year <- most_black_pop_rate %>%
  pull(year)

# Returns the highest yearly average jail population rate of black people.
most_black_rate <- most_black_pop_rate %>%
  pull(avg_black_pop_rate)
  
## Section 3  ---- Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function creates a dataframe with two columns: one that contains the year, 
# and the other containing the total jail population of that year.
get_year_jail_pop <- function(){
  graph_data <- incarcerations %>%
    group_by(year) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  
  return(graph_data)   
}

# This function uses the dataframe we created earlier and constructs a barchart 
# displaying the data.
plot_jail_pop_for_us <- function(){
  graph_data = get_year_jail_pop()
  
  chart <- ggplot(graph_data) +
    geom_bar(
      mapping = aes(y = total_jail_pop, x = year),
      stat = "identity"
    ) +    
    labs(
      title = "Increase of Jail Population in US (1970-2018)", 
      caption = "A bar graph of jail population in the US from 1970 to 2018, displaying 
      an increase over the years."
      ) +
    xlab("Year") +
    ylab("Total Jail Population") +
    scale_y_continuous(label = comma)
    
  return(chart)   
} 

## Section 4  ---- Growth of Prison Population by State 
#----------------------------------------------------------------------------#
# This function creates a dataframe with three columns: one that contains the 
# year, one that contains a state, and the last containing the total jail 
# population of the state in the given year.
get_jail_pop_by_states <- function(states){
  graph_data <- incarcerations %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  
  return(graph_data)   
}

# This function uses the dataframe we created earlier and constructs a line chart 
# displaying the data, with a different color for each state.
plot_jail_pop_by_states <- function(states){
  graph_data = get_jail_pop_by_states(states)
  
  chart <- ggplot(graph_data) +
    geom_line(
      mapping = aes(y = total_jail_pop, x = year, color = state),
    ) +    
    labs(
      title = "Increase of Jail Population in US (1970-2018) for Given States",
      caption = "A line graph of jail population in the US from 1970 to 2018, displaying 
      an increase over the years. Each state is represented by a different color line."
    ) +
    xlab("Year") +
    ylab("Total Jail Population") +
    scale_y_continuous(label = comma)
  
  return(chart)
}

plot_jail_pop_by_states("DC")

## Section 5  ---- <variable comparison that reveals potential patterns of inequality>
#----------------------------------------------------------------------------#
# This function creates a dataframe with three columns: one that contains the 
# year, one that contains a race, and the last containing the proportion of that 
# race's population in jail for that year.
get_race_prop <- function(){
  graph_data <- incarcerations %>%
    group_by(year) %>%
    filter(black_pop_15to64 != 0) %>%
    summarize(
      total_black_population = sum(black_pop_15to64, na.rm = TRUE), 
      total_white_population = sum(white_pop_15to64, na.rm = TRUE),
      total_black_jail = sum(black_jail_pop, na.rm = TRUE),
      total_white_jail = sum(white_jail_pop, na.rm = TRUE),
      Black = round(total_black_jail/total_black_population, 5),
      White = round(total_white_jail/total_white_population, 5)
    ) %>%
    select(year, Black, White) %>%
    gather(
      key = "race",
      value = "proportion",
      -year
    )
  
  return(graph_data)
}

# This function uses the dataframe we created earlier and constructs a line chart 
# displaying the data, with a different color for each race.
plot_race_prop <- function(){
  graph_data = get_race_prop()
  
  chart <- ggplot(graph_data) +
    geom_line(
      mapping = aes(y = proportion, x = year, color = race),
    ) +    
    labs(
      title = "Proportion of Total Population Incarcerated (1990-2018)",
      caption = "A line graph of the proportion of the total population of a race that 
      was incarcerated in the US from 1990 to 2018, displaying how a much bigger 
      proportion of the black population was incarcerated every year compared to 
      the proportion of white population that was incacerated."
    ) +
    xlab("Year") +
    ylab("Proportion of Jail Population Compared to Total Population")
  
  return(chart)
}

## Section 6  ---- <a map shows potential patterns of inequality that vary geographically>
#----------------------------------------------------------------------------#
# This function creates a dataframe with that joins two dataframes: the states one 
# used to graph the map, and a data set containing proportion of black people imprisoned 
# for every state. 
get_geog_black_prop <- function(){
  graph_data <- incarcerations %>%
    filter(year == 2018) %>%
    group_by(state) %>%
    filter(black_pop_15to64 != 0) %>%
    summarize(
      total_black_population = sum(black_pop_15to64, na.rm = TRUE), 
      total_black_jail = sum(black_jail_pop, na.rm = TRUE),
      black_proportion = round(total_black_jail/total_black_population, 5),
    ) %>%
    select(state, black_proportion)
  
  graph_data$state <- state.name[match(graph_data$state, state.abb)]
  graph_data <- graph_data %>%
    mutate(state = replace(state,is.na(state), "District of Columbia")) %>%
    mutate(state = tolower(state))
  
  state_shape <- map_data("state") %>%
    rename(state = region) %>% 
    left_join(graph_data, by = "state")
    
  return(state_shape)   
}

# This function creates a choropleth map that showcases the proportion of black 
# people imprisoned for every state. The brighter the state, the higher the proportion
# is.
plot_geog_black_prop <- function(){
  graph_data = get_geog_black_prop()
  
  blank_theme <- theme_bw() +
    theme(
      axis.line = element_blank(),        
      axis.text = element_blank(),        
      axis.ticks = element_blank(),       
      axis.title = element_blank(),       
      plot.background = element_blank(),  
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank()     
    )
  
  chart <- ggplot(graph_data) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = black_proportion),
      color = "white",
      size = .1 
    ) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Red") +
    labs(
      fill = "Proportion",
      title = "Proportion of Black People Incarcerated in 2018",
      caption = "A choropleth map of the proportion of the total population of 
      black people that were incarcerated in each US state in 2018. States with higher 
      white population seem to have higher prorportion of black people incarcerated."
      ) + 
    blank_theme
  
  return(chart)
}
