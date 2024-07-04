
# File --------------------------------------------------------------------

UNVotes <- "C:\\Users\\abbas\\Downloads\\UNVotes.rda"
load(file = UNVotes)

votes <- completeVotes

# Packages ----------------------------------------------------------------

# Load required packages
library(dplyr)
library(broom)
library(tidyr)
library(countrycode)
library(ggplot2)
library(purrr)
library(cluster)
library(qgraph)
library(igraph)

# Filter votes ------------------------------------------------------------

# Filter for votes that are "yes", "abstain", or "no"
votes %>%
  filter(vote <= 3)

# Add another %>% step to add a year column
votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945)

# Convert country code 100
countrycode(100, "cown", "country.name")

# Add a country column within the mutate: votes_processed
votes_processed <- votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945, country = countrycode(ccode, "cown", "country.name"))

votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# The votes summarized by country
by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Filter out countries with fewer than 100 votes
by_country %>%
  arrange(percent_yes) %>%
  filter(total >= 100)

# Define by_year
by_year <- votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Global Average of 'Yes' Votes -------------------------------------------

# Determine the maximum year in the dataset
max_year <- max(by_year$year)

# Create line plot
ggplot(by_year, aes(x = year, y = percent_yes)) +
  geom_line(color = "blue") +  # Adjust line color
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Convert y-axis labels to percentages
  labs(title = "Global Average of 'Yes' Votes",  # Add title
       x = "Year",  # Add x-axis label
       y = "Percent of 'Yes'") +  # Add y-axis label
  theme_minimal() +  # Apply a minimal theme
  scale_x_continuous(labels = scales::number_format()) +  # Convert x-axis labels to integers
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black") +  # Add vertical dashed line at year 2014
  annotate("text", x = 2014, y = -0.05, label = "2014", vjust = -0.5, hjust = -0.5)  # Annotate the year 2014 on the plot


# 'Yes' Votes of 5 Permanent UNSC Members ---------------------------------

# Create by_year_country dataset
by_year_country <- votes_processed %>%
  group_by(year, country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Vector of five countries to examine
countries <- c("United States", "United Kingdom",
               "France", "Russia", "China")

# Filter by_year_country: filtered_5_countries
filtered_5_countries <- by_year_country %>%
  filter(country %in% countries)

# Define the maximum year in the dataset
max_year <- max(filtered_5_countries$year)

# Create line plot
ggplot(filtered_5_countries, aes(x = year, y = percent_yes, color = country)) +
  geom_line() +
  labs(title = "'Yes' Votes of 5 Permanent UNSC Members",  # Add title
       x = "Year",  # Add x-axis label
       y = "Percent of 'Yes'") +  # Add y-axis label
  scale_x_continuous(labels = scales::number_format()) +  # Convert x-axis labels to integers
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Convert y-axis labels to percentages
  theme_minimal() +  # Apply a minimal theme
  theme(legend.position = "bottom") +  # Adjust legend position
  geom_vline(xintercept = 2014, linetype = "dashed", color = "black") +  # Add vertical dashed line at year 2014
  annotate("text", x = 2015, y = -0.05, label = "2014", vjust = 0, hjust = 0)  # Annotate the year 2014 on the plot

# Nest all columns --------------------------------------------------------

# Nest all columns besides country
by_year_country %>%
  nest(-country)

# All countries are nested besides country
nested <- by_year_country %>%
  nest(-country)

# Unnest the data column to return it to its original form
nested %>%
  unnest(data)

# Linear regression -------------------------------------------------------

# Perform a linear regression on each item in the data column
by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)))

# Add another mutate that applies tidy() to each model
by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .))) %>%
  mutate(tidied = map(model, tidy))

# Add one more step that unnests the tidied column
country_coefficients <- by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
         tidied = map(model, tidy)) %>%
  unnest(tidied)

# Filter for only the slope terms
slope_terms <- country_coefficients %>%
  filter(term == "year")

# Add p.adjusted column, then filter
slope_terms %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < 0.05)


# Distances ---------------------------------------------------------------

# Replace "Yes" with 1, "No" with -1, and "Abstain" with 0 in the "vote" column
votes_processed <- votes_processed %>%
  mutate(vote_numeric = case_when(
    vote == 1 ~ 1,
    vote == 2 ~ 0,
    vote == 3 ~ -1
  ))

# Pivot the data to wide format, with countries as rows and votes as columns
votes_wide <- votes_processed %>%
  pivot_wider(names_from = country, values_from = vote_numeric)

# Specify the list of countries
selected_countries <- c("United States", "Russia", "United Kingdom", "France", "China", "Brazil", 
                        "India", "Germany", "Japan", "Italy", "Canada", "South Korea", "Pakistan", 
                        "Iran", "Israel", "Spain", "Saudi Arabia", "Indonesia", "South Africa", "Ukraine")

# Filter the dataset to include only the selected countries
votes_selected <- votes_wide %>%
  select(year, all_of(selected_countries))

# Filter the dataset for the years 2013, 2015, and 2022
votes_selected_2013 <- votes_selected %>%
  filter(year == 2013)
votes_selected_2015 <- votes_selected %>%
  filter(year == 2015)
votes_selected_2022 <- votes_selected %>%
  filter(year == 2022)

# Calculate the distances for each year
distances_2013 <- dist(votes_selected_2013, method = "manhattan")
distances_2015 <- dist(votes_selected_2015, method = "manhattan")
distances_2022 <- dist(votes_selected_2022, method = "manhattan")

# Take the reciprocal of the distance matrices to obtain similarity matrices
similarity_matrix_2013 <- 1 / as.matrix(distances_2013)
similarity_matrix_2015 <- 1 / as.matrix(distances_2015)
similarity_matrix_2022 <- 1 / as.matrix(distances_2022)


# Graphs of distances -----------------------------------------------------

# Function to create graph from similarity matrix
create_graph <- function(similarity_matrix, countries) {
  graph <- graph_from_adjacency_matrix(similarity_matrix, weighted = TRUE, mode = "undirected")
  V(graph)$name <- V(graph)$label <- countries
  V(graph)$color <- "lightblue"  # Node color
  V(graph)$size <- 15  # Node size
  E(graph)$color <- "gray"  # Edge color
  E(graph)$arrow.size <- 0.5  # Arrow size
  E(graph)$width <- 2  # Edge width
  E(graph)$curved <- 0.1  # Curvature of edges
  return(graph)
}

# Extract country names from the row or column names of the matrices
selected_countries <- rownames(similarity_matrix_2013)

# Create graph for each year
graph_2013 <- create_graph(similarity_matrix_2013, selected_countries)
graph_2015 <- create_graph(similarity_matrix_2015, selected_countries)
graph_2022 <- create_graph(similarity_matrix_2022, selected_countries)

# Plot the graphs
par(mfrow=c(1, 3))  # Set up a 1x3 layout for three plots
plot(graph_2013, layout = layout_nicely(graph_2013), vertex.label.dist = 1.5, main = "2013")
plot(graph_2015, layout = layout_nicely(graph_2015), vertex.label.dist = 1.5, main = "2015")
plot(graph_2022, layout = layout_nicely(graph_2022), vertex.label.dist = 1.5, main = "2022")

