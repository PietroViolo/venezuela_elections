#---------------------------------------------------------------------------#
# Nom : venezuela_map.R                                   			            #
# Description : Venezuela map elections results                             #
# Auteur : Pietro Violo                                                     #
# Date : 30 Jul 2024                                                        #
# Modifications :                                                           #
#---------------------------------------------------------------------------#

options(scipen=999)

# Library
library(tidyverse)
library(rvest)
library(httr)
library(ggthemes)

#---------------------------------------------------------------------------#
# Data wrangling                                                            #
#---------------------------------------------------------------------------#
rm(list = ls(all = TRUE))

# Collect data through web scraping

df <- c()
df_extra_data <- c()
digitalization <- c()

i = 1

for(i in 1:24){
  url = paste("https://resultadosconvzla.com/estado/",i, sep = "")

  # collect html info
  page <- read_html(GET(url))
  
  # Get tallies
  
  state_name <- page %>% 
    html_nodes("#tabla .titulo") %>% 
    html_text()
  
  names <- page %>% 
    html_nodes("h3") %>% 
    html_text()
  
  votes <- page %>% 
    html_nodes("h4") %>% 
    html_text()
  
  df <- rbind(df, 
              cbind(names, votes) %>% 
                as.data.frame() %>% mutate(State = state_name))
  
  names_data <- c("Mesas totales", "Mesas transmitidas", "Electores",
                  "Votantes", "Participacion")
  
  data <- page %>% html_nodes(".font-weight-bold") %>% 
    html_text()
  
  df_extra_data <- rbind(df_extra_data,
                         cbind(names_data, data) %>% 
    as.data.frame() %>% 
    mutate(State = state_name))
  
  digitalization <- rbind(digitalization,
                          cbind(page %>% html_nodes(".justify-content-between .text-end") %>% 
    html_text() %>% as.data.frame(), state_name))
  
  Sys.sleep(2)
  print(i)
  
}

df %>% pull(State) %>% unique()
# Clean data

df_cleaned <- df %>% mutate(votes = str_remove(votes, " votos"),
              votes = as.numeric(str_remove(votes, "\\."))) %>% 
  pivot_wider(names_from = names,
              values_from = votes) %>% 
  mutate(State = str_extract(State, "(?<=\\. ).*"),
         State = tolower(State),
         State = case_when(State == "delta amac" ~ "delta amacuro",
                           State == "bolivar" ~ "bolívar",
                           State == "tachira" ~ "táchira",
                           State == "merida" ~ "mérida",
                           State == "falcon" ~ "falcón",
                           State == "anzoategui" ~ "anzoátegui",
                           is.na(State) ~ "nueva esparta",
                           State == "guarico" ~ "guárico",
                           State == "capital" ~ "distrito capital",
                           TRUE ~ State)) %>% 
  rename(name = State) %>% 
  mutate(pct = round(`Edmundo González` / (`Edmundo González` + `Nicolás Maduro` + `Otros`) * 100),0)

#---------------------------------------------------------------------------#
# Visualization.                                                            #
#---------------------------------------------------------------------------#

# Load necessary libraries
library(ggplot2)
library(sf)
library(rnaturalearth)

# Download Venezuela states shapefile using rnaturalearth
venezuela_states <- ne_states(country = "Venezuela", 
                              returnclass = "sf")

venezuela_states <- venezuela_states %>% mutate(name = tolower(name))

venezuela_states <- left_join(venezuela_states,
          df_cleaned)

# Create categories
venezuela_states <- venezuela_states %>% mutate(pct_label = case_when(
  pct < 35 ~ "< 35%",
  pct >= 35 & pct < 45 ~ "35% - 44%",
  pct >= 45 & pct < 50 ~ "45% - 49%",
  pct >= 50 & pct < 55 ~ "50% - 54%",
  pct >= 55 & pct < 65 ~ "55% - 64%",
  pct >= 65 & pct <= 75 ~ "65% - 74%",
  pct > 75 ~ "> 75%",
  is.na(pct) ~ NA_character_
),
pct_label = factor(pct_label,
             levels = c("< 25%",
                        "25% - 34%",
                        "35% - 44%",
                        "45% - 49%",
                        "50% - 54%",
                        "55% - 64%",
                        "65% - 74%",
                        "> 75%")))

venezuela_states %>% pull(pct_label) %>% unique()


colors <- c("#caf0f8",  # Very Light Blue
            "#90e0ef",  # Light Blue
            "#0077b6",
            "#03045e") 

plot <- ggplot(data = venezuela_states) +
  geom_sf(aes(fill = pct_label), color = "white") +  
  scale_fill_manual(values = colors) + 
  labs(title = "Map of Venezuela States",
       x = NULL,  # Remove x-axis label
       y = NULL,  # Remove y-axis label
       fill = "Percentage") +  # Label for the fill legend
  theme_minimal() +
  theme(
    legend.position = "right",  # Adjust legend position if necessary
    panel.background = element_rect(fill = "transparent"),  # Transparent panel background
    plot.background = element_rect(fill = "transparent", color = NA),  # Transparent plot background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text = element_blank(),  # Remove axis text
    axis.ticks = element_blank()  # Remove axis ticks
  )


# Save the plot with a transparent background
ggsave("./Outputs/venezuela_states_map.png", plot = plot, bg = "transparent", 
       width = 3000, height = 3000, units = "px")

# Bar chart
library(tools)

# Define the colors
library(grDevices)

# Define the mexican colors for the gradient
colors <- c("#f1c559", "#4995ca", "#e00c3b")

# Create a function to generate the color gradient
color_gradient <- colorRampPalette(colors)

# Generate a vector of 32 colors
color_vector <- color_gradient(24)
color_vector <- rev(color_vector)


plot <- df_cleaned %>% select(-pct, -`0`) %>% pivot_longer(`Edmundo González`:`Otros`,
                            names_to = "Candidate",
                            values_to = "Count") %>% 
  mutate(name = toTitleCase(name),
         Candidate = ifelse(Candidate == "Otros", "Others", Candidate)) %>% 
  ggplot(aes(y = Candidate, x = Count, fill = name)) +
  geom_bar(stat = "identity")+  # Label for the fill legend
  theme_minimal() +
  theme(
    legend.position = "none",  # Adjust legend position if necessary
    panel.background = element_rect(fill = "transparent"),  # Transparent panel background
    plot.background = element_rect(fill = "transparent", color = NA)  # Remove axis ticks
  ) +
  scale_fill_manual(values = color_vector)

# Save the plot with a transparent background
ggsave("./Outputs/venezuela_states_barplot.png", plot = plot, bg = "transparent", 
       width = 5000, height = 2000, units = "px")


df_cleaned %>% select(-pct, -`0`) %>% pivot_longer(`Edmundo González`:`Otros`,
                                                  names_to = "Candidate",
                                                  values_to = "Count") %>% 
  group_by(Candidate) %>% 
  summarise(Count = sum(Count))

round(7119768 / (7119768 + 3225819 + 250135) * 100,1)

round(3225819 / (7119768 + 3225819 + 250135) * 100,1)

round(250135 / (7119768 + 3225819 + 250135) * 100,1)


