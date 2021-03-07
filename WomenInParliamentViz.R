# Women In Parliament

# Set wd
setwd("~/Coding/Blog/Programming/WomenParliament")

# Load the necessary packages
if(!require("tidyverse")) install.packages("tidyverse") # Our rock in data analysis (includes ggplot2)
if(!require("janitor")) install.packages("janitor") # cleans data like the best janitor out there!
if(!require("ggsci")) install.packages("ggsci") # My favourite palettes 
if(!require("ggalt")) install.packages("ggalt") # Necessary for the dumbbell chart
if(!require("ggtext")) install.packages("ggtext") # ggtext allows for great text to include for charts

# Load the csv data file
df <- read_csv("WomenInGovernment_Data.csv")

# Change the column names to account for each year of the data 1997-2020
names <- colnames(df)[4:27]
names <- substring(names, 7,12)
colnames(df)[4:27] <- names
colnames(df) <- janitor::make_clean_names(colnames(df))

# Change all the ..s in the data to NAs and then convert the columns to numeric
df[,4:27] <- na_if(df[,4:27], "..")
df[,4:27] <- sapply(df[,4:27],as.numeric) %>% 
  round(digits = 1)

# Create a dataset for each chart, just to make it .
# For this we filter the data for the region, income or country, then make the graph longer using the pivot_longer() function
# Regional chart dataset
df.region <- df %>% 
  filter(category == "Region" | category == "World") %>% 
  pivot_longer(cols = starts_with("yr"), names_to = "year", names_prefix = "yr",
               values_to = "value", values_drop_na = TRUE)

# Income chart dataset
df.income <- df %>% 
  filter(category == "Income" | category == "World") %>% 
  pivot_longer(cols = starts_with("yr"), names_to = "year", names_prefix = "yr",
               values_to = "value", values_drop_na = TRUE)

# Selected Countries chart (Chose countries that are either near and dear to me or where my readers tend to be)
selected_countries <- c("World", "Canada", "China", "Denmark", "France", "Germany", "India", 
                        "Italy", "Norway", "Sweden", "United Kingdom", "United States")
df.country <- df %>% 
  filter(country_name %in% selected_countries| category == "World") %>%
  pivot_longer(cols = starts_with("yr"), names_to = "year", names_prefix = "yr",
               values_to = "value", values_drop_na = TRUE)

# Top Female representation by country dumbbell chart
df.top.dumbbell <- df %>% 
  filter(category == "Country") 
df.top.dumbbell$change <- df.top.dumbbell$yr2020 - df.top.dumbbell$yr1997
df.top.dumbbell <- head(arrange(df.top.dumbbell, desc(yr2020)), n = 20) %>% 
  droplevels()
df.top.dumbbell$country_name <- factor(df.top.dumbbell$country_name, levels = as.character(df.top.dumbbell$country_name))  # to retain the order in plot.

# Top Female Increase in representation dumbbell chart
df.change.dumbbell <- df %>% 
  filter(category == "Country") 
df.change.dumbbell$change <- df.change.dumbbell$yr2020 - df.change.dumbbell$yr1997
df.change.dumbbell <- head(arrange(df.change.dumbbell, desc(change)), n = 20) %>% 
  droplevels()
df.change.dumbbell$country_name <- factor(df.change.dumbbell$country_name, levels = as.character(df.change.dumbbell$country_name))  # to retain the order in plot.

# Okay time to visualize!!!
# Let's set the theme and create a folder to store your output
theme_set(theme_bw())
dir.create("plots")

# First create a plot of all the regional representation of women in government
region.plot <- ggplot(df.region, aes(x=year,y=value,group=country_name,color=country_name)) +
  geom_line(size = 1.5) +
  
  # My top color palette
  scale_color_simpsons() +
  
  # Set the x-axis scale
  scale_x_discrete(breaks=c(1997,2002,2007,2012,2017)) +
  
  # Update the labels of the axes
  labs(x = "Year",
       y = "Percentage of Women in Government",
       title = "Female Representation in Government by World Region",
       subtitle = "Unsurprisingly Europe leads the way in female representation, followed by Latin America \n& the Caribbean and then North America. Since 1997, the world's average has increased \nfrom 11.7% to 25.2%",
       color = "Region")  +
 
   #Adjust the axes
  theme(plot.title = element_text(face="bold", size =14), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 12))
region.plot
# Save the plot!
ggsave("plots/RegionalPlot.png", region.plot, height = 5, width = 8)

# Next comes all the countries by income level
income.plot <- ggplot(df.income, aes(x=year,y=value,group=country_name,colour=country_name)) +
  geom_line(size = 1.5) +
  scale_color_simpsons() +
  scale_x_discrete(breaks=c(1997,2002,2007,2012,2017)) +
  labs(x = "Year",
       y = "Percentage of Women in Government",
       title = "Female Representation in Government by Country Income Level",
       subtitle = "Looking at it by income basically replicates the regional charts, with OECD and high income \ncountries leading the way with around 30% female representation. Interestingly heavily indebted \npoor countries have made the biggest rise, gaining about 17 percentage points over the past 25 years",
       color = "Country Income Level Group")  +
  theme(plot.title = element_text(face="bold", size =14), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 12))
income.plot
ggsave("plots/IncomePlot.png", income.plot, height = 5, width = 8)

# Third comes my selected countries
country.plot <- ggplot(df.country, aes(x=year,y=value,group=country_name,colour=country_name)) +
  geom_line(size = 1.5) +
  scale_color_simpsons() +
  scale_x_discrete(breaks=c(1997,2002,2007,2012,2017)) +
  labs(x = "Year",
       y = "Percentage of Women in Government",
       title = "Female Representation in Government from Selected Countries",
       subtitle = "Sweden, Germany, Denmark and Norway have all had a higher representation of women in government \nfor almost all of the past 25 years. On the other hand, there has been large growth in representation \nfrom countries like Italy, the UK and France. India and the US lag against these other countries",
       color = "Selected Countries")  +
  theme(plot.title = element_text(face="bold", size =14), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 12))
country.plot
ggsave("plots/CountryPlot.png", country.plot, height = 5, width = 8)

# Okay now for the pretty dumbbell charts
# The first dumbbell chart shows
top.dumbbell.plot <- ggplot(df.top.dumbbell, aes(y = country_name, x = yr1997, xend = yr2020, group=country_name)) +
  geom_dumbbell(size= 1, size_x = 3, size_xend = 3, colour_x = "darkred", colour_xend = "darkgreen", colour = "grey") +
  labs(
    y = "Country",
    x = "Percentage of Women in Government",
    title = "20 Countries with the Highest Proportion of Women \nin Government as of 2020",
    subtitle = "Despite the higher income countries showing larger proportions of women in government, only 1 of <br>the top 10 countries in <span style = 'color:darkgreen'><b>2020</b></span> is from Europe and the top three countries are Rwanda, Cuba and Bolivia. <br>Definitely different than most people would expect!"
  )+
  scale_x_continuous(breaks = seq(from = 0, to = 60, by = 10)) +
  scale_y_discrete(limits = rev(levels(df.top.dumbbell$country_name))) +
  theme(
    panel.background = element_rect(fill = "#FFEFCB", color = NA),
    plot.background = element_rect(fill = "#FFEFCB", color = NA),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_markdown()
  ) +
  geom_rect(data = df.top.dumbbell, aes(xmin = max(yr2020) * 1.09, xmax = max(yr2020) * 1.13, ymin = -Inf, ymax = Inf), fill = "grey") +
  annotate(geom="text", x = 72, y= df.top.dumbbell$country_name[9], angle = 270,
           label=c("Percent Increase"), color="darkgreen", size = 4, fontface = 'bold') +  
  geom_text(data = df.top.dumbbell, aes(label = paste0("+", round(change), "%"), y = country_name, x = max(yr2020) * 1.11), fontface = "bold", size = 3, color = "darkgreen")
top.dumbbell.plot
ggsave("plots/TopdumbbellPlot.png", top.dumbbell.plot, height = 5, width = 10)

# The second dumbbell chart
change.dumbbell.plot <- ggplot(df.change.dumbbell, aes(y = country_name, x = yr1997, xend = yr2020, group=country_name)) +
  geom_dumbbell(size= 1, size_x = 3, size_xend = 3, colour_x = "darkred", colour_xend = "darkgreen", colour = "grey") +
  labs(
    y = "Country",
    x = "Percentage of Women in Government",
    title = "The 20 Countries with the Largest Increases of Women \nin Government from 1997 to 2020",
    subtitle = "Many of the countries that have grown their proportion of women in government had less than 15% of <br>women in government in <span style = 'color:darkred'><b>1997</b></span>, but have since seen those levels grow 25-50 percentage <br>points by <span style = 'color:darkgreen'><b>2020</b></span>."
  )+
  scale_x_continuous(breaks = seq(from = 0, to = 60, by = 10)) +
  scale_y_discrete(limits = rev(levels(df.change.dumbbell$country_name))) +
  theme(
    panel.background = element_rect(fill = "#FFEFCB", color = NA),
    plot.background = element_rect(fill = "#FFEFCB", color = NA),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_markdown()
  ) +
  geom_rect(data = df.change.dumbbell, aes(xmin = max(yr2020) * 1.09, xmax = max(yr2020) * 1.13, ymin = -Inf, ymax = Inf), fill = "grey") +
  annotate(geom="text", x = 72, y= df.change.dumbbell$country_name[9], angle = 270,
           label=c("Percent Increase"), color="darkgreen", size = 4, fontface = 'bold') +  
  geom_text(data = df.change.dumbbell, aes(label = paste0("+", round(change), "%"), y = country_name, x = max(yr2020) * 1.11), fontface = "bold", size = 3, color = "darkgreen")
change.dumbbell.plot
ggsave("plots/ChangedumbbellPlot.png", change.dumbbell.plot, height = 5, width = 10)


# Do it all in Plotly
library(plotly)
fig <- plot_ly(df.income, x = ~year, y = ~value, name = ~country_name, type = 'scatter', mode = 'lines') 
fig