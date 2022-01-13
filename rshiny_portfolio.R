# library(shiny)
library(tidyverse)
library(viridis)
library(ggplot2)
 library(ggrepel)
 library(reshape2)
 library(foreign)
 library(packcircles)
 library(ggiraph)
 library(htmlwidgets)
 library(gifski)
 library(png)
 library(plotly)
 library(gganimate)
 library(dplyr)
 library(GGally)

## Data Preparation

# Data Viz 1 --------------------------------------------------------------
covid <- read_csv("covidtracking_data_march2021.csv") %>% select(-Date)
pop <- read_csv("acs_state_by_state_race_and_proportion.csv")
regions <- read_csv("states_regions.csv")
regions <- rename(regions, State = state, Region = region)
covid$State <- state.name[match(covid$State, state.abb)]
covid$State[8] <- "District of Columbia"
covid_pop <- full_join(covid, pop)
covid_pop_regions <- full_join(covid_pop, regions)
total <- select(covid_pop_regions, State, Cases_Total, Deaths_Total, Pop_Total, Region)
total <- mutate(total, Death_Rate = (Deaths_Total / Cases_Total) * 100)
avg_death_rate <- mean(total$Death_Rate)
total <- total %>% arrange(desc(Death_Rate)) 
total$Death_Rate <- round(total$Death_Rate, digits = 2)

one <- ggplot(total, aes(reorder(Death_Rate, State), Death_Rate, label = State,
                         color = Region)) + 
  geom_point() +
  aes(
    text = paste(State, "had a fatality rate of", Death_Rate, "% as of March 2021."))+
  scale_colour_viridis_d() +
  aes(size = .5, show.legend = FALSE) +
  scale_fill_viridis() +
  geom_hline(yintercept = 1.714,
             linetype = "dotted",
             color = "grey",
             size = 1) +
  geom_label(aes(x = 6, label = "National Average\n 1.714%", y  = 1.9), 
             color = "black") +
  labs(x = "State (Hover to View)",
       y = "# of Fatalities / # of Positive Tests") +
  theme_minimal() +
  theme(axis.text.x= element_blank(),
        axis.ticks.x = element_blank(),
        aspect.ratio = 1/10) +
  guides(size = "none") +
  annotate("text", x = 30, y = -1.5, color = "darkgrey", size = 3,
           label = "Data from CovidTracking.com, March 2021")

viz_one <- ggplotly(one, tooltip = "text") %>%
  add_annotations(x = 10, y = 1.75, text = "National Average: 1.714%") 

# Data Viz 2 --------------------------------------------------------------
# Standardize
deaths_white_total <- sum(covid$Deaths_White) / sum(covid$Deaths_Total) * 100
deaths_black_total <- sum(covid$Deaths_Black) / sum(covid$Deaths_Total) * 100
deaths_latinx_total <- sum(covid$Deaths_Latinx) / sum(covid$Deaths_Total) * 100
deaths_asian_total <- sum(covid$Deaths_Asian) / sum(covid$Deaths_Total) * 100
deaths_aian_total <- sum(covid$Deaths_AIAN) / sum(covid$Deaths_Total) * 100
deaths_nhpi_total <- sum(covid$Deaths_NHPI) / sum(covid$Deaths_Total) * 100
deaths_other_total <- sum(covid$Deaths_Other_All) / sum(covid$Deaths_Total) * 100

race_char <- c("White", "Black", "Latinx", "Asian", "AIAN", "NHPI", 
               "Other")
death_by_race_dbl <- c(deaths_white_total, 
                       deaths_black_total,  
                       deaths_latinx_total,
                       deaths_asian_total, 
                       deaths_aian_total,  
                       deaths_nhpi_total,
                       deaths_other_total)
death_by_race <- tibble("Race" = race_char, 
                        "Death %" = death_by_race_dbl)

Race <- c("white", "asian", "latinx", "nhpi", "aian", "black", "other")
death_pop <- c(56.0, 3.26, 16.1, 0.15, 0.9, 14.1, 9.48)
pop <- c(60, 5.6, 18.4, 0.2, 0.7, 12.4, 2.5)
new_tib <- tibble(Race, death_pop, pop)
new_tib <- new_tib %>% mutate(Gap = pop - death_pop)

p <- ggplot(new_tib) +
  geom_segment(aes(x = 0, xend = Gap, y = Race, yend = Race), color = "grey") +
  geom_point(aes(x = 0, y = Race), size =3 ) +
  geom_point(aes(x = Gap, y = Race), size = 6, color = "goldenrod3") +
aes(
  x = Gap,
  y = Race,
  text = paste("This group is", pop, "% of the population,\nbut accounted for", death_pop, "% of COVID-19\ndeaths, which is a", Gap, "% gap."))+
  scale_y_discrete(limits = c("white", "asian", "latinx", "nhpi", "aian", "black", "other")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "seagreen") +
  annotate("text", x = -3.5, y = 7.3, label = '"Other" Race')+
  annotate("text", x = -0.8, y = 6.3, label = 'Black Americans')+
  annotate("text", x = -0.2, y = 5.4, label = 'American Indian and Alaskan Native Americans')+
  annotate("text", x = 0, y = 4.4, label = 'Native Hawaiian and Pacific Islander Americans')+
  annotate("text", x = 1.2, y = 3.3, label = 'LatinX Americans')+
  annotate("text", x = 1.2, y = 2.3, label = 'Asian Americans')+
  annotate("text", x = 1.9, y = 1.4, label = 'White Americans') +
  annotate("text", x = -5, y = 0.7, size = 3, color = "darkgrey", 
           label = "Data from the Kaiser Family Foundation, July 14, 2021") +
  annotate("text", x = 2, y = 6.5, size = 4, color = "seagreen", 
           label = "COVID-19 Fatalities") +
  annotate("segment", x = 0, xend = 2, y = 7.3, yend = 6.7, colour = "seagreen") 

pp <- ggplotly(p, tooltip = "text")

# National race totals taken directly from Census bureau
race_pop_total <- c(60, 12.4, 18.4, 5.6, 0.7, 0.2, 2.5)
pop_tibble <- tibble("Race" = race_char, "Population" = race_pop_total)

death_vs_pop <- full_join(death_by_race, pop_tibble)

death_vs_pop <- melt(death_vs_pop)
names(death_vs_pop)[3] <- "Percent"


# Data Viz 3 --------------------------------------------------------------

daily <- read_csv("covid_daily.csv") %>%
  select(date = "DATE_OF_INTEREST",
         deaths = "all_death_count_7day_avg")

daily <- daily %>% mutate(date = as.Date(date, "%m/%d/%Y"))

# Data Viz 4 & 5 --------------------------------------------------------------

# Pull in datasets
deaths <- read_csv("ny_county_coviddeath.csv")
county <- read_csv("ny_county_census_data_2.csv")
election <- read_csv("ny_county_election.csv")
census <- select(county, "county", 
                 pop = "RELATIONSHIP!!Population in households",
                 ninth = "EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Less than 9th grade", 
                 twelth = "EDUCATIONAL ATTAINMENT!!Population 25 years and over!!9th to 12th grade, no diploma")

# Select only the columns in use
census <- census %>% add_column(deaths = deaths$deaths, 
                                trump = election$trump_pct,
                                biden = election$biden_pct)

# Standardize the numerical formats
census$trump <- census$trump * 100
census$biden <- census$biden * 100
census$ninth = as.numeric(gsub("[\\%,]", "", census$ninth))
census$twelth = as.numeric(gsub("[\\%,]", "", census$twelth))

# Add two new columns: dpt = deaths per 10k residents, no dip = No Diploma
census <- census %>% 
  mutate(dpt = deaths / pop) %>%
  mutate(no_dip = ninth + twelth) 
census <- census[,c(1,5,2,8,3,4,9,7,6)] # Put them in sensical order
data <- census # Name the data more generally
data$dpt <- round(data$dpt * 10000, digits = 2)

# Data for first analysis (death rate per county and literacy)
literacy_data <- select(data, "county", "dpt", "no_dip", "pop") 

# Data for second analysis (death rate per county and Trump vote % in 2020)
trump_data <- select(data, "county", "dpt", "trump", "pop") 

six <- ggplot(trump_data, aes(x = trump, y = dpt, color = pop)) +
  geom_point() +
  scale_colour_viridis_d() +
  aes(size = .5, show.legend = FALSE) +
  aes(text = paste("This is", county, "which had a\ndeath rate of", dpt, "per 10,000 residents\nand", trump, "percent of residents supported\nDonald Trump in 2020.")) +
  scale_color_viridis(discrete = FALSE) +
  theme_minimal() +
  
  guides(size = "none") +
  guides(alpha = "none") +
  labs(subtitle = "Per 10,000 residents by Percent of Trump Vote in County")+
  labs(color = "Population (10,000's)")+
  labs(size = "") +
  labs(alpha = "")+
  labs(x = "2020 Trump Support (%) - Data from New York State, July 2021")+
  labs(y = "Fatalities per 10,000 Residents")+
  labs(caption = "by Casey Neubauer | Data Visualizaion | Johns Hopkins 2021\nData from NYC.gov.") +
  geom_abline(intercept = 23.2811, slope = -0.1623, color = "darkgrey", linetype = "dashed")

seven <- ggplot(literacy_data, aes(x = no_dip, y = dpt, color = pop)) +
  geom_point() +
  scale_colour_viridis_d() +
  aes(size = .5, show.legend = FALSE) +
  aes(text = paste("This is", county, "which had a\ndeath rate of", dpt, "per 10,000 residents\nand", no_dip, "percent of residents have\nno high school diploma.")) +
  scale_color_viridis(discrete = FALSE) +
  theme_minimal() +
  guides(size = "none") +
  guides(alpha = "none") +
  labs(color = "Population (10,000's)")+
  labs(size = "") +
  labs(alpha = "")+
  labs(x = "Percent of Residents Without High School Diploma - Data from Census.gov, July 2021")+
  labs(y = "Fatalities per 10,000 Residents")+
  labs(caption = "by Casey Neubauer | Data Visualizaion | Johns Hopkins 2021\nData from NYC.gov.") +
  geom_abline(intercept = 7.2475, slope = 0.7156, color = "darkgrey", linetype = "dashed")


# Data Viz 6-8 ---------------------------------------------------------
pew_data <- read.spss("ATP W72.sav", 
                      to.data.frame = TRUE, 
                      use.value.labels = FALSE)


# Select the data I am interested in
my_pew <- select(pew_data,
                 race = "F_RACETHNMOD",
                 gender = "F_GENDER",
                 party = "F_PARTY_FINAL",
                 news = "NEWSFINCORP_W72",
                 lost_job = "COVIDWORKR_a_W72",
                 returned = "POSTCOVIDJOBR2_W72",
                 pay = "POSTCOVIDPAYR_W72",
                 lost_income = "COVIDWORKR_b_W72") %>% 
  as_tibble(.) 

# Rename categorical variables for simplicity
my_pew$race <- recode_factor(my_pew$race,
                             "1" = "white", 
                             "2" = "black",
                             "3" = "asian",
                             "4" = "aian",
                             "5" = "nhpi",
                             "6" = "other",
                             "99" = "refused")

my_pew$gender <- recode_factor(my_pew$gender,
                               "1" = "male",
                               "2" = "female",
                               "3" = "other",
                               "99" = "refused") 

my_pew$party <- recode_factor(my_pew$party,
                              "1" = "rep",
                              "2" = "dem",
                              "3" = "ind",
                              "4" = "other",
                              "99" = "refused")

my_pew$news <- recode_factor(my_pew$news,
                             "1" = "alot",
                             "2" = "somewhat",
                             "3" = "little",
                             "4" = "none",
                             "99" = "refused")

my_pew$lost_job <- recode_factor(my_pew$lost_job,
                                 "1" = "yes",
                                 "2" = "no",
                                 "3" = "NA",
                                 "99" = "refused")

my_pew$returned <- recode_factor(my_pew$returned,
                                 "1" = "returned",
                                 "2" = "new",
                                 "3" = "NA",
                                 "99" = "refused")

my_pew$pay <- recode_factor(my_pew$pay,
                            "1" = "less",
                            "2" = "same",
                            "3" = "more",
                            "99" = "refused")

my_pew$lost_income <- recode_factor(my_pew$lost_income,
                                    "1" = "yes",
                                    "2" = "no",
                                    "99" = "refused")

lost_inc <- subset(my_pew, lost_income == "yes" & 
                     race != "refused" &
                     news != "refused" &
                     party != "refused")

# Prepare data for circle plots
prop_lost_inc <- round(table(lost_inc$race) / table(my_pew$race) * 100, digits = 2)
data <- data.frame(prop_lost_inc)
data <- rename(data, group = Var1, value = Freq)

# Rename so labels make sense
races <- c("White", "Black", "Asian", "American Indian\n or Alaskan Native",
           "Native Hawaiian\n or Pacific Islander", "Refused")
data$group <- races
data$text <- paste0(data$value)
packing <- circleProgressiveLayout(data$value, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)
# Pull out respondents who lost job
lost_job <- subset(my_pew, lost_job == "yes" & 
                     race != "refused" &
                     news != "refused" &
                     party != "refused")

# Prepare data for plotting
prop_lost_job <- round(table(lost_job$race) / table(my_pew$race) * 100, digits = 2)
data_two <- data.frame(prop_lost_job)
data_two <- rename(data_two, group = Var1, lost = Freq)
data_two <- data_two[-6,]
races <- c("White", "Black", "Asian", "American Indian or Alaskan Native",
           "Native Hawaiian or Pacific Islander")
data_two$group <- races
returned <- subset(my_pew, returned == "returned") %>% 
  drop_na(.)
working <- round(prop.table(table(returned$race)) * 100, digits = 2)
data_three <- data.frame(working)
data_three <- rename(data_three, group = Var1, back = Freq) 
data_three <- data_three[1:5,]
races <- c("White", "Black", "Asian", "American Indian or Alaskan Native",
           "Native Hawaiian or Pacific Islander")
data_three$group <- races

job_return <- full_join(data_two, data_three)

para <- ggparcoord(job_return, columns = 2:3, 
                   groupColumn = 1, 
                   showPoints = TRUE) +
  geom_line(size = 2) +
  aes(text = paste(group, "Americans")) +
  geom_point(size = 5) +
  scale_color_viridis(discrete = TRUE) +

  theme_void() +
  # Lost labels
  annotate("text", x = 0.8, y = -1.4, label = "10.37%") +
  annotate("text", x = 0.8, y = -.3, label = "13.76%")+
  annotate("text", x = 0.8, y = 0.1, label = "14.93%")+
  annotate("text", x = 0.8, y = 0.4, label = "15.92%")+
  annotate("text", x = 0.8, y = 1.3, label = "18.79%")+
  # Returned labels
  annotate("text", x = 2.2, y = 1.7, label = "63.02%") +
  annotate("text", x = 2.2, y = -.7, label = "2.96%")+
  annotate("text", x = 2.2, y = -.45, label = "8.58%")+
  annotate("text", x = 2.2, y = 0.1, label = "21.89%") +
  theme(legend.title = element_blank())+
  annotate("text", x = 2, y = -1.5, color = "darkgrey", size = 3,
           label = "Pew Research Center | August 2020")


# Data Viz 9 --------------------------------------------------------------

pew_data <- read.spss("ATP W72.sav", 
                      to.data.frame = TRUE, 
                      use.value.labels = FALSE)

# Select the data I am interested in
my_pew <- select(pew_data,
                 race = "F_RACETHNMOD",
                 gender = "F_GENDER",
                 party = "F_PARTY_FINAL",
                 news = "NEWSFINCORP_W72",
                 lost_job = "COVIDWORKR_a_W72",
                 returned = "POSTCOVIDJOBR2_W72",
                 pay = "POSTCOVIDPAYR_W72",
                 lost_income = "COVIDWORKR_b_W72") %>% 
  as_tibble(.) 

# Rename categorical variables for simplicity
my_pew$race <- recode_factor(my_pew$race,
                             "1" = "white", 
                             "2" = "black",
                             "3" = "asian",
                             "4" = "aian",
                             "5" = "nhpi",
                             "6" = "other",
                             "99" = "refused")

my_pew$gender <- recode_factor(my_pew$gender,
                               "1" = "male",
                               "2" = "female",
                               "3" = "other",
                               "99" = "refused") 

my_pew$party <- recode_factor(my_pew$party,
                              "1" = "Republican",
                              "2" = "Democratic",
                              "3" = "Independent",
                              "4" = "other",
                              "99" = "refused")

my_pew$news <- recode_factor(my_pew$news,
                             "1" = "yes",
                             "2" = "yes",
                             "3" = "no",
                             "4" = "no",
                             "99" = "refused")

my_pew$lost_job <- recode_factor(my_pew$lost_job,
                                 "1" = "yes",
                                 "2" = "no",
                                 "3" = "NA",
                                 "99" = "refused")

my_pew$returned <- recode_factor(my_pew$returned,
                                 "1" = "returned",
                                 "2" = "new",
                                 "3" = "NA",
                                 "99" = "refused")

# Pull out only the individuals who lost income (2,418 observations)
lost_inc <- subset(my_pew, lost_income == "1" | lost_income == "2" &
                     race != "refused" &
                     news != "refused" &
                     party != "refused")

less_money <- subset(my_pew, pay == "1" & 
                       gender != "refused" & 
                       party != "refused")

less <- select(less_money, "party", "gender", "pay") %>%
  group_by(party, gender) %>%
  summarize(less = sum(pay == "1")) %>%
  subset(party != "other" & gender != "other") 
percentages <- c(45.87, 54.21, 38.37, 61.63, 53.06, 46.94)
less$pct <- percentages

# Data Viz 10 --------------------------------------------------------------

# Set it up by race
by_race <- select(lost_inc, "race", "gender", "lost_income") %>%
  group_by(race, gender) %>%
  summarize(lost_inc = sum(lost_income == "1")) %>%
  subset(race != "refused" & gender != "refused") 

# Shiny UI ----------------------------------------------------------------

ui <- fluidPage(
  # Main Title
  titlePanel("The Unequal Consequences of COVID-19"),
      navlistPanel(
# Category 1
    "United States Fatalities:",
    tabPanel("By State",
             mainPanel(
              h2("COVID-19 Fatality Rate by US State (%)"),
              h4("Northeastern states have higher death rates, possibly due to low testing availability in early 2020."),
               plotlyOutput("plot1", 
                            width = "130%", 
                            height = "500px"))),
    tabPanel("By Race",
             mainPanel(
               h2('Large Percent of Reported COVID-19 Deaths Reported "Other" Race, White Americans Dying Less Than Others'),
               h4("Demonstrated below is the gap between COVID-19 deaths and the US population of each respective race. Nearly ten percent of COVID-19 deaths are labeled as 'Other Race', but only about three percent of the population. This distinction makes it difficult to draw more than prelminary conclusions from this data. Of the COVID-19 deaths with a known race, White Americans have the most significant positive gap at 4%."),
               plotlyOutput("plot2", 
                            width = "130%", 
                            height = "500px"))),
    
# Category 2
    "New York Fatalities:",
    tabPanel("Daily Fatalities in New York City",
               mainPanel(
                 h2("Fatalities in New York City Over Time (7-day Average)"),
                 h4("New York City was among the first to be hit with a horrendous COVID-19 outbreak. Because testing was so scarce during the beginning of the pandemic, the fatality rate is likely skewed. These rates should be compared to other cities during the same period only."),
                 h5("Data is from NYC.gov, July 2021."),
             imageOutput("plot3"))),
tabPanel("By New York County",
         tabsetPanel(
    tabPanel("By Educational Attainment", 
             mainPanel(
               h2("COVID-19 Death Rates by Educational Attainment"),
               h4("Each dot represents a New York County. Fatalities were calculated per 10,000 residents and measured against that county's level of educational attainment. Educational attainment was determined by the percent of residents with a high school diploma. There is a moderate, positive correlation between these two variables of 0.36."),
               plotlyOutput("plot4",  width = "130%", height = "600px"))),
    tabPanel("By Trump Support (2020)", 
             mainPanel(
               h2("COVID-19 Death Rates by Trump Support"),
               h4("Each dot represents a New York County. Fatalities were calculated per 10,000 residents and measured against that county's level of Trump support in 2020. There is a negative correlation of -0.30 between these two variables. It may be skewed by higher fatality rates in New York City counties."),
               plotlyOutput("plot5", width = "130%", height = "600px"))))),

# Category 3
    "Pew Research Survey Data (August 2020)",

    tabPanel("Lost Jobs, Returned to Work By Race", 
             mainPanel(
               h2("White Americans Have Disproportionately Returned to Work"),
               h4("The left side of this graph displays the racial breakdown of survey respondents who reported they lost a job due to COVID-19. The right side shows the racial breakdown of those respondents who have since returned to work. White Americans only accounted for about 10% of jobs lost, but 63% had already returned to work."),
               plotlyOutput("plot7", width = "100%", height = "500px"))),
tabPanel("Loss of Income by Race", 
         mainPanel(
           h2("Fewer White Americans Experienced a Loss of Income From COVID-19 Than Other Americans"),
           h4("According to Pew Research in August 2020, nearly 20% of Americans reported a loss of income due to COVID-19. White Americans reported a loss of income the least often at 16.58%."),
           plotOutput("plot6", width = "150%", height = "600px"))),
    tabPanel("Lost Income by Gender and Political Party",
             mainPanel(
               h2("Democratic Women Most Frequently Reported Making Less Money Since Pandemic Began"),
               h4("Displaying only the individuals who reported making less money since the pandemic began, Democratic women reported making less the most frequently."),
               h6("Data From Pew Research Center | August 2020"),
               plotOutput("plot8", width = "150%", height = "600px"))),
    tabPanel("Lost Income by Gender and Race",
             mainPanel(
               img(src = "data_viz_10.png", height = 700, width = 830))),
    tabPanel("Lost Income and Trust of News/Media",
             mainPanel(
               img(src = "data_viz_9.png", height = 700, width = 900))) 
), 
hr(),
print("Created by Casey Neubauer | Johns Hopkins University | Summmer 2021")
)

# Shiny Server ------------------------------------------------------------

server <- function(input, output){
  
  output$plot1 <- renderPlotly({

ggplotly(viz_one)

  })
  
  output$plot2 <- renderPlotly({
    
ggplotly(pp)
    
  })
  
  output$plot3 <- renderImage({
    
    outfile <- tempfile(fileext='.gif') 
    
    p = ggplot(daily, aes(x = date, y = deaths)) + 
      geom_line() +
      geom_area(fill = viridis::viridis(1)) +
      ylim(0, 850) +
      annotate(geom="text", x=as.Date("2020-07-10"), y=620, 
               size = 6,
               label="On Apri 11th, 2020,\nNYC Deaths Hit Highest\n7-day Average of 760.") +
      annotate(geom="point", x=as.Date("2020-04-11"), 
               y=760, size=9, shape=21, fill="transparent") +
      annotate(geom = "curve",
               x = as.Date("2020-07-01"),
               y = 710,
               xend = as.Date("2020-04-20"),
               yend = 760 + 10,
               color = viridis(1),
               arrow = arrow(angle = -30)) +
      labs(x = "", y = "Fatalities (7-day Average)") + 
      scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
      theme_minimal() +
      transition_reveal(date) +
      shadow_wake(wake_length=1,alpha=FALSE,wrap=FALSE)

    anim_save("outfile.gif", animate(p, height = 500, width = 800)) 
    
    list(src = "outfile.gif",
         contentType = 'image/gif')
}, deleteFile = TRUE)
  
  output$plot4 <- renderPlotly({
    
    ggplotly(seven, tooltip = "text")
    
  })
  
  output$plot5 <- renderPlotly({

    ggplotly(six, tooltip = "text")
    
  })
  
  output$plot6 <- renderPlot({
    
    ggplot() + 
      geom_polygon(data=dat.gg, aes(x, y, 
                                    group = id, 
                                    fill=as.factor(id)),
                   colour = "white", 
                   alpha = 1) +
      scale_fill_viridis_d() +
      geom_text(data=data, aes(x, y, label = group), size=9, color="white") +
      geom_text(data=data, aes(x, y, label = value), size=7, color="white", nudge_y = -1.25) +
      scale_size_continuous(range = c(1,4.5)) +
      labs(caption = "Pew Research Center | Racial make-up of the 2,412 respondents who lost income | August 2020") +
      theme_void() + 
      theme(legend.position="none") +
      coord_equal()
    
  })
  
  output$plot7 <- renderPlotly({
    
    ggplotly(para, tooltip = "text")
    
  })
  
  output$plot8 <- renderPlot({
    
    ggplot(less, aes(x = reorder(party, less), y = less, fill = gender)) + 
      geom_text(aes(label = less), 
                position = position_dodge(width = 1), vjust = -.5) +
      geom_bar(position="dodge", stat="identity") +
      ylim(0, 400) +
      scale_fill_viridis_d() +
      theme_minimal() +
      theme(legend.title = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank()) +
      labs(y = "# of Respondents Making Less Money Now")+
      annotate("segment", x = 1.6, xend = 2.9, y = 300, yend = 275, colour = "grey")+
      annotate("text", x = 1, y = 325, label = "Democratic women reported making\nless money since COVID-19 began\nthe most frequently, especially\nwhen compared to their male,\nDemocratic counterparts.", hjust=0)
    
    
  })
  
  output$plot9 <- renderPlot({
    
    
  })
  
  output$image2 <- renderPlot({

  })
  
}

shinyApp(ui=ui,server=server)

