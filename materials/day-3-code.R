#==============================================================================
# Day 3: RI Workshop
#==============================================================================

#==================================
# Group Work
#==================================

# Write your code below



#==================================
# Becoming Familiar with Data
#==================================
library(tidyverse)

# Read in the data
anes <- read_csv('https://www.dropbox.com/scl/fi/i6gp3y8ctmwxs1lp705ax/anes.csv?rlkey=44twem0nyu6tdq65sv2wlw93c&dl=1')

# Pull out some variables
anes_data<-anes %>%
  rename('pid' = V201018,
         'gender' = V201600) %>%
  select(pid,gender)

table(anes_data$pid)

table(anes_data$gender)

# Fixing gender
anes_data_g<- anes_data %>%
  mutate(gender = case_when(gender < 0 ~ NA,
                            TRUE ~ gender),
         gender = factor(gender,
                         levels = c(1,2),
                         labels = c('Male','Female')))

table(anes_data_g$gender)

# Fixing PID
table(anes_data$pid)

anes_data_p<-anes_data %>%
  mutate(pid = case_when(pid < 0 ~ NA,
                         TRUE ~ pid),
         pid = factor(pid,
                      levels = c(1,2,3),
                      labels = c('Democrat',
                                 'Republican',
                                 'Independent')))

table(anes_data_p$pid)

#==================================
# Group Work
#==================================

# Type your code below


#==================================
# ggplot
#==================================

# Basic structure of a ggplot graph
ggplot(data, aes(x = x_var,y = y_var))+
  geom_line()

# The additional lines here change the theme and move the title up
ggplot(data, aes(x = x_var,y = y_var))+
  geom_line()+
  theme_minimal()+
  theme(plot.title = element_text(hjust=.5))

# Run this code to create the graphs below
data<-read_csv("https://www.dropbox.com/scl/fi/vk45hy9buvuhfels3hrzl/live_coding_1.csv?rlkey=zn85n9pg2ztqx8czd4pe1xnj0&st=dvziitby&dl=1")

politicians <- data |>
  rename(
    full_name          = `Full Name`,
    party              = `Political Affiliation`,
    state              = `Represented State`,
    age                = `Politician Age`,
    years_served       = `Years Served`,
    votes_received     = `Votes Received`,
    legislation_passed = `Legislation Passed`,
    speeches_given     = `Speeches Given`,
    bills_sponsored    = `Bills Sponsored`,
    committees_joined  = `Committees Joined`
  ) %>%
  mutate(
    # Order the party ID using factor
    party = factor(
      party,
      levels = c("Democratic", "Republican", "Independent", "Libertarian")
    )
  )

# Scatter Plot

politicians |>
ggplot(
  mapping = aes(x = age, y = votes_received)) +
  geom_point()


## add a line to see the correlation
politicians |>
  ggplot(
    mapping = aes(x = age, y = votes_received)) +
  geom_point()+
  geom_smooth(method="lm", se=FALSE)

# Histogram (only requires x axsis argument)
politicians |>
  ggplot(
    mapping = aes(x = age)) +
  geom_histogram()

hist(politicians$age)

# Density Plot
politicians |>
  ggplot(
    mapping = aes(x = age)) +
  geom_density()

# Bar Plot
politicians |>
  group_by(party) |>
  summarise(ave_age=mean(age)) |>
  ggplot(aes(x=party, y=ave_age)) +
  geom_bar(stat="identity")

# Boxplots
politicians |>
  ggplot(aes(x=party, y=age)) +
  geom_boxplot()


# Adding Color
politicians |>
  group_by(party) |>
  summarise(ave_age=mean(age)) |>
  ggplot(aes(x=party, y=ave_age, fill=party)) +
  geom_bar(stat="identity")+
  theme_bw()


## Adding color manually
politicians |>
  group_by(party) |>
  summarise(ave_age=mean(age)) |>
  ggplot(aes(x=party, y=ave_age, fill=party)) +
  geom_bar(stat="identity")+
  theme_bw()+
  scale_fill_manual(values=c("blue","red", "purple", "gold"))

# Changing axis labels
politicians |>
  ggplot(aes(x = age, y = votes_received, color=party)) +
  geom_point()+
  scale_color_manual(values=c("blue","red", "purple", "gold"))+
  labs(y="Votes Recieved", x="Age", color="Party")+
  theme_bw()

# Adding a title
politicians |>
  ggplot(aes(x = age, y = votes_received, color=party)) +
  geom_point()+
  scale_color_manual(values=c("blue","red", "purple", "gold"))+
  labs(y="Votes Recieved", x="Age", color="Party",
       title="Congress Members Age and Votes")+
  theme_bw()

# Adding a caption
politicians |>
  ggplot(aes(x = age, y = votes_received, color=party)) +
  geom_point()+
  scale_color_manual(values=c("blue","red", "purple", "gold"))+
  labs(y="Votes Recieved", x="Age", color="Party",
       title="Congress Members Age and Votes",
       caption="Source: Congressional Data")+
  theme_bw()

# Removing the legend
politicians |>
  group_by(party) |>
  summarise(ave_age=mean(age)) |>
  ggplot(aes(x=party, y=ave_age, fill=party)) +
  geom_bar(stat="identity")+
  theme_bw()+
  scale_fill_manual(values=c("blue","red", "purple", "gold"))+
  theme(legend.position = 'none')

# Chaning point size
politicians |>
  ggplot(aes(x = age, y = votes_received, color=party)) +
  geom_point(size=3)+
  scale_color_manual(values=c("blue","red", "purple", "gold"))+
  labs(y="Votes Recieved", x="Age", color="Party",
       title="Congress Members Age and Votes")+
  theme_bw()

# Changing transparency of points
politicians |>
  ggplot(aes(x = age, y = votes_received, color=party)) +
  geom_point(alpha=0.3)+
  geom_smooth(method="lm", se=FALSE)+
  scale_color_manual(values=c("blue","red", "purple", "gold"))+
  labs(y="Votes Recieved", x="Age", color="Party",
       title="Congress Members Age and Votes")+
  theme_bw()
