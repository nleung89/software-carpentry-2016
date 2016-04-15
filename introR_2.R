# PLYR
install.packages('dplyr')
library(dplyr)

# Opening data files
dat = read.csv(file.choose())
  # Viewing data files
View(dat)
head(dat)
glimpse(dat)
dim(dat)

# Selecting data - shrink dataset
select(dat, order, species)
select(dat, starts_with('adult'))
select(dat, -order)
# Selecting data - certain rows
  ## Only carnivores
filter(dat, order == "Carnivora")
  ## Only carnivores smaller than 5kg
filter(dat, order == "Carnivora" & adult_body_mass_g < 5000)
  ## Any carnivore or primate
filter(dat, order == "Carnivora" | order == "Primates")

# Arranging data
  ## Row 1 is the smallest mammals, the bubblebee bat
head(arrange(dat, adult_body_mass_g))
  ## Sorts by descending - Row 1 is the blue whale
head(arrange(dat, desc(adult_body_mass_g)))
  ## Sorts first alphabetically by order, then by mass within order
head(arrange(dat, order, adult_body_mass_g))

# EXERCISE 1
  ## What is the smallest mammal
dat.1 <- arrange(dat, adult_body_mass_g)
head(dat.1)
  ## What is the heaviest carnivore?
dat.2 <- filter(dat, order == 'Carnivora')
dat.3 <- arrange(dat, desc(adult_body_mass_g))
head(dat.3)
  ## How many primates are in our dataset?
dat.4 <- filter(dat, order == 'Primates')
dim(dat.4)

# Summarizing data
  ## The average mass of each order in a new column
  ## na.rm removes all the 'na' data
a <- group_by(dat, order)
summarize(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE))
  ## The average mass of each order with the SD in new columns
summarize(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE), sd_mass = sd(adult_body_mass_g, na.rm = TRUE))

  ## Mutate will add these columns to the original dataframe
a <- group_by(dat, order)
mutate(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE))
a <- group_by(dat, order)
mutate(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE), norm_mass = adult_body_mass_g / mean_mass)

  ## In summary of summarizing data...
a <- group_by(dat, order)
b <- summarize(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE))
c <- mutate(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE))
head(c)
glimpse(c)
d <- summarize(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE), sd_mass = sd(adult_body_mass_g, na.rm = TRUE))
e <- mutate(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE), norm_mass = adult_body_mass_g / mean_mass)
  ## This will also work but can be more confusing
e <- mutate(group_by(dat, order), mean_mass = mean(adult_body_mass_g, na.rm = TRUE), norm_mass = adult_body_mass_g / mean_mass)
View(e)

# Using pipes for code streamlining
e <- dat %>%  ## take data
  group_by(order) %>% ## split it up by order
  mutate(mean_mass = mean(adult_body_mass_g, na.rm = TRUE))  # find average weight for every order
  
  ## This is the longhand
a = group_by(dat, order)
b = mutate(a, mean_mass = mean(adult_body_mass_g, na.rm = TRUE), normalized_mass = adult_body_mass_g / mean_mass)
c = arrange(b, desc(normalized_mass))
d = select(c, species, normalized_mass)

  ## This is the pipe - less messy and reduces the number of variables
e = dat %>%
  group_by(order) %>%
  mutate(mean_mass = mean(adult_body_mass_g, na.rm = TRUE), 
  normalized_mass = adult_body_mass_g / mean_mass) %>%
  arrange(desc(normalized_mass)) %>%
  select(species, normalized_mass, adult_body_mass_g) 
View(e)
  
# EXERCISE 2
  ## Which order has the most species?
most_species <- dat %>%
  group_by(order) %>%
  summarize(species_count = length(species)) %>%
  arrange(desc(species_count))
most_species
  ## Which order has the widest range of body mass (max-min)?
body_mass_range <- dat %>%
  group_by(order) %>%
  summarize(range = max(adult_body_mass_g, na.rm = TRUE) - min(adult_body_mass_g, na.rm = TRUE)) %>%
  arrange(desc(range))
body_mass_range
  ## Which species of carnivore has the largest body length to body mass ratio?
carn_ratio <- dat %>%
  filter(order == 'Carnivora') %>%
  mutate(length_mass_ratio = adult_head_body_len_mm / adult_body_mass_g) %>%
  arrange(desc(length_mass_ratio))
carn_ratio

# Listing the data
##dat$<tab>
dat$order

  
  