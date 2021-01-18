# Load library
library(tidyverse)
library(lubridate)

# Data source: https://nrc.canada.ca/en/research-development/products-services/software-applications/sun-calculator/
#df <- read_csv('yyz_sunrise.csv', skip = 1) %>%
 # mutate(nDate = mdy(Date))

#ggplot(data = df) +
 # geom_line(mapping = aes(x = nDate, y = `Sun set`))

# Angle source: https://jonathancritchley.ca/height.html

# Toronto angles

# Setup to read files from angle_data folder
data_path <- "angle_data"
files <- dir(data_path, pattern = "*.csv")

# Read in monthly tables into single tibble, adding filename as a column
angles <- tibble(filename = files) %>%
  mutate(month = map(filename,
                     ~ read_csv(file.path(data_path, .), skip_empty_rows = TRUE)))
df_angles <- unnest(angles, cols = c(month)) %>%
  drop_na(Time) %>%
  # Check if it falls between min and max unblocked height
  mutate(height_check = ifelse(between(`Solar Altitude`,19,51),1,0),
         # Check if azimuth falls within unblocked range
         azi_check = ifelse(between(`Solar Azimuth`,165,223),1,0),
         # Indicate when both height and azimuth are unblocked
         direct_sun = height_check * azi_check)

# Find total unblocked minutes in each month (each observation is a 15min increment)
sum_monthly <- df_angles %>%
  group_by(filename) %>%
  summarise(sun_hr = sum(direct_sun)*15/60) %>%
  mutate(Month = substr(filename, start = 1, stop = 3))

# max angle: tan-1(3.1/2.5) = 51deg
# min angle: tan-1(14/36) = 21deg


sum_monthly$Month <- factor(sum_monthly$Month,levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

# Chart
ggplot(sum_monthly) +
  geom_col(mapping = aes(x = Month, y = sun_hr)) +
  theme_minimal() +
  labs(title = "Hours of Direct Sunlight into my Apartment",
       caption = "@carsonfong | Data Sources: NRC Canada Sunrise/sunset calculator, Saroglou (2017), Critchley (2016)",
       y = "Protein (g) in 100g of Food",
       x = "Calories in 100g of Food")
