library(tidyverse)
library(baseballr)
library(ggplot2)

# Obtain the data from statcast via baseballr only once, save to .csv file.

#Get entire 2025 season of data for Sal Frelick's plate appearances.
# statcast_data_salfrelick <- statcast_search_batters(
#   start_date="2025-03-01",
#   end_date="2025-11-01",
#   batter = 686217
# )
# write_csv(statcast_data_salfrelick, 
#           '/home/kate/r_scripts/blog/baseball_batters_ind_files/statcast_batters_salfrelick_2025.csv')

statcast_data_salfrelick <- 
  read_csv('/home/kate/r_scripts/blog/baseball_batters_ind_files/statcast_batters_salfrelick_2025.csv')

# Reformat Sal's data as a tibble.
salfrelick <- as_tibble(statcast_data_salfrelick)
# Subset Sal's data to study only launch speeds.
salfrelick_launch_speed <- select(salfrelick, launch_speed)
# Remove plate appearance with no ball batted event.
clean_salfrelick_ls <- drop_na(salfrelick_launch_speed, launch_speed)
# Calculate the mean and standard deviation of Sal's launch speeds.
sf_avg_launch_speed <- mean(clean_salfrelick_ls$launch_speed)
sf_sd_launch_speed <- sd(clean_salfrelick_ls$launch_speed)
# Use a normal model to predict Sal's launch speeds 
# based only on mean and standard deviation.
predictions <- rnorm(n=16000, mean=sf_avg_launch_speed, sd=sf_sd_launch_speed)
sf_normal <- as_tibble(c(predictions))

# Create a denisty-type histogram 
# with a blue line delineating Sal's mean launch speed.
hist_with_mean <- ggplot(clean_salfrelick_ls, aes(x=launch_speed, after_stat(density))) +
  geom_histogram(fill='skyblue', bins=30) +
  # vertical line at Sal's mean launch speed 
  geom_vline(aes(xintercept=sf_avg_launch_speed), color='blue') +
  # create 30 bins each containing a range of Sal's launch speeds
  scale_x_continuous(n.breaks=30) + 
  ggtitle("Sal Frelick's Launch Speeds in 2025")+
  theme(plot.title=element_text(hjust=.5)) +
  coord_cartesian(xlim=c(18,125)) +
  labs(x="Launch speeds", 
       y="Probability of each\nrange of launch speeds")

ggsave('/home/kate/r_scripts/blog/baseball_1/EV50/frelick_launchspeeds_hist.png'
       , plot=hist_with_mean
       , width=8
       ,height=3)

# Create a denisty-type histogram 
# with a blue line delineating Sal's mean launch speed and
# a predicted normal curve based on Sal's mean and standard deviation.
hist_with_normal <- ggplot(clean_salfrelick_ls, aes(x=launch_speed, after_stat(density))) +
  geom_histogram(fill='skyblue', bins=30) +
  geom_vline(aes(xintercept=sf_avg_launch_speed), color='blue') +
  geom_density(data=sf_normal, 
                aes(x=predictions), 
                color='black', alpha=.6) +
  scale_x_continuous(n.breaks=30) + 
  labs(title="Sal Frelick's Launch Speeds in 2025",
       subtitle="and randomly generated normal curve parameterized\nwith the mean and variance of Sal's launch speeds",
       x="Launch speeds", 
       y="Probability of each\nrange of launch speeds") + 
  theme(plot.title=element_text(hjust=.5), 
        plot.subtitle=element_text(hjust=.5, size=8)) +
  coord_cartesian(xlim=c(18,125))

ggsave('/home/kate/r_scripts/blog/baseball_1/EV50/frelick_launchspeeds_hist_normal.png'
       , plot=hist_with_normal
       , width=8
       ,height=3)
