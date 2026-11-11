
################################################################################
## Load the required packages for the practical
################################################################################
## Load the required packages for the practical
# If the packages have not been installed, please use the command install.packages('package_name')

library('ggplot2') # We will use ggplot2 for plotting figures
library('patchwork') # We will use patchwork for creating multi-panel plots
library('EpiNow2') # We will use EpiNow2 for estimating the reproduction number from a time-series

######################################################################################################################################################
# 1. Reading in and visualising the data
######################################################################################################################################################
# Read in the dataset 'simulated_data.csv'
# The data set includes the daily infection incidence

df1 <- read.csv('data/simulated_data.csv')
df1$date <- as.Date(df1$date)

################################################################################
# 1.a To complete
################################################################################
# Visualise the time-series of infection incidence as a figure


plt1 <- ggplot(df1, aes(x=date, y=incidence))+
  geom_point()+
  geom_line()+
  theme_bw(base_size=14)+
  xlab("Time (days)")+
  ylab("Infection incidence")


plt1

#####################################################################################################################################################
# 2. Use EpiNow2 to estimate the smoothed trend in the infection incidence
#####################################################################################################################################################
# We will make use of the EpiNow2 package to estimate smoothed trends in  the
# infection incidence.

# EpiNow2 requires that the generation distribution is defined.
# We will assume that the generation interval is given by a Gamma distribution
# with mean of 5 days and standard deviation of 2 days.
gamma_distribution <- EpiNow2::Gamma(mean=5, sd=2, max=14)

model1 <- epinow(
  data = data.frame(date=df1$date, confirm=df1$incidence),
  generation_time = gt_opts(gamma_distribution),
  delays = delay_opts(Fixed(0)),
  stan = stan_opts(cores = 4,
                   samples = 1500,
                   warmup = 500),
  forecast = forecast_opts(horizon=0),
  rt = rt_opts(rw=1)
)

saveRDS(model1, 'epinow_model_fits/model_fit_example.rds')
model_fit_example <- readRDS('epinow_model_fits/model_fit_example.rds')

summary_example <- model_fit_example$estimates$summarised

plt_inc1 <- ggplot()+
  geom_line(data = summary_example[summary_example$variable=="reported_cases",], aes(x=date, y=median))+
  geom_ribbon(data = summary_example[summary_example$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary_example[summary_example$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  theme_bw(base_size=14)+
  xlab("Date")+
  ylab("Infection incidence")

plt_inc1

################################################################################
# 2.a To complete
################################################################################
# Create a new figure that is the same as above but also includes the infection
# incidence data

plt_inc2 <- ggplot()+
  geom_line(data = summary_example[summary_example$variable=="reported_cases",], aes(x=date, y=median))+
  geom_ribbon(data = summary_example[summary_example$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary_example[summary_example$variable=="reported_cases",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  theme_bw(base_size=14)+
  geom_point(data=df1, aes(x=date, y=incidence))+
  xlab("Date")+
  ylab("Infection incidence")

plt_inc2

################################################################################
# 2.b To complete
################################################################################
# The summary_example object also includes EpiNow2's modelled estimates of the Reproduction 
# number (variable==R) and the growth rate (variable==growth_rate)
#
# Visualise how the growth rate and reproduction number varied over time in two
# separate figures.
#
# On each figure mark the threshold for epidemic growth/decline with a horizontal 
# dashed line


plt_gr <- ggplot()+
  geom_line(data = summary_example[summary_example$variable=="growth_rate",], aes(x=date, y=median))+
  geom_ribbon(data = summary_example[summary_example$variable=="growth_rate",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary_example[summary_example$variable=="growth_rate",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  geom_hline(yintercept = 0, linetype="dashed")+
  theme_bw(base_size=14)+
  xlab("Date")+
  ylab("Growth rate")

plt_gr

plt_R <- ggplot()+
  geom_line(data = summary_example[summary_example$variable=="R",], aes(x=date, y=median))+
  geom_ribbon(data = summary_example[summary_example$variable=="R",], aes(x=date, y=median, ymin=lower_50, ymax=upper_50), alpha=0.2)+
  geom_ribbon(data = summary_example[summary_example$variable=="R",], aes(x=date, y=median, ymin=lower_90, ymax=upper_90), alpha=0.2)+
  geom_hline(yintercept = 1, linetype="dashed")+
  theme_bw(base_size=14)+
  xlab("Date")+
  ylab("Reproduction number")

plt_R

################################################################################
# 2.c To complete
################################################################################
# Plot infection incidence, growth rate, and reproduction number as one figure 
# with multiple panels. 

plt_inc2 + plt_gr + plt_R +plot_layout(nrow=3)


