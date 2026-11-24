library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
# We'll upload the repicates for the data
replicate_1 <- read.table('data/growth_curves_1.txt', sep='\t', header = TRUE) %>%
  rename(OD600_1 = OD600)
replicate_2 <- read.table('data/growth_curves_2.txt', sep='\t', header = TRUE) %>%
  rename(OD600_2 = OD600)
replicate_3 <- read.table('data/growth_curves_3.txt', sep='\t', header = TRUE)%>%
  rename(OD600_3 = OD600)
replicate_3$Cepa[24] <- 'ΔpiPolB' # It is necessary to change this value because it is incorrect in the data. 
replicate_3 <- replicate_3 %>%
  arrange(Cepa == 'ΔpiPolB+piPolB') # The function arrange with a boolean will make the true conditions to locate at the end. 

replicate_4 <- read.table('data/growth_curves_4.txt', sep='\t', header = TRUE)%>%
  rename(OD600_4 = OD600)


gr_curves <- replicate_1 %>%

  # We'll use the function full_join() that joins the columns matching by 'Cepa' and 'Tiempo'. 
  full_join(replicate_2, by = c('Cepa', 'Tiempo')) %>%
  full_join(replicate_3, by=c('Cepa','Tiempo')) %>%
  full_join(replicate_4, by = c('Cepa','Tiempo')) %>%

  # Once we have all the OD600 columns added, we are going to arrange the rows following the order indicated. 
  arrange(Cepa != 'WT', Cepa, Tiempo) %>%
  select(Tiempo, Cepa, OD600_1, OD600_2, OD600_3, OD600_4) %>%
  
  # It makes thet function to apply the function line per line. We could make it with the function rowMean() but we would have to apply it
  # to calculate the sd either way, soy we calculate the mean with this structure too. 
  rowwise()%>%
  
  # We use mutate to create the mean and sd columns.
  mutate(OD600_mean = mean(c_across(OD600_1:OD600_4), na.rm = TRUE),
        OD600_sd = sd(c_across(OD600_1:OD600_4), na.rm = TRUE))%>%    
  ungroup()%>%

  # rowwise converts it into a tibble
  as.data.frame() 

gr_curves

# This function is going to elongate the table, removing the four columns dedicated to each replicate to one, and create another column where 
# we are going to locate the name of each replicate for each value. 
gr_long <- gr_curves %>%
  pivot_longer(cols = OD600_1:OD600_4, names_to = "replica", values_to = "OD600") %>%
  as.data.frame()

# The following column plot represent all the replicates, all in one plot (the first)
# and separately. Also in the separate ones there are represented de points. Maybe
# it'll help to identificate the outliers. 
ggplot(gr_long, aes(x=Tiempo, y=OD600, fill = replica))+
  geom_col()+
  facet_wrap(.~Cepa)

ggplot(subset(gr_long, Cepa == 'WT'),  aes(x=Tiempo, y=OD600, fill = replica))+
  geom_col(position = 'dodge', width = 0.4)+
  geom_point(size = 2, aes(alpha = 0.7))+
   facet_wrap(.~Cepa)

ggplot(subset(gr_long, Cepa == 'ΔpiPolB'),  aes(x=Tiempo, y=OD600, fill = replica))+
  geom_col(position = 'dodge', width = 0.4)+
  geom_point(size = 2, aes(alpha = 0.7))+
   facet_wrap(.~Cepa)


ggplot(subset(gr_long, Cepa == 'ΔpiPolB+piPolB'),  aes(x=Tiempo, y=OD600, fill = replica))+
  geom_col(position = 'dodge', width = 0.4)+
  geom_point(size = 2, aes(alpha = 0.7))+
   facet_wrap(.~Cepa)


# This plot it to see the groth tendency with the error bar represented. 
ggplot(gr_curves, aes(x=Tiempo, y=OD600_mean, color = Cepa))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin = OD600_mean - OD600_sd, 
                ymax = OD600_mean + OD600_sd))+
  facet_wrap(~Cepa)+
  labs(title = 'E. coli growth over time between mutants and WT', 
        y = 'OD600 mean', x = 'Time')+
  scale_color_manual(values = c('WT' = "#E5989B", 
                                'ΔpiPolB+piPolB' = "#acf170",
                                'ΔpiPolB' = "#4ce7ec"))+
  theme_bw()

