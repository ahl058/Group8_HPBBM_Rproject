library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

#===TASK 1: Data pre-processing ===

# We'll upload the replicates for the data and rename 0D600 column to distinguish between replicates.

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
      #(Errors found while analyzing data already solved)

#Same but for Arabinose replicates files

replicate_Ara1<-read.table("data/growth_curves_Ara1.txt",header=TRUE,sep="\t")%>%
  rename(OD600_A1=OD600)

replicate_Ara2<-read.table("data/growth_curves_Ara2.txt", header=TRUE,sep="\t")%>%
  rename(OD600_A2=OD600)

replicate_Ara3<-read.table("data/growth_curves_Ara3.txt", header=TRUE,sep="\t")%>%
  rename(OD600_A3=OD600)

replicate_Ara4<-read.table("data/growth_curves_Ara4.txt", header=TRUE,sep="\t")%>%
  rename(OD600_A4=OD600)

      #(After checking in the console no error was found, only missing values in some cells, which has 
      #to be taken in account when doing operations beyond)


# We'll use the function full_join() that joins the columns matching by 'Cepa' and 'Tiempo'. 
gr_curves <- replicate_1 %>%

  full_join(replicate_2, by = c('Cepa', 'Tiempo')) %>%
  full_join(replicate_3, by=c('Cepa','Tiempo')) %>%
  full_join(replicate_4, by = c('Cepa','Tiempo'))

# Once we have all the OD600 columns added, we are going to arrange the rows following the order indicated. 
#First we define the order we want by turning the column cepa into factors and then establishing the levels
gr_curves$Cepa<-factor(gr_curves$Cepa,
                        levels=c("WT", "ΔpiPolB","ΔpiPolB+piPolB"))

gr_curves<-gr_curves %>%
  arrange(Cepa, Tiempo) %>%
  #Now we have all rows ordered
  
  select(Tiempo, Cepa, OD600_1, OD600_2, OD600_3, OD600_4) %>%
  
  # It makes the function to apply the function line per line. We could make it with the function rowMean() but we would have to apply it
  # to calculate the sd either way, so we calculate the mean with this structure too. 
  rowwise()%>%
  
  # We use mutate to create the mean and sd columns.
  mutate(OD600_mean = mean(c_across(OD600_1:OD600_4), na.rm = TRUE),
        OD600_sd = sd(c_across(OD600_1:OD600_4), na.rm = TRUE))%>%    
  ungroup()%>% #Close the line per line function.

  # rowwise converts it into a tibble
  as.data.frame() 
  
#gr_curves

#Now we have to repeat the work with the treated replicates:

gr_curves_Ara<-replicate_Ara1%>%
  full_join(replicate_Ara2,by= c('Cepa','Tiempo'))%>%
  full_join(replicate_Ara3, by= c('Cepa','Tiempo'))%>%
  full_join(replicate_Ara4, by= c('Cepa','Tiempo'))%>%
  full_join(replicate_Ara5, by= c('Cepa','Tiempo'))%>%
  full_join(replicate_Ara6, by= c('Cepa','Tiempo'))%>%
  full_join(replicate_Ara7, by= c('Cepa','Tiempo'))

#Again, we have to rearrange rows in the desired order

gr_curves_Ara$Cepa<-factor(gr_curves_Ara$Cepa,
                           levels=c("WT", "ΔpiPolB","ΔpiPolB+piPolB"))
gr_curves_Ara<-gr_curves_Ara %>%
  arrange(Cepa, Tiempo)

#Done this, we calculate mean and standard deviation
gr_curves_Ara<-gr_curves_Ara %>%
  select(Tiempo, Cepa, starts_with('OD600_'))%>%
  rowwise()%>%
  mutate(OD600_mean=mean(c_across(OD600_A1:OD600_A7),na.rm=TRUE),
         OD600_sd=sd(c_across(OD600_A1:OD600_A7), na.rm = TRUE))%>%
  ungroup()%>% #Close line per line function
  as.data.frame()

#gr_curves_Ara #Decomment this to show the generated dataframe
#(By this moment, task 1 is completed with both experiments(with and without Arabinose)

#===TASK2: Data visualization===

# This function is going to elongate the table, removing the four columns dedicated to each replicate to one, and create another column where 
# we are going to locate the name of each replicate for each value. We need to change this to graphicate.
gr_long <- gr_curves %>%
  pivot_longer(cols = OD600_1:OD600_4, names_to = "replica", values_to = "OD600") %>%
  as.data.frame()

# The following column plot represent all the replicates, all in one plot (the first)
# and separately. Also in the separate ones there are represented the points. Maybe
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
  labs(title = 'E. coli growth over time between mutants and WT (WITHOUT ARABINOSE)', 
        y = 'OD600 mean', x = 'Time')+
  scale_color_manual(values = c('WT' = "#E5989B", 
                                'ΔpiPolB+piPolB' = "#acf170",
                                'ΔpiPolB' = "#4ce7ec"))+
  theme_bw()


  
#Now we have to plot the Arabinose data. First of all, we have to adapt our data to a "long" dataframe useful in ggplot.

gr_long_Ara <- gr_curves_Ara %>%
  pivot_longer(cols = OD600_A1:OD600_A7, names_to = "replica", values_to = "OD600") %>%
  as.data.frame()

ggplot(gr_long_Ara, aes(x=Tiempo, y=OD600, fill = replica))+
  geom_col()+
  facet_wrap(.~Cepa)

#Now plot individually each phenotype and its replicates:

ggplot(subset(gr_long_Ara, Cepa == 'WT'),  aes(x=Tiempo, y=OD600, fill = replica))+
  geom_col(position = 'dodge', width = 0.4)+
  geom_point(size = 2, aes(alpha = 0.7))+
  facet_wrap(.~Cepa)

ggplot(subset(gr_long_Ara, Cepa == 'ΔpiPolB'),  aes(x=Tiempo, y=OD600, fill = replica))+
  geom_col(position = 'dodge', width = 0.4)+
  geom_point(size = 2, aes(alpha = 0.7))+
  facet_wrap(.~Cepa)

ggplot(subset(gr_long_Ara, Cepa == 'ΔpiPolB+piPolB'),  aes(x=Tiempo, y=OD600, fill = replica))+
  geom_col(position = 'dodge', width = 0.4)+
  geom_point(size = 2, aes(alpha = 0.7))+
  facet_wrap(.~Cepa)

#Now the last plot
ggplot(gr_curves_Ara, aes(x=Tiempo, y=OD600_mean, color = Cepa))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin = OD600_mean - OD600_sd, 
                    ymax = OD600_mean + OD600_sd))+
  facet_wrap(~Cepa)+
  labs(title = 'E. coli growth over time between mutants and WT (WITH ARABINOSE)', 
       y = 'OD600 mean', x = 'Time')+
  scale_color_manual(values = c('WT' = "#E5989B", 
                                'ΔpiPolB+piPolB' = "#acf170",
                                'ΔpiPolB' = "#4ce7ec"))+
  theme_bw()

#Now to compare both conditions:
#First, create the data frame ggplot function needs as argument:
gr_compare<-bind_rows(
  gr_curves %>% select(Tiempo, Cepa, OD600_mean, OD600_sd) %>% mutate(Condition="Whithout Arabinose"),
  gr_curves_Ara %>% select(Tiempo, Cepa, OD600_mean, OD600_sd)%>% mutate(Condition='With Arabinose'))

ggplot(gr_compare, aes(x=Tiempo, y=OD600_mean, color = Cepa))+
  geom_line(linewidth=0.8)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin = OD600_mean - OD600_sd, 
                    ymax = OD600_mean + OD600_sd),width=0.2)+
  facet_grid(Condition~Cepa)+
  labs(title = 'E. coli growth curves: testing Arabinose iPolB induction and phenotype rescue', 
       y = 'OD600 mean', x = 'Time')+
  scale_color_manual(values = c('WT' = "#E5989B", 
                                'ΔpiPolB+piPolB' = "#acf170",
                                'ΔpiPolB' = "#4ce7ec"))+
  theme_bw()

#===TASK3:Curve modelling and parametrization===
#Our data follow a sigmoid behavour so a simple exponential model doesn´t fit the curve.
#Gompertz is a better model for it. It include a parameter (K) for the maximum growth of bacteria.

