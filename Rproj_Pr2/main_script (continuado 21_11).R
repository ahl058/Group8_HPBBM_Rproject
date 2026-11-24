library(dplyr) # To execute, its necessary to install dplyr package, use the command install.packages('dplyr') in your console. 
adip_data <- read.table('data/adipodatav8.tsv',sep='\t',stringsAsFactors = TRUE, header=TRUE)
str(adip_data)

# In the following lines, we convert the variables into factor to make it easier to work with these variables
adip_data$mouse <- as.factor(adip_data$mouse)
adip_data$weeks <- as.factor(adip_data$weeks)
adip_data$image <- as.factor(adip_data$image)


# Task 1. We've created a new data frame which is much easier to work with because it just have the
# m.id, which has in it the treatment and the week (so we don't have to work with those variables separately).
# it also has the nº of mouse, the nº of image for each mouse and values obtained in each image. 
adip_clean<- data.frame('m.id'=adip_data$m.id,'mouse'=adip_data$mouse,'image'=adip_data$image,'pixel2'=adip_data$pixel2)
levels(adip_clean$m.id)[levels(adip_clean$m.id) == 'HFD.0'] <- 'HFD.7'
levels(adip_clean$m.id) <- sort(levels(adip_clean$m.id))

# With the package dplyr we're going to modify that data frame we've created. 

adip_per_img <- adip_clean %>%
  
  # This will group the variables hierarchically so that later we can calculate the mean sorting by this order.
  group_by(m.id, mouse, image) %>%  
  
  # It creates a new data frame (which we'll be substituted to adip_per_img) with the columns we grouped by and with the new column that we indicate, 
  # which operations we'll be executed with the whierarchy we indicate in the group_by
  summarise(total_img = sum(pixel2), num_drops=n(), mean_img = mean(pixel2), .groups = 'drop') %>%
  
  # Is transformed into a data frame because group_by creates a tibble, which does not show decimal numbers.
  as.data.frame()
  
 
# Task 2. Avrg adipocite size in the levels per image, per mouse, per group. The first one it's done above within the same
# summarise function to that of the sum of all measures per image. It is done like that because otherwise we'd have to make another group_by 
# and is less aesthetic. 
# Repeat the same operation as before for the next tasks. 
adip_per_mouse <- adip_clean %>%
  group_by(m.id, mouse) %>%
  summarise(mean_mouse = mean(pixel2), .groups = 'drop') %>%
  as.data.frame()

adip_per_group <- adip_clean %>%
  group_by(m.id)%>%
  summarise(mean_group = mean(pixel2), .groups='drop')%>%
  as.data.frame()

#Task 3. Our experimental unit are the 50 mice so we´re working on adip_per_mouse dataframe.
#First of all we have to create new variables dividing between groups and its respective controls:
mice_week4<-subset(adip_per_mouse,m.id %in% c("CTRL.4","HFD.4"))
mice_week7<-subset(adip_per_mouse,m.id%in% c("CTRL.7","HFD.7"))
mice_ctrl<-subset(adip_per_mouse,m.id %in% c("CTRL.0", "CTRL.4","CTRL.7"))

#We use aov function with the following arguments (including the data frame from where R has to take the data)
anova_test_week4<- aov(mean_mouse~m.id, data=mice_week4)
anova_test_week7<- aov(mean_mouse~m.id, data=mice_week7)
anova_test_ctrls<-aov( mean_mouse~m.id,data=mice_ctrl)

#summary function to see the values obtained in anova
anova4_result<-summary(anova_test_week4)
anova4_result

anova7_result<-summary(anova_test_week7)
anova7_result

anova_ctrl_result<-summary(anova_test_ctrls)
anova_ctrl_result

#According to the p-values, the differences are significant and the F values indicate that the differences between 
#week 4 and 7 and their groups are huge.
#(Cuando hagamos el informe en Markdown, comentar mejor los resulgtados)


#PREGUNTA. Qué test no paramétrico hay que usar?? hay alguno en los apuntes o tengo que buscar yo por mi cuenta?

