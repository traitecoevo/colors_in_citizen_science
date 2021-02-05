#This R script uses 3 base files to produce figures 2-5
#"upper_breast.csv" (colorZapper data), "our_species.csv" (spec data), and "bird_colours.csv" (colour information)
#Also includes code to produce plant figure 6, which uses 14 base files
#marked with "SP" (Senna pendula) and "MA" (Macroptilium atropurpureum)

#Note that this R script needs to be run in order from beginning to end
#Some sections rely on data frames created ahead

#load packages
library(readr)
library(dplyr)
library(ggplot2)
library(pavo)
library(data.table)
library(tibble)
library(tidyr)
library(broom)

#read in files from the folder "r_files" within the working directory - change this if files have been saved elsewhere
colorZapper_data <- read.csv("r_files/upper_breast.csv")
spec_data <- read.csv("r_files/our_species.csv")

#convert spec data into RGB
    #first we organise the data
spec_dat <- spec_data%>%
  dplyr::select(9:89) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var="Wavelength") %>%
  mutate(Wavelength=as.integer(gsub("wl", "", .$Wavelength)))
    #we only want the wavelengths that will be converted into RGB colour
specmsmt_limit <- as.rspec(spec_dat, lim = c(300, 700), exceed.range = TRUE)
    #fix the measurements that are below zero due to error
specmsmt_limit_fixed <- procspec(specmsmt_limit, fixneg = "zero")
   #convert spec measurements into Hex colour codes
rgb_values <- spec2rgb(specmsmt_limit_fixed)
spec_rgb <- data.frame(hexCol=rgb_values, 
                         species=spec_data$species,
                         patch=spec_data$patch.name)
   #now we write a function to obtain RGB measurements from the Hex colour codes
get_RGB_function <- function(i){
  
  temp_dat <- spec_rgb %>%
    slice(i)
  
  red=col2rgb(temp_dat$hexCol)[1]
  green=col2rgb(temp_dat$hexCol)[2]
  blue=col2rgb(temp_dat$hexCol)[3]
  
  dat_out <- data.frame(species=temp_dat$species,
                        patch=temp_dat$patch,
                        hexCol=temp_dat$hexCol,
                        red=red,
                        green=green,
                        blue=blue,
                        row=i)
  
}
    #apply this function to every row
results_list <- lapply(c(1:nrow(spec_rgb)), function(x){get_RGB_function(x)})

#bind the results into a data frame
#we obtained mean RGB measurements for each species
spec_rgb_final <- bind_rows(results_list) %>%
  dplyr::select(-row, -hexCol) %>%
  group_by(species, patch) %>%
  summarize(red=mean(red),
            green=mean(green),
            blue=mean(blue)) %>%
  mutate(Source="Museum")

#organise colorZapper_data to obtain data frame with matching format with spec_rgb_final
zapped_rgb_final <- colorZapper_data %>%
  dplyr::select(species, id, mark, mean_red, mean_green, mean_blue) %>%
  rename(patch=mark) %>%
  rename(red=mean_red) %>%
  rename(green=mean_green) %>%
  rename(blue=mean_blue) %>%
  mutate(Source="Citizen science") %>%
  dplyr::filter(complete.cases(species))

#combine the two - this will be our first data frame for plotting bird figures
dataframe_for_plotting <- zapped_rgb_final %>%
  right_join(., spec_rgb_final, by=c("species", "patch"))

#this is our second data frame for plotting bird figures
plot_dat <- bind_rows(spec_rgb_final,
                      zapped_rgb_final)
        
#PLOTTING FIGURE 2
  #create a museum data frame with RGB columns as rows instead
museum <- plot_dat %>%
  dplyr::filter(Source == "Museum") %>%
  pivot_longer(cols=c(red, green, blue),
               names_to="colour", 
               values_to="value") %>%
  pivot_wider(names_from=Source,
              values_from=value) %>%
  ungroup() %>%
  dplyr::select(-id) %>%
  dplyr::filter(complete.cases(.))
    #create a citizen science data frame with RGB columns as rows instead
citizen_science <- plot_dat %>%
  dplyr::filter(Source == "Citizen science") %>%
  pivot_longer(cols=c(red, green, blue),
               names_to="colour", 
               values_to="value") %>%
  pivot_wider(names_from=Source,
              values_from=value) %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.))
    #join the two
plot_dat.2 <- museum %>%
  left_join(., citizen_science)
    #this is to reiterate the order of the colour components of RGB
plot_dat.2$colour <- factor(plot_dat.2$colour, levels=c("red", "green", "blue"), labels=c("red", "green", "blue"))
    #now we plot
plot_dat.2 %>%
  group_by(species, colour, patch) %>%
  summarize(Museum=mean(Museum),
            `Citizen science`=mean(`Citizen science`)) %>%
  ggplot(., aes(x=Museum, y=`Citizen science`, colour=colour, label=species),)+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  scale_colour_manual(values = c("firebrick1", "green2", "royalblue1"), labels = c("R", "G", "B"))+
  labs(color="")+
  labs(y="Citizen science", x="Museum")+
  theme_bw()+
  theme(axis.text=element_text(color="black"), text = element_text(size=15))+
  xlim(0,255)+
  ylim(0,255) -> fig2

#preview figure 2
fig2

#PLOTTING FIGURE 3
    #obtain species mean for citizen science RGB measurements
zapped_rgb_mean <- aggregate(zapped_rgb_final[, 4:6], list(zapped_rgb_final$species), mean)
colnames(zapped_rgb_mean)[1] <- "species"
colnames(zapped_rgb_mean)[2] <- "cs_red"
colnames(zapped_rgb_mean)[3] <- "cs_green"
colnames(zapped_rgb_mean)[4] <- "cs_blue"
    #organise spec RGB data into similar format and join
all_mean <- spec_rgb_final %>%
  rename(museum_red=red) %>%
  rename(museum_green=green)%>%
  rename(museum_blue=blue) %>%
  select(1, 3:5) %>%
  full_join(., zapped_rgb_mean, by="species") %>%
  drop_na()
    
#get a list of species
species_order <- rownames_to_column(all_mean, var="row") %>%
  select(1:2)

#count number of photos per species and rename columns
how_many_photos <- as.data.frame(table(dataframe_for_plotting$species))
colnames(how_many_photos) <- c("species", "frequency")

#obtain residuals for each RGB component, adding new columns for colour, absolute values, and squared residuals, then subsetting
    #R (red)
resid_mean_red <-all_mean %>%
  lm(cs_red ~ museum_red, data=.) %>%
  augment() %>%
  rownames_to_column(., var="row") %>%
  full_join(., species_order, by="row") %>%
  full_join(., how_many_photos, by="species")
resid_mean_red["colour"]<- "red"
resid_mean_red["absresid"]<- abs(resid_mean_red$.resid)
resid_mean_red["sqresid"]<- (resid_mean_red$.resid)^2
resid_mean_red_subset <- select(resid_mean_red, 11:14)
    #G (green)
resid_mean_green <-all_mean %>%
  lm(cs_green ~ museum_green, data=.) %>%
  augment()%>%
  rownames_to_column(., var="row")%>%
  full_join(., species_order, by="row") %>%
  full_join(., how_many_photos, by="species")
resid_mean_green["colour"]<- "green"
resid_mean_green["absresid"]<- abs(resid_mean_green$.resid)
resid_mean_green["sqresid"]<- (resid_mean_green$.resid)^2
resid_mean_green_subset <- select(resid_mean_green, 11:14)
    #B (blue)
resid_mean_blue <-all_mean %>%
  lm(cs_blue ~ museum_blue, data=.) %>%
  augment()%>%
  rownames_to_column(., var="row")%>%
  full_join(., species_order, by="row") %>%
  full_join(., how_many_photos, by="species")
resid_mean_blue["colour"]<- "blue"
resid_mean_blue["absresid"]<- abs(resid_mean_blue$.resid)
resid_mean_blue["sqresid"]<- (resid_mean_blue$.resid)^2
resid_mean_blue_subset <- select(resid_mean_blue, 11:14)

#bind together
resid_mean_all_subset <- bind_rows(resid_mean_red_subset, resid_mean_blue_subset, resid_mean_green_subset)

#R thinks frequency is a continuous numerical variable so we have to do something about it
resid_mean_all_subset$frequency <- as.factor(resid_mean_all_subset$frequency)

#get the colour order to appear correctly
resid_mean_all_subset$colour <- factor(resid_mean_all_subset$colour, levels=c("red", "green", "blue"), labels=c("red", "green", "blue"))

#plot figure 3
group_by(resid_mean_all_subset, frequency, colour) %>%
  summarise(mean_photo=mean(absresid)) %>%
  ggplot(aes(x=frequency, y=mean_photo, group=colour, colour=colour)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  xlab("Number of photos per species") +
  ylab("Mean absolute residuals") +
  scale_colour_manual(breaks = c("red", "green", "blue"), values=c("firebrick1", "green2", "royalblue1"), labels = c("R", "G", "B"))+
  labs(color="")+
  theme_bw()+
  theme(axis.text=element_text(color="black"), text = element_text(size=15)) -> fig3

#preview figure 3
fig3

#PLOTTING FIGURE 4
#there are 4 components that make up figure 4 (a, b, c, d)
#this R script will mostly exclude non-essential code (e.g. to collate 4 components together)

#obtain data for spec RGB values that are not averaged for each species, and add row numbers to new column
spec_rgb_final_indiv_values <- bind_rows(results_list) %>%
  dplyr::select(-row, -hexCol) %>%
  group_by(species, patch) %>%
  tibble::rownames_to_column(var="row")

#select only the specimen column, and add row numbers to new column
spec_data_only_ID <- spec_data %>%
  dplyr::select(3) %>%
  tibble::rownames_to_column(var="row")

#join the two
spec_rgb_indiv_with_ID <- full_join(spec_rgb_final_indiv_values, spec_data_only_ID, by="row")

#obtain corresponding year of collection for each museum specimen
spec_year <- select(spec_data, specimen, year)

#join specimen collection year with RGB measurements, filtering out unwanted entries with problematic values
spec_ID_and_year <- full_join(spec_rgb_indiv_with_ID, spec_year, by="specimen") %>%
  drop_na(species) %>%
  dplyr::filter(year>"0")

#join the above with mean RGB measurements for each species, cleaning up NA values again
spec_indiv_vs_mean <- full_join(spec_ID_and_year, spec_rgb_final, by="species") %>%
  drop_na(row)

#obtain residuals for individual specimen vs. species mean measurements for R/G/B, add column for year
    #R (red)
spec_indiv_vs_mean %>%
  rename(indiv_red=red.x,
         indiv_green=green.x,
         indiv_blue=blue.x,
         mean_red=red.y,
         mean_green=green.y,
         mean_blue=blue.y) %>%
  ungroup() %>%
  lm(indiv_red ~ mean_red, data=.) %>%
  augment() -> resids_spec_red
resids_spec_red['year']= spec_indiv_vs_mean['year']
    #G (green)
spec_indiv_vs_mean %>%
  rename(indiv_red=red.x,
         indiv_green=green.x,
         indiv_blue=blue.x,
         mean_red=red.y,
         mean_green=green.y,
         mean_blue=blue.y) %>%
  ungroup() %>%
  lm(indiv_green ~ mean_green, data=.) %>%
  augment() -> resids_spec_green
resids_spec_green['year']= spec_indiv_vs_mean['year']
    #B (blue)
spec_indiv_vs_mean %>%
  rename(indiv_red=red.x,
         indiv_green=green.x,
         indiv_blue=blue.x,
         mean_red=red.y,
         mean_green=green.y,
         mean_blue=blue.y) %>%
  ungroup() %>%
  lm(indiv_blue ~ mean_blue, data=.) %>%
  augment() -> resids_spec_blue
resids_spec_blue['year']= spec_indiv_vs_mean['year']

#plot residuals (y) against year of collection (x) to obtain FIGURES 4A, B, C
    #R (red)
ggplot(resids_spec_red, aes(x=year, y=.resid))+
  geom_point(colour ="firebrick1")+
  theme_bw()+
  theme(axis.text=element_text(color="black"), text = element_text(size=13))+
  xlab("Year")+
  ylab("Residuals") +
  geom_smooth(method="lm", colour="black") -> spec_red_plot
    #G (green)
ggplot(resids_spec_green, aes(x=year, y=.resid))+
  geom_point(colour ="green2")+
  theme_bw()+
  theme(axis.text=element_text(color="black"), text = element_text(size=13))+
  xlab("Year")+
  ylab("Residuals") +
  geom_smooth(method="lm", colour="black") -> spec_green_plot
    #B (blue)
ggplot(resids_spec_blue, aes(x=year, y=.resid))+
  geom_point(colour ="royalblue1")+
  theme_bw()+
  theme(axis.text=element_text(color="black"), text = element_text(size=13))+
  xlab("Year")+
  ylab("Residuals") +
  geom_smooth(method="lm", colour="black") -> spec_blue_plot

#preview these plots for FIGURES 4A, B, C
spec_red_plot
spec_green_plot
spec_blue_plot

#to obtain R2 and p values
summary(lm(.resid~year, resids_spec_red))
summary(lm(.resid~year, resids_spec_green))
summary(lm(.resid~year, resids_spec_blue))

#read in files from the folder "r_files" within the working directory - change this if files have been saved elsewhere
bc<-read_csv("r_files/bird_colours.csv")

#data manipulation steps to obtain specific workable format for bc data frame
bc$colours_cs <- gsub(" ","",bc$colours_cs)
color_list<-unique(unlist(strsplit(bc$colours_cs,",")))
color_list<-color_list[!is.na(color_list)]
for (i in 1:length(color_list)){
  bc[,color_list[i]] <- grepl(color_list[i],bc$colours_cs)
}

#first we obtain data frame that shows whether each species is patterned or not
patterned_or_not <- bc %>%
  select(2, 7) 

#merge this with our main dataframe for plotting (RGB measurements for museum and citizen science)
data_patterned_or_not <- merge(dataframe_for_plotting, patterned_or_not, by="species")

#obtain two separate data frames from the above, one for patterned birds and another for unpatterned birds
data_only_patterned <- subset(data_patterned_or_not, patterned == "Y")
data_only_not_patterned <- subset(data_patterned_or_not, is.na(patterned))

#obtain residuals from linear models
    #patterned birds in order of R, G, then B
      #R
data_only_patterned %>%
  rename(Citizen_science_red=red.x,
         Citizen_science_green=green.x,
         Citizen_science_blue=blue.x,
         Museum_red=red.y,
         Museum_green=green.y,
         Museum_blue=blue.y) %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  lm(Citizen_science_red ~ Museum_red, data=.) %>%
  augment() -> resids_red_only_patterned
      #G
data_only_patterned %>%
  rename(Citizen_science_red=red.x,
         Citizen_science_green=green.x,
         Citizen_science_blue=blue.x,
         Museum_red=red.y,
         Museum_green=green.y,
         Museum_blue=blue.y) %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  lm(Citizen_science_green ~ Museum_green, data=.) %>%
  augment() -> resids_green_only_patterned
      #B
data_only_patterned %>%
  rename(Citizen_science_red=red.x,
         Citizen_science_green=green.x,
         Citizen_science_blue=blue.x,
         Museum_red=red.y,
         Museum_green=green.y,
         Museum_blue=blue.y) %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  lm(Citizen_science_blue ~ Museum_blue, data=.) %>%
  augment() -> resids_blue_only_patterned
    #unpatterned birds in order of R, G, then B
      #R
data_only_not_patterned %>%
  rename(Citizen_science_red=red.x,
         Citizen_science_green=green.x,
         Citizen_science_blue=blue.x,
         Museum_red=red.y,
         Museum_green=green.y,
         Museum_blue=blue.y) %>%
  ungroup() %>%
  lm(Citizen_science_red ~ Museum_red, data=.) %>%
  augment() -> resids_red_unpatterned
      #G
data_only_not_patterned %>%
  rename(Citizen_science_red=red.x,
         Citizen_science_green=green.x,
         Citizen_science_blue=blue.x,
         Museum_red=red.y,
         Museum_green=green.y,
         Museum_blue=blue.y) %>%
  ungroup() %>%
  lm(Citizen_science_green ~ Museum_green, data=.) %>%
  augment() -> resids_green_unpatterned
      #B
data_only_not_patterned %>%
  rename(Citizen_science_red=red.x,
         Citizen_science_green=green.x,
         Citizen_science_blue=blue.x,
         Museum_red=red.y,
         Museum_green=green.y,
         Museum_blue=blue.y) %>%
  ungroup() %>%
  lm(Citizen_science_blue ~ Museum_blue, data=.) %>%
  augment() -> resids_blue_unpatterned

#now we want to create a boxplot
#boxplot data frame format = one column for residuals and one column for patterned/unpatterned
#but we also want R, G, B all in one plot, so 6 boxes in total with patterned & unpatterned grouped together for each component
#our data frame should have 3 columns: type(patterned/unpatterned) to FILL, colour(R/G/B) for axis 1, and residuals for axis 2

#for each resid data frame, we add columns to differentiate patch type (patterned or not) and colour component (R/G/B)
#then we subset to keep only necessary columns
resids_red_unpatterned["type"] <- "unpatterned"
resids_red_unpatterned["colour"] <- "red"
resids_red_unpatterned_subset <- select(resids_red_unpatterned, 6, 11, 12)
resids_green_unpatterned["type"] <- "unpatterned"
resids_green_unpatterned["colour"] <- "green"
resids_green_unpatterned_subset <- select(resids_green_unpatterned, 6, 11, 12)
resids_blue_unpatterned["type"] <- "unpatterned"
resids_blue_unpatterned["colour"] <- "blue"
resids_blue_unpatterned_subset <- select(resids_blue_unpatterned, 6, 11, 12)
resids_red_only_patterned["type"] <- "patterned"
resids_red_only_patterned["colour"] <- "red"
resids_red_only_patterned_subset <- select(resids_red_only_patterned, 5, 10, 11)
resids_green_only_patterned["type"] <- "patterned"
resids_green_only_patterned["colour"] <- "green"
resids_green_only_patterned_subset <- select(resids_green_only_patterned, 5, 10, 11)
resids_blue_only_patterned["type"] <- "patterned"
resids_blue_only_patterned["colour"] <- "blue"
resids_blue_only_patterned_subset <- select(resids_blue_only_patterned, 5, 10, 11)

#join these together
patterned_unpatterned_for_boxplot <- bind_rows(resids_red_only_patterned_subset, resids_red_unpatterned_subset, resids_green_only_patterned_subset, resids_green_unpatterned_subset, resids_blue_only_patterned_subset, resids_blue_unpatterned_subset)

#plot FIGURE 4D
fig4d <- ggplot(patterned_unpatterned_for_boxplot, aes(x=colour, y=.resid, fill=type)) +
  geom_boxplot() +
  xlab("") +
  ylab("Residuals") +
  coord_flip() +
  labs(fill= "patch type") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))+
  scale_fill_manual(values=c("grey", "white"))

#preview FIGURE 4D
fig4d

#PLOTTING FIGURE 5
#there are 2 components that make up figure 5 (a, b)
#this R script will mostly exclude non-essential code (e.g. to collate 2 components together)

#we need to create a boxplot with colour family on the y axis and residuals on the x axis
#first, obtain residuals from linear models of citizen science vs museum species mean measurements for R, G, and B
    #R
dataframe_for_plotting %>%
  rename(Citizen_science_red=red.x,
         Citizen_science_green=green.x,
         Citizen_science_blue=blue.x,
         Museum_red=red.y,
         Museum_green=green.y,
         Museum_blue=blue.y) %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  lm(Citizen_science_red ~ Museum_red, data=.) %>%
  augment() %>%
  mutate(id=dataframe_for_plotting %>%
           rename(Citizen_science_red=red.x,
                  Citizen_science_green=green.x,
                  Citizen_science_blue=blue.x,
                  Museum_red=red.y,
                  Museum_green=green.y,
                  Museum_blue=blue.y) %>%
           ungroup() %>% 
           .$id) %>%
  arrange(desc(abs(.resid))) -> resids_red
    #G
dataframe_for_plotting %>%
  rename(Citizen_science_red=red.x,
         Citizen_science_green=green.x,
         Citizen_science_blue=blue.x,
         Museum_red=red.y,
         Museum_green=green.y,
         Museum_blue=blue.y) %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  lm(Citizen_science_green ~ Museum_green, data=.) %>%
  augment() %>%
  mutate(id=dataframe_for_plotting %>%
           rename(Citizen_science_red=red.x,
                  Citizen_science_green=green.x,
                  Citizen_science_blue=blue.x,
                  Museum_red=red.y,
                  Museum_green=green.y,
                  Museum_blue=blue.y) %>%
           ungroup() %>% 
           .$id) %>%
  arrange(desc(abs(.resid))) -> resids_green
    #B
dataframe_for_plotting %>%
  rename(Citizen_science_red=red.x,
         Citizen_science_green=green.x,
         Citizen_science_blue=blue.x,
         Museum_red=red.y,
         Museum_green=green.y,
         Museum_blue=blue.y) %>%
  ungroup() %>%
  dplyr::filter(complete.cases(.)) %>%
  lm(Citizen_science_blue ~ Museum_blue, data=.) %>%
  augment() %>%
  mutate(id=dataframe_for_plotting %>%
           rename(Citizen_science_red=red.x,
                  Citizen_science_green=green.x,
                  Citizen_science_blue=blue.x,
                  Museum_red=red.y,
                  Museum_green=green.y,
                  Museum_blue=blue.y) %>%
           ungroup() %>% 
           .$id) %>%
  arrange(desc(abs(.resid))) -> resids_blue

#merge colorZapper_data with colours and species on bc, then subset to keep only necessary columns
bc_subset <- select(bc, 2, 11:23)
cd_with_colours <- full_join(bc_subset, colorZapper_data, by="species")
cd_with_colours_subset <- select(cd_with_colours, 1:14, 16)

#merge thrice, with each resid data frame, and drop incomplete entries
resids_red_with_colours <- full_join(resids_red, cd_with_colours_subset, by="id") %>%
  dplyr::filter(complete.cases(.))
resids_green_with_colours <- full_join(resids_green, cd_with_colours_subset, by="id") %>%
  dplyr::filter(complete.cases(.))
resids_blue_with_colours <- full_join(resids_blue, cd_with_colours_subset, by="id") %>%
  dplyr::filter(complete.cases(.))

#get resid values column next to column of corresponding colour families (e.g. row 1: 50, brown; row 2: 20, white)
resids_red_with_colours_true <- pivot_longer(resids_red_with_colours, 12:24) %>%
  dplyr::filter(value=="TRUE")
resids_green_with_colours_true <- pivot_longer(resids_green_with_colours, 12:24) %>%
  dplyr::filter(value=="TRUE")
resids_blue_with_colours_true <- pivot_longer(resids_blue_with_colours, 12:24) %>%
  dplyr::filter(value=="TRUE")

#add column RGB to each data frame for FILL
resids_red_with_colours_true["RGB"] <- "R"
resids_red_with_colours_true_subset <- select(resids_red_with_colours_true, 5, 12, 14)
resids_green_with_colours_true["RGB"] <- "G"
resids_green_with_colours_true_subset <- select(resids_green_with_colours_true, 5, 12, 14)
resids_blue_with_colours_true["RGB"] <- "B"
resids_blue_with_colours_true_subset <- select(resids_blue_with_colours_true, 5, 12, 14)
colourfamilies_all_joined <- bind_rows(resids_red_with_colours_true_subset, resids_green_with_colours_true_subset, resids_blue_with_colours_true_subset)

#plot FIGURE 5A
fig5a <-ggplot(colourfamilies_all_joined, aes(x=name, y=.resid, fill=RGB)) +
  geom_boxplot() +
  xlab("Colour Family") +
  ylab("Residuals") +
  theme_bw()+
  coord_flip() +
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_fill_manual(values=c("royalblue1","green2","firebrick1")) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15))

#next, we need to create a boxplot with colour family on the y axis and standard deviation on the x axis
#obtain standard deviation values
#note that this produces some "NaN" values because some species have only 1 entry... 
#because we calculated mean red/green/blue
#so these were filtered too
dataframe_sd_red <- select(dataframe_for_plotting, 1, 4) %>%
  group_by(species) %>%
  summarise(sd=sd(red.x))
dataframe_sd_red <- dataframe_sd_red[!is.na(as.numeric(as.character(dataframe_sd_red$sd))),]
dataframe_sd_green <- select(dataframe_for_plotting, 1, 5) %>%
  group_by(species) %>%
  summarise(sd=sd(green.x))
dataframe_sd_green <- dataframe_sd_green[!is.na(as.numeric(as.character(dataframe_sd_green$sd))),]
dataframe_sd_blue <- select(dataframe_for_plotting, 1, 6)%>%
  group_by(species) %>%
  summarise(sd=sd(blue.x))
dataframe_sd_blue <- dataframe_sd_blue[!is.na(as.numeric(as.character(dataframe_sd_blue$sd))),]

#add column to differentiate colour component (R/G/B)
dataframe_sd_red["RGB"]<-"R"
dataframe_sd_green["RGB"]<-"G"
dataframe_sd_blue["RGB"]<-"B"

#join together
dataframe_sd_all<-bind_rows(dataframe_sd_red, dataframe_sd_green, dataframe_sd_blue)

#merge with corresponding colour family
dataframe_sd_all_with_colours<-merge(dataframe_sd_all, bc_subset, by="species")

#manipulate data to obtain workable data frame format
dataframe_sd_all_with_colours_true <- pivot_longer(dataframe_sd_all_with_colours, 4:16) %>%
  dplyr::filter(value=="TRUE") %>%
  select(2:4)

#plot FIGURE 5B
fig5b<-ggplot(dataframe_sd_all_with_colours_true, aes(x=name, y=sd, fill=RGB)) +
  geom_boxplot() +
  xlab("Colour Family") +
  ylab("Standard Deviation") +
  theme_bw()+
  coord_flip() +
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_fill_manual(values=c("royalblue1","green2","firebrick1")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_text(size = 15)) +
  theme(axis.title = element_text(size = 15)) +
  theme(legend.text = element_text(size = 15))

#preview FIGURES 5A & 5B
fig5a
fig5b

#PLOTTING FIGURE 6
#there are 6 components that make up figure 6 (a - f)
#this R script will mostly exclude non-essential code (e.g. to collate 6 components together)

#read in all measurement files from the folder "upload" within the working directory - change this if files have been saved elsewhere
    #Senna pendula
SP_olympus_flash <- read_csv("upload/SP_olympus_flash_zapped.csv") %>%
  mutate(source="Olympus flash")
SP_olympus_noflash <- read_csv("upload/SP_olympus_noflash_zapped.csv") %>%
  mutate(source="Olympus no flash")
SP_phone_flash <- read_csv("upload/SP_phone_flash_zapped.csv") %>%
  mutate(source="Phone flash")
SP_phone_noflash <- read_csv("upload/SP_phone_noflash_zapped.csv") %>%
  mutate(source="Phone no flash")
SP_spec <- read_csv("upload/SP_spec_zapped.csv") %>%
  mutate(source="Spec")
SP_inat <- read_csv("upload/SP_iNat_zapped.csv") %>%
  mutate(source="iNat")
    #Macroptilium atropurpureum
MA_olympus_flash <- read_csv("upload/MA_olympus_flash_zapped.csv") %>%
  mutate(source="Olympus flash")
MA_olympus_noflash <- read_csv("upload/MA_olympus_noflash_zapped.csv") %>%
  mutate(source="Olympus no flash")
MA_phone_flash <- read_csv("upload/MA_phone_flash_zapped.csv") %>%
  mutate(source="Phone flash")
MA_phone_noflash <- read_csv("upload/MA_phone_noflash_zapped.csv") %>%
  mutate(source="Phone no flash")
MA_spec <- read_csv("upload/MA_spec_zapped.csv") %>%
  mutate(source="Spec")
MA_inat <- read_csv("upload/MA_iNat_zapped.csv") %>%
  mutate(source="iNat")

#read in quality assessment files for both sets of iNat photos, keeping only essential columns
#rename column into path
#modify text under path to match with photo ID in filter file
#join the two, and clean up NA values
#then split each iNat data frame into two categories of quality
    #Senna pendula
SP_filter <- read_csv("upload/SP_observations-78219.csv") %>%
  select(1,37)
names(SP_filter)[1] <- "path"
SP_filter$path <- as.character((SP_filter$path))
SP_inat2 <- SP_inat %>%
  mutate(path=gsub("D:/R-3.6.2/library/colorZapper/sample/", "", .$path)) %>%
  mutate(path=gsub(".jpg", "", .$path))
SP_inat3 <- full_join(SP_inat2, SP_filter, by = "path") %>%
  drop_na(id)
SP_inat_good <- SP_inat3 %>%
  subset(., hq=="Y")%>%
  mutate(source="Citizen science, 4-5*")
SP_inat_average <- SP_inat3 %>%
  subset(., is.na(hq))%>%
  mutate(source="Citizen science")
    #Macroptilium atropurpureum
MA_filter <- read_csv("upload/MA_observations-77915.csv") %>%
  select(1,37)
names(MA_filter)[1] <- "path"
MA_filter$path <- as.character((MA_filter$path))
MA_inat2 <- MA_inat %>%
  mutate(path=gsub("C:/Users/z5119682/Documents/R/win-library/3.6/colorZapper/sample/", "", .$path)) %>%
  mutate(path=gsub(".jpg", "", .$path))
MA_inat3 <- full_join(MA_inat2, MA_filter, by = "path") %>%
  drop_na(id)
MA_inat_good <- MA_inat3 %>%
  subset(., hq=="Y")%>%
  mutate(source="Citizen science, 4-5*")
MA_inat_average <- MA_inat3 %>%
  subset(., is.na(hq))%>%
  mutate(source="Citizen science")

#bind together
    #Senna pendula
SP_dat_joined <- bind_rows(SP_olympus_flash,
                        SP_olympus_noflash,
                        SP_phone_flash,
                        SP_phone_noflash,
                        SP_spec,
                        SP_inat_good,
                        SP_inat_average)
    #Macroptilium atropurpureum
MA_dat_joined <- bind_rows(MA_olympus_flash,
                        MA_olympus_noflash,
                        MA_phone_flash,
                        MA_phone_noflash,
                        MA_spec,
                        MA_inat_good,
                        MA_inat_average)

#obtain data frames with mean values
SP_dat2 <- SP_dat_joined %>%
  group_by(source, id) %>%
  summarize(R=mean(R),
            G=mean(G),
            B=mean(B))
MA_dat2 <- MA_dat_joined %>%
  group_by(source, id) %>%
  summarize(R=mean(R),
            G=mean(G),
            B=mean(B))

#plot
fig6a <- ggplot(MA_dat2, aes(x=R, y=G, color=source, shape=source))+
  geom_point()+
  theme_bw()+
  scale_colour_manual(values = c("gray85", "gray30","cadetblue1", "royalblue1", "chartreuse", "forestgreen", "firebrick1"))+
  scale_shape_manual(values = c(4, 4, 16, 16, 15, 15, 17))+
  theme(axis.text=element_text(color="black"), text = element_text(size=15))+
  theme(legend.position = "none") +
  xlab("R")+
  ylab("G")
fig6b <- ggplot(MA_dat2, aes(x=R, y=B, color=source, shape=source))+
  geom_point()+
  theme_bw()+
  scale_colour_manual(values = c("gray85", "gray30","cadetblue1", "royalblue1", "chartreuse", "forestgreen", "firebrick1"))+
  scale_shape_manual(values = c(4, 4, 16, 16, 15, 15, 17))+
  theme(axis.text=element_text(color="black"), text = element_text(size=15))+
  theme(legend.position = "none") +
  xlab("R")+
  ylab("B")
fig6c <- ggplot(MA_dat2, aes(x=B, y=G, color=source, shape=source))+
  geom_point()+
  theme_bw()+
  scale_colour_manual(values = c("gray85", "gray30","cadetblue1", "royalblue1", "chartreuse", "forestgreen", "firebrick1"))+
  scale_shape_manual(values = c(4, 4, 16, 16, 15, 15, 17))+
  theme(axis.text=element_text(color="black"), text = element_text(size=15))+
  theme(legend.position = "none") +
  xlab("B")+
  ylab("G")
fig6d <- ggplot(SP_dat2, aes(x=R, y=G, color=source, shape=source))+
  geom_point()+
  theme_bw()+
  scale_colour_manual(values = c("gray85", "gray30","cadetblue1", "royalblue1", "chartreuse", "forestgreen", "firebrick1"))+
  scale_shape_manual(values = c(4, 4, 16, 16, 15, 15, 17))+
  theme(axis.text=element_text(color="black"), text = element_text(size=15))+
  theme(legend.position = "none") +
  xlab("R")+
  ylab("G")
fig6e <- ggplot(SP_dat2, aes(x=R, y=B, color=source, shape=source))+
  geom_point()+
  theme_bw()+
  scale_colour_manual(values = c("gray85", "gray30","cadetblue1", "royalblue1", "chartreuse", "forestgreen", "firebrick1"))+
  scale_shape_manual(values = c(4, 4, 16, 16, 15, 15, 17))+
  theme(axis.text=element_text(color="black"), text = element_text(size=15))+
  theme(legend.position = "none") +
  xlab("R")+
  ylab("B")
fig6f <- ggplot(SP_dat2, aes(x=B, y=G, color=source, shape=source))+
  geom_point()+
  theme_bw()+
  scale_colour_manual(values = c("gray85", "gray30","cadetblue1", "royalblue1", "chartreuse", "forestgreen", "firebrick1"))+
  scale_shape_manual(values = c(4, 4, 16, 16, 15, 15, 17))+
  theme(axis.text=element_text(color="black"), text = element_text(size=15))+
  theme(legend.position = "none") +
  xlab("B")+
  ylab("G")

#preview FIGURES 6 A-F
fig6a
fig6b
fig6c
fig6d
fig6e
fig6f