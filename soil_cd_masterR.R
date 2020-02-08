rm(list = ls())

setwd("")

library(dplyr)
library(ggpmisc)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
library(xlsx)
library(forcats)
library(soiltexture)
library(PivotalR)
library(RcmdrMisc)

cd_data <- readxl::read_xlsx("~./cd_soildata_Master.xlsx", 
                             sheet = "mastersheet", col_names = TRUE)
corr <-  readxl::read_xlsx("~./cd_soildata_Master.xlsx", 
                           sheet = "correlation", col_names = TRUE)

#themeing####
mytheme <-   theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
                   panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
                   panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
                   panel.border=element_blank(), #gets rid of square going around the entire graph
                   axis.line.x = element_line(color = 'black', size = 0.5),#sets the axis line size
                   axis.line.y = element_line(color = 'black', size = 0.5),#sets the axis line size
                   axis.ticks=element_line(color = 'black', size = 0.5), #sets the tick lines
                   axis.title.x = element_text(size=12, color="black"), #size of x-axis title
                   axis.title.y = element_text(size=12, color="black"), #size of y-axis title
                   axis.text.x = element_text(size=12, color="black", hjust = 1), #size of x-axis text
                   axis.text.y = element_text(size=12, color="black")) #size of y-axis text

#soil depth histogram####
Cd_SD <- data.frame(cd_data$`Study ID`, 
                       cd_data$`Authors et al.`, 
                       cd_data$`Maximum Soil depth tested (cm)`, 
                       cd_data$`Total Cd (mg/kg) in soil`,
                       cd_data$`Available Cd (mg/kg)`)

Cd_SD <- Cd_SD %>% rename(
  studyid = cd_data..Study.ID.,
  author = cd_data..Authors.et.al..,
  maxdepth = cd_data..Maximum.Soil.depth.tested..cm..,
  total_cd = cd_data..Total.Cd..mg.kg..in.soil.,
  avail_cd = cd_data..Available.Cd..mg.kg..)

ggplot(Cd_SD, aes(x = maxdepth))+
  geom_histogram(binwidth = 5, fill = "black", color = "black", alpha = 0.25)+
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 80, 20), expand=c(0,0))+
  #scale_x_continuous(breaks = seq(0, 100, 10), expand = c(0,0))+  
  xlab("Max Soil Depth (cm)")+
  ylab("Frequency")+
  ggtitle("Frequency Histogram of Max Soil Depth Examined \
          in Soil for Cd in Soil of Cocoa Plantations")+
  coord_flip(xlim = c(0,151), ylim = c(0,60))+
  scale_x_reverse(breaks = seq(0, 150, 10), expand = c(0,0), position = "left")+
  #scale_y_reverse()+
  mytheme+
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

#correlation data####
str(corr)
corr$`Study ID` <- as.factor(corr$`Study ID`)

corr.sub <- subset(corr, !is.na(corr$`Country Only`)) #subset for those without a country label
corr.sub <- corr.sub %>% rename(
  Country = `Country Only`,
  Countries = `Country Letter`)

#Total Cd and Nibs/Beans Correlation####
sub.total <- subset(corr.sub, !is.na(corr.sub$`Total Cd (mg/kg) in soil`))
sub.total.nibs <- subset(sub.total, !is.na(sub.total$`Nibs Cd (mg/kg)`))
sub.total.wb <- subset(sub.total, !is.na(sub.total$`Whole Beans Cd (mg/kg)`))

pred_nibs <- predict(lm(sub.total.nibs$`Nibs Cd (mg/kg)` ~ 
                     sub.total.nibs$`Total Cd (mg/kg) in soil`), se.fit = TRUE,
                interval = "confidence")

limits_nibs = as.data.frame(pred_nibs$fit)

ggtop <- ggplot(data = sub.total.nibs, aes(x = sub.total.nibs$`Total Cd (mg/kg) in soil`,
                        y = sub.total.nibs$`Nibs Cd (mg/kg)`))+
  geom_point(aes(color = Country))+
  scale_x_continuous(limits = c(0, 2.75), expand = c(0, 0.025))+
  scale_y_continuous(limits = c(0, 1.5), expand = c(0.05, 0))+
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, color = "gray65")+
  xlab("Total Cd in soil (mg/kg)")+
  ylab("Cd in Nibs (mg/kg)")+
  theme(plot.margin = unit(c(1,5,-30,6),units="points"),
        axis.title.y = element_text(vjust =0.25), 
        legend.position = "right")+
  #geom_text(aes(label=Countries, color = Country))+
  labs(fill = "Country")+
  mytheme+
  geom_line(aes(x = sub.total.nibs$`Total Cd (mg/kg) in soil`, y = limits_nibs$lwr), 
            linetype = 2) +
  geom_line(aes(x = sub.total.nibs$`Total Cd (mg/kg) in soil`, y = limits_nibs$upr), 
            linetype = 2)+
  scale_color_manual(breaks = c("Ecuador", "Ghana", "Peru", "Venezuela"),
                     values=c("blue", "green", "tan4", "lightpink1"))+
  #scale_colour_grey()+
  stat_regline_equation(formula = y ~ x, label.x = 0.1, label.y = 1.3,
                        geom = "text")+
  stat_cor(method = "pearson", label.x = 0.1, label.y = 1.42)

pred_wb_total <- predict(lm(sub.total.wb$`Whole Beans Cd (mg/kg)` ~ 
                          sub.total.wb$`Total Cd (mg/kg) in soil`), se.fit = TRUE,
                     interval = "confidence")

limits_wb_total = as.data.frame(pred_wb_total$fit)

ggbottom <- ggplot(data = sub.total.wb, aes(x = sub.total.wb$`Total Cd (mg/kg) in soil`,
                                     y = sub.total.wb$`Whole Beans Cd (mg/kg)`))+
  geom_point(aes(color = Country))+
  scale_x_continuous(limits = c(0, 2.75), expand = c(0, 0.025))+
  scale_y_continuous(limits = c(0, 8.0), expand = c(0.05, 0),
                     labels = scales::number_format(accuracy = 0.1))+
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, color = "gray65")+
  xlab("Total Cd in soil (mg/kg)")+
  ylab("Cd in Whole Beans (mg/kg)")+
  theme(plot.margin = unit(c(1,5,1,6),units="points"),
        legend.position = "right")+
  mytheme+
  geom_line(aes(x = sub.total.wb$`Total Cd (mg/kg) in soil`, y = limits_wb_total$lwr), 
            linetype = 2) +
  geom_line(aes(x = sub.total.wb$`Total Cd (mg/kg) in soil`, y = limits_wb_total$upr), 
            linetype = 2)+ 
  #scale_colour_grey()+
  #geom_text(aes(label=Countries, color = Country))+
  scale_color_manual(breaks = c("Bolivia", "Ecuador", "Ghana", "Honduras", "Peru"),
                     values=c("red", "blue", "green", "gold2", "tan4"))+
  stat_cor(method = "pearson", label.x = 0.1, label.y = 7.75)+
  stat_regline_equation(formula = y ~ x, label.x = 0.1, label.y = 7.05,
                        geom = "text")

grid.arrange(ggtop, ggbottom)

#Available Cd and Nibs/Beans Correlation####
sub.avail <- subset(corr.sub, !is.na(corr.sub$`Available Cd (mg/kg)`))
sub.avail.nibs <- subset(sub.avail, !is.na(sub.avail$`Nibs Cd (mg/kg)`))
sub.avail.wb <- subset(sub.avail, !is.na(sub.avail$`Whole Beans Cd (mg/kg)`))

pred <- predict(lm(sub.avail.nibs$`Nibs Cd (mg/kg)` ~ 
                     sub.avail.nibs$`Available Cd (mg/kg)`), se.fit = TRUE,
                interval = "confidence")

limits = as.data.frame(pred$fit)

availtop <- ggplot(data = sub.avail.nibs, aes(x = sub.avail.nibs$`Available Cd (mg/kg)`,
                                     y = sub.avail.nibs$`Nibs Cd (mg/kg)`))+
  geom_point(aes(color = Country))+
  scale_x_continuous(limits = c(0, 2.75), expand = c(0, 0.025))+
  scale_y_continuous(limits = c(0, 0.75), expand = c(0.05, 0))+
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, color = "gray65")+
  xlab("Available Cd in soil (mg/kg)")+
  ylab("")+
  theme(plot.margin = unit(c(1,10,-30,6),units="points"),
        axis.title.y = element_text(vjust =0.25),
        legend.position = "right")+
  #geom_text(aes(label=Countries))+
  labs(fill = "Country")+
  mytheme+
  #scale_colour_grey()+
  scale_color_manual(breaks = c( "Ecuador", "Venezuela"),
                     values=c( "blue", "lightpink1"))+
  geom_line(aes(x = sub.avail.nibs$`Available Cd (mg/kg)`, y = limits$lwr), 
            linetype = 2) +
  geom_line(aes(x = sub.avail.nibs$`Available Cd (mg/kg)`, y = limits$upr), 
            linetype = 2)+
  stat_regline_equation(formula = y ~ x, label.x = 0.1, label.y = 0.63,
                        geom = "text")+
  stat_cor(method = "pearson", label.x = 0.1, label.y = 0.70)


pred_wb <- predict(lm(sub.avail.wb$`Whole Beans Cd (mg/kg)` ~ 
                        sub.avail.wb$`Available Cd (mg/kg)`), se.fit = TRUE,
                   interval = "confidence")

limits_wb = as.data.frame(pred_wb$fit)


availbottom <- ggplot(data = sub.avail.wb, aes(x = sub.avail.wb$`Available Cd (mg/kg)`,
                                        y = sub.avail.wb$`Whole Beans Cd (mg/kg)`))+
  geom_point(aes(color = Country))+
  scale_x_continuous(limits = c(0, 2.75), expand = c(0, 0.025))+
  scale_y_continuous(limits = c(0.0, 5.0), expand = c(0.05, 0), 
                     labels = scales::number_format(accuracy = 0.1))+
  geom_smooth(method = "lm", se = FALSE, fullrange = FALSE, color = "gray65")+
  xlab("Available Cd in soil (mg/kg)")+
  ylab("")+
  #geom_text(aes(label=Countries))+
  theme(plot.margin = unit(c(1,6,1,6),units="points"),
        legend.position = "right")+
  mytheme+
  #scale_colour_grey()+
  scale_color_manual(breaks = c("Bolivia", "Ecuador", "Honduras"),
                     values=c("red", "blue", "gold2"))+
  stat_cor(method = "pearson", label.x = 0.1, label.y = 5.0)+
  stat_regline_equation(formula = y ~ x, label.x = 0.1, label.y = 4.5,
                        geom = "text")+
  geom_line(aes(x = sub.avail.wb$`Available Cd (mg/kg)`, y = limits_wb$lwr), 
            linetype = 2) +
  geom_line(aes(x = sub.avail.wb$`Available Cd (mg/kg)`, y = limits_wb$upr), 
            linetype = 2)

grid.arrange(availtop, availbottom)

#total points of interest####
#poi stands for points of interest
poi<- data.frame(cd_data$`Study ID`,
                 cd_data$Title,
                 cd_data$`Country Only`,
                 cd_data$`Soil pH (in water)`, 
                 cd_data$`CEC cation exchange capacity; cmol/kg`, 
                 cd_data$`Total Zinc (mg/kg)`, 
                 cd_data$`Pb (mg/kg)`, 
                 cd_data$`Soil depth of sample (cm)`,
                 cd_data$`Maximum Soil depth tested (cm)`,
                 cd_data$Quantification, 
                 cd_data$`% Clay by weight`, 
                 cd_data$`%Silt by weight`, 
                 cd_data$`%Sand by weight`,
                 cd_data$`Soil textural class`,
                 cd_data$`Available Cd (mg/kg)`,
                 cd_data$`Total Cd (mg/kg) in soil`) 

#rename columns####
poi <- poi %>% rename(
  study_id = cd_data..Study.ID.,
  countries = cd_data..Country.Only.,
  titles = cd_data.Title,
  soil_depth = cd_data..Soil.depth.of.sample..cm..,
  max_soil_depth = cd_data..Maximum.Soil.depth.tested..cm..,
  pH_in_water = cd_data..Soil.pH..in.water..,
  CEC_values = cd_data..CEC.cation.exchange.capacity..cmol.kg.,
  total_zinc = cd_data..Total.Zinc..mg.kg..,
  total_lead = cd_data..Pb..mg.kg..,
  total_cd = cd_data..Total.Cd..mg.kg..in.soil.,
  available_cd = cd_data..Available.Cd..mg.kg..,
  quantification = cd_data.Quantification,
  clay_percent = cd_data....Clay.by.weight.,
  silt_percent = cd_data...Silt.by.weight.,
  sand_percent = cd_data...Sand.by.weight.,
  textures = cd_data..Soil.textural.class.)

poi$study_id <- as.factor(poi$study_id) 

#subset table based on available Cd
sub.avail <- subset(poi, !is.na(available_cd))
sub_sub.avail <- subset(sub.avail, !is.na(sub.avail$quantification))
avail_plot<- ggplot(data=sub_sub.avail, aes(x=fct_reorder(sub_sub.avail$quantification, 
                                                          sub_sub.avail$available_cd), sub_sub.avail$available_cd))+
  geom_boxplot()+geom_jitter(width=0.1, alpha=0.2)+ 
  xlab("Quantification")+ 
  ylab("Available Cd (mg/kg)")
avail_plot

####Soil Texture Classes####
avail.texture <- subset(sub.avail, !is.na(sub.avail$textures))
avail_texture_plot <- ggplot(avail.texture, 
                             aes(x=fct_reorder(avail.texture$textures, 
                                               avail.texture$available_cd), 
                                 avail.texture$available_cd))+
  geom_boxplot()+
  geom_jitter(width=0.1, alpha=0.2)+
  xlab("Soil Textural Class")+
  ylab("Available Cd (mg/kg)")+
  mytheme+
  theme(axis.text.x = element_text(angle = 45),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
avail_texture_plot

####TOTAL CD####

sub_totalcd <- subset(poi, !is.na(poi$total_cd))
total_soil <- subset(sub_totalcd, !is.na(sub_totalcd$textures))
total_soil_plot <- ggplot(total_soil, 
                          aes(x=fct_reorder(total_soil$textures, 
                                            total_soil$total_cd),  
                              total_soil$total_cd))+
  geom_boxplot()+
  geom_jitter(width=0.1, alpha=0.2)+
  xlab("Soil Texture Class")+
  ylab("Total Cd (mg/kg)")+
  mytheme+
  theme(axis.text.x = element_text(angle = 45),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
total_soil_plot

total_quant <- subset(sub_totalcd, !is.na(sub_totalcd$quantification))
total_quant_plot <- ggplot(total_quant, aes(x=fct_reorder(
  total_quant$quantification, 
  total_quant$total_cd),  
  total_quant$total_cd))+
  geom_boxplot()+
  geom_jitter(width=0.1, alpha=0.2)+
  xlab("Quantification Method")+
  ylab("Total Cd (mg/kg)")+
  mytheme+
  theme(axis.text.x = element_text(angle = 45),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

total_quant_plot

####TEXTURE TRIANGLE####

sub.total <- subset(poi, !is.na(poi$total_cd))  #set up for total cd
sub.total.subsand <- subset(sub.total, !is.na(sub.total$sand_percent))
tri.total <- data.frame(sub.total.subsand$study_id, 
                        sub.total.subsand$countries, 
                        sub.total.subsand$total_cd, 
                        sub.total.subsand$available_cd, 
                        sub.total.subsand$clay_percent, 
                        sub.total.subsand$silt_percent, 
                        sub.total.subsand$sand_percent, 
                        sub.total.subsand$textures)

tri.total <- tri.total %>%
  rename(
    study_id = sub.total.subsand.study_id,
    countries = sub.total.subsand.countries,
    total_cd = sub.total.subsand.total_cd,
    available_cd = sub.total.subsand.available_cd,
    clay_percent = sub.total.subsand.clay_percent,
    silt_percent = sub.total.subsand.silt_percent,
    sand_percent = sub.total.subsand.sand_percent,
    textures = sub.total.subsand.textures)

sub.avail <- subset(poi, !is.na(poi$available_cd)) #set up for avail cd
sub.avail.subsand <- subset(sub.avail, !is.na(sub.avail$sand_percent))

tri.avail <- data.frame(sub.avail.subsand$study_id, 
                        sub.avail.subsand$countries,
                        sub.avail.subsand$total_cd, 
                        sub.avail.subsand$available_cd, 
                        sub.avail.subsand$clay_percent, 
                        sub.avail.subsand$silt_percent, 
                        sub.avail.subsand$sand_percent,
                        sub.avail.subsand$textures)

tri.avail <- tri.avail %>%
  rename(
    study_id = sub.avail.subsand.study_id,
    countries = sub.avail.subsand.countries,
    avail_cd = sub.avail.subsand.available_cd,
    available_cd = sub.avail.subsand.available_cd,
    clay_percent = sub.avail.subsand.clay_percent,
    silt_percent = sub.avail.subsand.silt_percent,
    sand_percent = sub.avail.subsand.sand_percent,
    textures = sub.avail.subsand.textures)

tri.total.sub <- data.frame(tri.total$clay_percent, 
                            tri.total$silt_percent, 
                            tri.total$sand_percent, 
                            tri.total$total_cd, 
                            tri.total$countries)

tri.avail.sub <- data.frame(tri.avail$clay_percent, 
                            tri.avail$silt_percent, 
                            tri.avail$sand_percent, 
                            tri.avail$available_cd, 
                            tri.avail$countries)

tri.total.sub <- tri.total.sub %>%
  rename(
    CLAY = tri.total.clay_percent,
    SILT = tri.total.silt_percent,
    SAND = tri.total.sand_percent,
    TotalCd = tri.total.total_cd,
    countries = tri.total.countries)
tri.total.sub$CLAY <- round(tri.total.sub$CLAY, 2)
tri.total.sub$SAND <- round(tri.total.sub$SAND, 2)
tri.total.sub$SILT <- round(tri.total.sub$SILT, 2)


tri.avail.sub <- tri.avail.sub %>%
  rename(
    CLAY = tri.avail.clay_percent,
    SILT = tri.avail.silt_percent,
    SAND = tri.avail.sand_percent,
    AvailCd = tri.avail.available_cd,
    countries = tri.avail.countries)

tri.avail.sub$CLAY <- round(tri.avail.sub$CLAY, 2)
tri.avail.sub$SAND <- round(tri.avail.sub$SAND, 2)
tri.avail.sub$SILT <- round(tri.avail.sub$SILT, 2)


#RUN THE WHOLE THING (Highlight and Run)
tri.total.plot <- TT.plot(
  class.sys = "USDA.TT",
  tri.data = tri.total.sub,
  z.name = "TotalCd",
  main = "Soil texture triangle and Total Cd bubble plot"
) #
# Recompute some internal values:
z.cex.range <- TT.get("z.cex.range")
def.pch <- par("pch")
def.col <- par("col")
def.cex <- TT.get("cex")
tcd.str <- TT.str(
  tri.total.sub[,"TotalCd"],
  z.cex.range[1],
  z.cex.range[2]
) #
# The legend:
legend(
  x = 70,
  y = 100,
  title = expression(bold('Total Cd (mg/kg)')),
  legend = formatC(
    c(
      min(tri.total.sub[,"TotalCd"] ),
      quantile(tri.total.sub[,"TotalCd"], probs=c(25,50,75)/100),
      max(tri.total.sub[,"TotalCd"] )
    ),
    format = "f",
    digits = 2,
    width = 2,
    flag = "0"
  ), #
  pt.lwd = 4,
  col = def.col,
  pt.cex = c(
    min(tcd.str),
    quantile(tcd.str, probs=c(25,50,75)/100),
    max(tcd.str)
  ), #,
  pch = def.pch,
  bty = "o",
  bg = NA,
  box.col = NA, # Uncomment this to remove the legend box
  text.col = "black",
  cex = def.cex
) #

tri.avail.plot <- TT.plot(
  class.sys = "USDA.TT",
  tri.data = tri.avail.sub,
  z.name = "AvailCd",
  main = "Soil texture triangle and Available Cd bubble plot",
  z.col.hue = NULL
) #
# Recompute some internal values:
z.cex.range <- TT.get("z.cex.range")
def.pch <- par("pch")
def.col <- par("col")
def.cex <- TT.get("cex")
cd.str <- TT.str(
  tri.avail.sub[,"AvailCd"],
  z.cex.range[1],
  z.cex.range[2]
) #
# The legend:
legend(
  x = 63,
  y = 100,
  title = expression(bold('Available Cd (mg/kg)')),
  legend = formatC(
    c(
      min(tri.avail.sub[,"AvailCd"] ),
      quantile(tri.avail.sub[,"AvailCd"], probs=c(25,50,75)/100),
      max(tri.avail.sub[,"AvailCd"] )
    ),
    format = "f",
    digits = 2,
    width = 2,
    flag = "0"
  ), #
  pt.lwd = 4,
  col = def.col,
  pt.cex = c(
    min(cd.str),
    quantile(cd.str, probs=c(25,50,75)/100),
    max(cd.str)
  ), #,
  pch = def.pch,
  bty = "o",
  bg = NA,
  box.col = NA, # Uncomment this to remove the legend box
  text.col = "black",
  cex = def.cex
) #


####With Countries####
#RUN THE WHOLE THING (Highlight and Run)
site.pch <- 1:length(unique(tri.total.sub$countries)) 
names(site.pch) <- as.character(unique(tri.total.sub$countries))

z.cex.range <- TT.get("z.cex.range")
# def.pch <- par("pch")
# def.col <- par("col")

# s.str <- TT.str( tri.total.sub[,"TotalCd"], 0, 0.75 )
v.str <- TT.str( tri.total.sub[,"TotalCd"], 0.9, 0.5 )

# def.cex <- TT.get("cex")
tcd.str <- TT.str(tri.total.sub[,"TotalCd"], z.cex.range[1], z.cex.range[2]) 

cols <- hsv(h = 0.21, s = 1, v = v.str )

oc.legend  <- quantile(tri.total.sub[,"TotalCd"], probs = c(0, 0.25, 0.5, 0.75, 1))
col.legend <- hsv(h = 0.21, s = 1, v = TT.str(oc.legend, 0.9, 0.5))
cex.legend <- TT.str(oc.legend, z.cex.range[1], z.cex.range[2]) 


# :: And plot it, with one color per site
soiltexture::TT.plot( 
  class.sys = "USDA-NCSS.TT", 
  tri.data  = tri.total.sub, 
  main      = "Total Cadmium with Soil texture data, per Country", 
  col       = "black", 
  pch       = site.pch[as.character(tri.total.sub[,"countries"])], 
  cex       = tcd.str, 
  lwd       = 3L) 

legend( 
  x       = -13,
  y       = 105,
  title   = "Countries:", 
  legend  = names(site.pch), 
  pch     = as.integer(site.pch),
  cex     = 1.4,
  pt.cex  = 2, 
  pt.lwd  = 3, 
  bty     = "n", 
  y.intersp = 1)

legend( 
  x       = 67,
  y       = 105,
  title   = "Total Cd (mg/kg):", 
  legend  = formatC(oc.legend, format = "f", digits = 2, width = 2, flag = "0" ),
  cex     = 1.4,
  pt.cex  = cex.legend, 
  col     = "black", 
  pch     = 1L, 
  pt.lwd  = 4, 
  bty     = "n", 
  y.intersp = 1)

####AVAILABLE  TT PLOT####
site.pch <- 1:length(unique(tri.avail.sub$countries)) 
names(site.pch) <- as.character(unique(tri.avail.sub$countries))

z.cex.range <- TT.get("z.cex.range")
# def.pch <- par("pch")
# def.col <- par("col")

# s.str <- TT.str( tri.avail.sub[,"AvailCd"], 0, 0.75 )
v.str <- TT.str( tri.avail.sub[,"AvailCd"], 0.9, 0.5 )

# def.cex <- TT.get("cex")
cd.str <- TT.str(tri.avail.sub[,"AvailCd"], z.cex.range[1], z.cex.range[2]) 

cols <- hsv(h = 0.21, s = 1, v = v.str )

oc.legend  <- quantile(tri.avail.sub[,"AvailCd"], probs = c(0, 0.25, 0.5, 0.75, 1))
col.legend <- hsv(h = 0.21, s = 1, v = TT.str(oc.legend, 0.9, 0.5))
cex.legend <- TT.str(oc.legend, z.cex.range[1], z.cex.range[2]) 

# :: And plot it, with one color per site
soiltexture::TT.plot( 
  class.sys = "USDA-NCSS.TT", 
  tri.data  = tri.avail.sub, 
  main      = "Available Cadmium with Soil texture data, per Country", 
  col       = "black", 
  pch       = site.pch[as.character(tri.avail.sub[,"countries"])], 
  cex       = cd.str, 
  lwd       = 3L) 

legend( 
  x       = -20,
  y       = 105,
  title   = "Countries:", 
  legend  = names(site.pch), 
  pch     = as.integer(site.pch),
  cex     = 1.4,
  pt.cex  = 2, 
  pt.lwd  = 3, 
  bty     = "n", 
  y.intersp = 1)

legend( 
  x       = 67,
  y       = 105,
  title   = "Available Cd (mg/kg):", 
  legend  = formatC(oc.legend, format = "f", digits = 2, width = 2, flag = "0" ),
  cex     = 1.4,
  pt.cex  = cex.legend, 
  col     = "black", 
  pch     = 1L, 
  pt.lwd  = 4, 
  bty     = "n", 
  y.intersp = 1)

####soil depths####

Cd_SD <- readxl::read_xlsx("~./Engeseth Work/Cd Paper/Cd_data/cd_soildata_Master.xlsx", 
                             sheet = "mastersheet", col_names = TRUE)

Cd_SD <- Cd_SD %>% rename(
  studyid = `Study ID`,
  author = `Authors et al.`,
  depthrange = `Soil depth range`,
  maxdepth = `Maximum Soil depth tested (cm)`,
  total_cd = `Total Cd (mg/kg) in soil`,
  avail_cd = `Available Cd (mg/kg)`,
  soildepthused = `Soil depth of sample (cm)`
)

ggplot(Cd_SD, aes(x = maxdepth))+
  geom_histogram(binwidth = 5, fill = "black", color = "black", alpha = 0.25)+
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 80, 20), expand=c(0.01,0.0))+
  #scale_x_continuous(breaks = seq(0, 100, 10), expand = c(0,0))+  
  xlab("Max Soil Depth (cm)")+
  ylab("Frequency")+
  ggtitle("Frequency Histogram of Max Soil Depth Examined \
          in Soil for Cd in Soil of Cocoa Plantations")+
  coord_flip(xlim = c(0,151), ylim = c(0,60))+
  scale_x_reverse(breaks = seq(0, 150, 10), expand = c(0,0), position = "left")+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.border=element_blank(),
        axis.line = element_line(colour = 'black', size = 0.5),
        axis.ticks= element_line(colour = 'black', size = 0.5),
        axis.title.x = element_text(face="bold", size=16, color="black"),
        axis.title.y = element_text(face="bold", size=16, color="black"),
        axis.text.x = element_text(size=13, color="black"),
        axis.text.y = element_text(size=13, color="black"),
        plot.title = element_text(hjust=0.5))

####Stepwise Regression####
# filter the specific variables to keep track of #
step_poi <- data.frame(poi$study_id,
                       poi$countries,
                       poi$pH_in_water,
                       poi$CEC_values,
                       poi$total_zinc,
                       poi$total_lead,
                       poi$max_soil_depth,
                       poi$textures,
                       cd_data$`Total Organic carbon TOC (g/kg)`,
                       cd_data$`Organic matter (g/kg)`,
                       cd_data$`Total Cd (mg/kg) in shells`,
                       cd_data$`Whole Beans Cd (mg/kg)`,
                       cd_data$`Nibs Cd (mg/kg)`)

step_poi <- step_poi %>% rename(
  study_id = poi.study_id,
  pH = poi.pH_in_water,
  countries = poi.countries,
  cec = poi.CEC_values,
  total_zn = poi.total_zinc,
  total_pb = poi.total_lead,
  soil_depth = poi.max_soil_depth,
  soil_textures = poi.textures,
  toc = cd_data..Total.Organic.carbon.TOC..g.kg..,
  org_mat = cd_data..Organic.matter..g.kg..,
  total_cd_shells = cd_data..Total.Cd..mg.kg..in.shells.,
  cd_wholebeans = cd_data..Whole.Beans.Cd..mg.kg..,
  cd_nibs = cd_data..Nibs.Cd..mg.kg..)

str(step_poi)
step_poi$pH <- as.numeric(as.character(step_poi$pH))  #change pH back to its original form, R likes to change the value
step_poi$study_id <- as_factor(step_poi$study_id)  #same with study_id

###Subset Cd_nibs out####
nib_poi <- subset(step_poi, !is.na(step_poi$cd_nibs))
nib_poi_sub <- subset(nib_poi, !is.na(nib_poi$cec))
nib_poi_subs <- subset(nib_poi_sub, !is.na(nib_poi_sub$toc))

all(is.na(nib_poi$total_pb))

###Fit Cd_nibs Model####
fit_nibs <- lm(cd_nibs ~ pH + cec + toc, data = nib_poi)
fit_nibs_sub <- lm(cd_nibs ~ pH + cec + toc, data = nib_poi_subs)
summary(fit_nibs)
summary(fit_nibs_sub)

####Stepwise portion of the models####
step(fit_nibs, direction = "backward")
stepwise(fit_nibs_sub, direction = c("backward"), criterion = c("BIC"))

forward_nibs <- lm(cd_nibs ~ 1, data = nib_poi)
forward_nibs_sub <- lm(cd_nibs ~ 1, data = nib_poi_subs)
summary(forward_nibs)
summary(forward_nibs_sub)

step(forward_nibs, direction = "forward", scope = formula(fit_nibs))
step(forward_nibs_sub, direction = "forward", scope = formula(fit_nibs_sub))

nib_poi$log_cd_nibs <- log10(nib_poi$cd_nibs)
nib_poi$ln_cd_nibs <- log(nib_poi$cd_nibs)
nib_poi_sub <- subset(nib_poi, !is.na(nib_poi$cec))
nib_poi_subs <- subset(nib_poi_sub, !is.na(nib_poi_sub$toc))

nib_poi <- nib_poi[!is.infinite(nib_poi$log_cd_nibs),]
nib_poi_subs <- nib_poi_subs[!is.infinite(nib_poi_subs$log_cd_nibs),]
fit_log_nibs <- lm(log_cd_nibs ~ pH + cec + toc, data = nib_poi)
fit_log_nibs_sub <- lm(log_cd_nibs ~ pH + cec + toc, data = nib_poi_subs)
summary(fit_log_nibs)

forward_log_nibs <- lm(log_cd_nibs ~ 1, data = nib_poi)
forward_log_nibs_sub <- lm(log_cd_nibs ~ 1, data = nib_poi_subs)

step(forward_log_nibs, direction = "forward", scope = formula(fit_log_nibs))
step(forward_log_nibs_sub, direction = "forward", scope = formula(fit_log_nibs_sub))

stepwise(fit_log_nibs, direction = c("backward"), criterion = c("AIC")) #AIC
stepwise(fit_log_nibs, direction = c("backward"), criterion = c("BIC")) #BIC - both are same as the below
step(fit_log_nibs, direction = "backward", k = log(69)) #BIC
step(fit_log_nibs, direction = "backward", k = 2) #AIC

wb_poi <- subset(step_poi, !is.na(step_poi$cd_wholebeans))

fit_wb <- lm(cd_wholebeans ~ pH + cec + toc, data = wb_poi)
fit_log_wb <- lm(log_cd_wb ~ pH + cec + toc, data = wb_poi)
summary(fit_wb)
summary(fit_log_wb)

forward_wb <- lm(cd_wholebeans ~ 1, data = wb_poi)
step(forward_wb, direction = "forward", scope = formula(fit_nibs))

wb_poi$log_cd_wb <- log(wb_poi$cd_wholebeans)
wb_poi_sub <- subset(wb_poi, !is.na(wb_poi$pH))
wb_poi_sub <- subset(wb_poi_sub, !is.na(wb_poi_sub$cec))
wb_poi_sub <- subset(wb_poi_sub, !is.na(wb_poi_sub$toc))

fit_wb_sub <- lm(cd_wholebeans ~ pH + cec + toc, data = wb_poi_sub)
fit_log_wb_sub <- lm(log_cd_wb ~ pH + cec + toc, data = wb_poi_sub)
summary(fit_wb_sub)
summary(fit_log_wb_sub)

step(fit_wb, direction = "backward", k = 2)
step(fit_wb, direction = "backward", k = log(125))
step(fit_wb_sub, direction = "backward", k = 2)
step(fit_wb_sub, direction = "backward", k = log(125))
step(fit_log_wb, direction = "backward", k = 2)
step(fit_log_wb, direction = "backward", k = log(125))
step(fit_log_wb_sub, direction = "backward", k = 2)
step(fit_log_wb_sub, direction = "backward", k = log(125))

forward_wb <- lm(cd_wholebeans ~ 1, data = wb_poi)
forward_wb_sub <- lm(cd_wholebeans ~ 1, data = wb_poi_sub)
forward_log_wb_sub <- lm(log_cd_wb ~ 1, data = wb_poi_sub)
step(forward_wb, direction = "forward", scope = formula(fit_wb), k = log(223)) #BIC
step(forward_wb, direction = "forward", scope = formula(fit_wb), k = 2) #AIC

forward_log_wb <- lm(log_cd_wb ~ 1, data = wb_poi)
forward_log_wb_sub <- lm(log_cd_wb ~ 1, data = wb_poi_sub)
step(forward_log_wb, direction = "forward", scope = formula(fit_log_wb), 
     k = 2) #AIC
step(forward_log_wb, direction = "forward", scope = formula(fit_log_wb), 
     k = log(223)) #BIC

step(forward_log_wb_sub, direction = "forward", scope = formula(fit_log_wb_sub), 
     k = 2) #AIC
step(forward_log_wb_sub, direction = "forward", scope = formula(fit_log_wb_sub), 
     k = log(125)) #BIC

#DONE####
