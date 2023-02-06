install.packages("ggplot2")
install.packages("tidyverse")
install.packages("lubridate")
library(ggplot2)
library(tidyverse)
library(lubridate)

war_dead <- read_tsv("War_Dead_Nijmegen.tsv")
warcat <- war_dead %>% count(`War Category`)
eventgrp <- war_dead %>% count(`Event Group`)
dates <- as.Date(dmy(war_dead$`Date of Death`))
deaths <- as.data.frame(table(dates))
colnames(deaths) <- c("Date","Deaths")

warc_plot <- ggplot(warcat,aes(`War Category`,n,fill=`War Category`)) + 
  geom_col() + 
  scale_fill_manual(values=c("#E0115F","#005EB8","#008000","#FF9B00")) + 
  scale_y_continuous(limits=c(0,1800), expand=c(0,0)) + 
  annotate("text",x="Civilian",y=1723,label="74.5%",size=4,fontface="bold") + 
  annotate("text",x="Civilian (Jewish)",y=583,label="22.8%",size=4,fontface="bold") + 
  annotate("text",x="Civilian (Roma/Sinti)",y=91,label="0.3%",size=4,fontface="bold") + 
  annotate("text",x="Civilian Resistance",y=138,label="2.4%",size=4,fontface="bold") + 
  labs(x="",y="Civilian Casualties", 
       title="Civilian Casualties of War, Nijmegen: January 1941 to July 1945") + 
  theme_classic() + 
  theme(legend.position="none",
        panel.background=element_rect(fill="#FFFFF1"),
        panel.grid.major.y=element_line(color="#444444",
                                        linetype="dotted",linewidth=0.2),
        plot.title=element_text(face="bold",size=16,hjust=0.5),
        axis.title.y=element_text(face="bold",size=12,vjust=2),
        axis.text.x=element_text(face="bold",size=11),
        axis.text.y=element_text(face="bold",size=11),
        aspect.ratio=1/3)

evgr_plot <- ggplot(eventgrp,aes(`Event Group`,n,fill=`Event Group`)) + geom_col() + 
  annotate("text",x="Bombardment of Nijmegen",y=785,label="34%",size=4,fontface="bold") + 
  annotate("text",x="Front City Nijmegen",y=705,label="30.4%",size=4,fontface="bold") + 
  annotate("text",x="Holocaust",y=559,label="23.8%",size=4,fontface="bold") + 
  annotate("text",x="Internment and Expulsion",y=165,label="2.4%",size=4,fontface="bold")+
  annotate("text",x="Liberation of Nijmegen",y=153,label="5.4%",size=4,fontface="bold") + 
  annotate("text",x="Liberation of the Netherlands",y=49,label="0.5%",size=4,
           fontface="bold") + 
  scale_fill_manual(values=c("#E0115F","#008000","#005EB8",
                             "#FF9B00","#7851A9","#FFD700")) + 
  scale_y_continuous(limits=c(0,800), expand=c(0,0)) + 
  scale_x_discrete(labels=c("Bombardment
of Nijmegen","Front City
Nijmegen","Holocaust","    Internment   
  and Expulsion","Liberation of
Nijmegen","Liberation  
of the
Netherlands")) + 
  labs(x="",y="Civilian Casualties",
       title="Second World War Events Causing Civilian Casualties,
Nijmegen: January 1941 to July 1945") + 
  theme_classic() + 
  theme(legend.position="none",
        panel.background=element_rect(fill="#FFFFF1"),
        panel.grid.major.y=element_line(color="#444444",
                                        linetype="dotted",linewidth=0.2),
        plot.title=element_text(face="bold",size=15,hjust=0.5),
        axis.title.y=element_text(face="bold",size=12,vjust=3),
        axis.text.x=element_text(face="bold",size=11),
        axis.text.y=element_text(face="bold",size=11),
        aspect.ratio=1/3)

timeline <- ggplot(deaths, aes(as.Date(Date),Deaths)) + 
  scale_x_date(breaks="3 months",minor_breaks="1 month",expand=c(0,0),date_labels=("%b
%Y"),limits=c(as.Date("1942-05-01"),as.Date("1945-05-31"))) + 
  scale_y_continuous(n.breaks=8,limits=c(0,800),expand=c(0.0125,0)) + 
  labs(x="",y="Civilian Casualties",
       title="Civilian Casualties of War, Nijmegen: May 1942 to May 1945") + 
  annotate("text",x=as.Date("1943-08-04"),y=119,label="Holocaust Transports",
           size=4,color="#005EB8",fontface="bold") + 
  geom_segment(aes(x=as.Date("1942-07-15"),y=0,xend=as.Date("1942-07-15"),yend=90),
               linewidth=0.7,color="#005EB8") + 
  geom_segment(aes(x=as.Date("1944-09-13"),y=0,xend=as.Date("1944-09-13"),yend=90),
               linewidth=0.7,color="#005EB8") + 
  geom_segment(aes(x=as.Date("1942-07-15"),y=90,xend=as.Date("1944-09-13"),yend=90),
               linewidth=0.7,color="#005EB8") + 
  annotate("text",x=as.Date("1944-12-17"),y=350,label="Nijmegen as
a Front City",size=4,color="#008000",fontface="bold") + 
  geom_segment(aes(x=as.Date("1944-09-01"),y=0,xend=as.Date("1944-09-01"),yend=300),
               linewidth=0.7,color="#008000") + 
  geom_segment(aes(x=as.Date("1945-03-31"),y=0,xend=as.Date("1945-03-31"),yend=300),
               linewidth=0.7,color="#008000") + 
  geom_segment(aes(x=as.Date("1944-09-01"),y=300,xend=as.Date("1945-03-31"),yend=300),
               linewidth=0.7,color="#008000") + 
  annotate("text",x=as.Date("1942-11-17"),y=283,label="Jewish
Razzia",size=4,color="#00008B") + 
  geom_segment(aes(x=as.Date("1942-11-17"),y=0,xend=as.Date("1942-11-17"),yend=225),
               linewidth=0.7,color="#00008B") + 
  annotate("text",x=as.Date("1944-10-02"),y=168,label="Bombardment
Kapokfabriek",size=4,color="#E0115F") + 
  annotate("text",x=as.Date("1944-02-22"),y=761,label="February 22
Bombardment",size=4,color="#E0115F") + 
  theme_classic() + 
  theme(legend.position="none",
        plot.title=element_text(face="bold",size=15,hjust=0.5),
        axis.title.y=element_text(face="bold",size=12,vjust=3),
        axis.text.x=element_text(face="bold",size=10),
        axis.text.y=element_text(face="bold",size=10),
        panel.background=element_rect(fill="#FFFFF1"),
        panel.grid.major.x=element_line(color="#444444",
                                        linetype="dotted",linewidth=0.2),
        panel.grid.minor.x=element_line(color="#444444",
                                        linetype="dotted",linewidth=0.2),
        aspect.ratio=1/2)

# War Category Comparison
warc_plot

# Event Group Comparison
evgr_plot

# Timeline of Civilian Casualties
timeline + geom_line(linewidth=0.645,color="#C30000")