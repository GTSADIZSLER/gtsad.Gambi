library(tidyverse)
library(here)
library(readxl)
library(binom)
library(ggthemes)
library(rmeta)
library(forestplot)


#dtsalm <- read_excel(here("salmodt.xlsx"))

dtsalm <- read_excel("Nsalmodt.xlsx", col_types = c("text", 
                                                    "date", "text", "text", "text", "text"))
 


prevS <- dtsalm %>% 
  group_by(specie, sex, age, salmonella) %>% 
  count() %>% 
  pivot_wider(names_from = "salmonella", values_from = "n", values_fill = 0) %>%  
  mutate(`tested` = sum(Neg, Pos)) %>%  
  #mutate(`N° animali analizzati` = sum(Neg, Pos)) %>%  
  #select(`Specie animale` = specie, `N° Positivi` = Pos, `N° animali analizzati`) 
  select(-Neg)  


prevS 

options(digits=2)

resbinomS <- binom.bayes(
  x = prevS$Pos, n = prevS$tested,
  type = "highest", conf.level = 0.95, tol = 1e-9)

# resbinomS <- binom.bayes(
#   x = prevS$`N° Positivi`, n = prevS$`N° animali analizzati`,
#   type = "highest", conf.level = 0.95, tol = 1e-9)


dtS <- cbind(prevS, resbinomS[,6:8])

dtS$age[is.na(dtS$age)] = "Not recorded" 
dtS$sex[is.na(dtS$sex)] = "Not recorded" 



Tasso <- dtS %>% filter(specie == "Badgers")

Tasso <- Tasso %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round,2)) %>% 
  select(-specie)



Tasso %>% ungroup() %>% 
  forestplot(labeltext = c( sex,age, Pos,
                            tested,  Prevalence, liminf,limsup),
             clip = c(0,1),
             xlog = FALSE, 
             xlab= "Prevalence",
             title = "Badgers",
             # txt_gp = fpTxtGp(cex=0.90),
             # axes = gpar(cex = 0.9),
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1))) %>%  
  
  fp_add_header(sex = "Sex", 
                age =  "Age", 
                Pos = "N.positive", 
                tested = "N.tested", 
                Prevalence = "Prevalence",
                liminf = "95%CI liminf", 
                limsup = "95%CI limsup" ) %>% 
  fp_append_row(mean  = 0.12,
                lower = 0.07,
                upper = 0.16,
                sex = "Overall",
                Pos = "21",
                tested = "182",
                Prevalence = "0.12",
                liminf = "0.07",
                limsup = "0.16",
                position = "last",
                is.summary = TRUE)
  
volpe <- dtS %>% filter(specie == "Foxes")

volpe <- volpe %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round,2)) %>% 
  select(-specie)



volpe %>% ungroup() %>% 
  forestplot(labeltext = c( sex,age, Pos,
                            tested,  Prevalence, liminf,limsup),
             clip = c(0,1),
             xlog = FALSE, 
             xlab= "Prevalence",
             title = "Foxes",
             # txt_gp = fpTxtGp(cex=0.90),
             # axes = gpar(cex = 0.9),
             txt_gp = fpTxtGp(ticks=gpar(cex=1), 
                              xlab = gpar(cex=1))) %>%  
  
  fp_add_header(sex = "Sex", 
                age =  "Age", 
                Pos = "N.positive", 
                tested = "N.tested", 
                Prevalence = "Prevalence",
                liminf = "95%CI liminf", 
                limsup = "95%CI limsup" ) %>% 
  fp_append_row(mean  = 0.06,
                lower = 0.04,
                upper = 0.08,
                sex = "Overall Period Prevalence",
                Pos = "42",
                tested = "718",
                Prevalence = "0.06",
                liminf = "0.04",
                limsup = "0.08",
                position = "last",
                is.summary = TRUE)

 

#---------------------------------

x <-  dtsalm %>% 
  group_by(specie, salmonella) %>% 
  count() %>% 
  pivot_wider(names_from = "salmonella", values_from = "n", values_fill = 0) %>%  
  mutate(`tested` = sum(Neg, Pos)) %>%  
  #mutate(`N° animali analizzati` = sum(Neg, Pos)) %>%  
  #select(`Specie animale` = specie, `N° Positivi` = Pos, `N° animali analizzati`) 
  select(-Neg)

xx <- binom.bayes(
  x = 3, n = 27,
  type = "highest", conf.level = 0.95, tol = 1e-9)



wolves <- dtS %>% filter(specie == "Wolves")

wolves <- wolves %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round,2)) %>% 
  select(-specie)


wolves %>% ungroup() %>% 
  forestplot(labeltext = c( sex,age, Pos,
                            tested,  Prevalence, liminf,limsup),
             clip = c(0,1),
             xlog = FALSE, 
             title = "Wolves") %>%  
  
  fp_add_header(sex = "Sex", 
                age =  "Age", 
                Pos = "N.positive", 
                tested = "N.tested", 
                Prevalence = "Prevalence",
                liminf = "95%CI liminf", 
                limsup = "95%CI limsup" ) %>% 
  
  fp_append_row(mean  = 0.12,
                lower = 0.02,
                upper = 0.25,
                sex = "Overall Period Prevalence",
                Pos = "3",
                tested = "27",
                Prevalence = "0.12",
                liminf = "0.02",
                limsup = "0.25",
                position = "last",
                is.summary = TRUE)



dt <- read_excel("C:/Users/vito.tranquillo/Desktop/Git Projects/gtsad.Gambi/meta.xlsx")


dt <- dt[, c(1, 2,3, 4, 5, 6, 9)]

dt <- dt %>% 
  arrange(Anno) %>% 
  mutate(ID = paste0("Study-", seq(1:nrow(.))))

# p <- rbeta(1000000, shape1 = 3+1, shape2 = 188-3+1)
# 
# p %>% tibble() %>% View()
# ggplot()+
#   aes(p)+
#   geom_density()
# 
# 
#  mybeta <- function(n, a, b){
#    
#    rbeta(n, a+1, b-a+1)
#  }
# 
# 
# x <- mybeta(100, a = dt$`N° Positivi`, b = dt$`N° animali analizzati`)


#amrbib <- read_excel(here("ANALYSIS",  "data", "raw",  "meta.xlsx"))
#amrbib<-amrbib %>% 
 # filter(articolo!="8") %>% 
  #filter(articolo!="10")

options(digits=2)

resbinom <- binom.bayes(
  x = dt$`N° Positivi`, n = dt$`N° animali analizzati`,
  type = "highest", conf.level = 0.95, tol = 1e-9)


dt<-cbind(dt, resbinom[,6:8])


# dt %>% 
#   ggplot( aes(y=mean,ymin=lower, ymax=upper, x=ID))+
#   geom_point( size=2)+geom_linerange(color=grey, size=.6)+
#   coord_flip()+
#   # theme_ipsum_rc(axis_title_just = "mc")+
#   facet_wrap(~ `Specie animale`,  scales = "free")+
#   labs(x="", y="Posterior Bayesian Estimated Prevalence with 95% Compatibility Interval")

 

Tasso <- dt  %>% filter(`Specie animale` == "Tasso")  %>% 
select(Study = ID, Year = Anno, Country = Nazione, 
       pos = `N° Positivi`,
       tested = `N° animali analizzati`, 
        mean,   lower, 
          upper) %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
    across(where(is.double), round,2))


Tasso %>% 
  forestplot(labeltext = c( Study, Year, Country, pos,
                            tested,  Prevalence, liminf,limsup),
             clip = c(0,1),
             xlog = FALSE, 
             title = "Badger") %>% 
  
  fp_add_header(Study = "Study", 
                Year =  "Year", 
                Country = "Country", 
                pos = "N.positive", 
                tested = "N.tested", 
                Prevalence = "Prevalence",
                liminf = "95%CI liminf", 
                limsup = "95%CI limsup" ) %>% 
  fp_append_row(mean  = 0.117,
                lower = 0.072,
                upper = 0.165,
                Study = "Our Study",
                Year = "2022",
                Country = "Italy",
                pos = "21",
                tested = "182",
                Prevalence = "0.117",
                liminf = "0.072",
                limsup = "0.165",
                position = "last",
                is.summary = TRUE)
    
  









 

  Volpe <- dt %>% filter(`Specie animale` == "Volpe") 
  

Volpe <- dt  %>% filter(`Specie animale` == "Volpe")  %>% 
  select(Study = ID, Year = Anno, Country = Nazione, 
         pos = `N° Positivi`,
         tested = `N° animali analizzati`, 
         mean,   lower, 
         upper) %>% 
  mutate(Prevalence = mean,
         liminf = lower,
         limsup = upper,
         across(where(is.double), round,2))



Volpe %>% 
  forestplot(labeltext = c( Study, Year, Country, pos,
                            tested,  Prevalence, liminf,limsup),
             clip = c(0 , 1),
             xlog = FALSE,
             title = "Fox") %>% 
  
  fp_add_header(Study = "Study", 
                Year =  "Year", 
                Country = "Country", 
                pos = "N.positive", 
                tested = "N.tested", 
                Prevalence = "Prevalence",
                liminf = "95%CI liminf", 
                limsup = "95%CI limsup" ) %>% 
  fp_append_row(mean  = 0.059,
                lower = 0.042,
                upper = 0.0765,
                Study = "Our Study",
                Year = "2022",
                Country = "Italy",
                pos = "42",
                tested = "718",
                Prevalence = "0.06",
                liminf = "0.042",
                limsup = "0.08",
                position = "last",
                is.summary = TRUE)

















# tabletextV <- cbind( c("Year",Volpe$Anno),c("Study", Volpe$ID),c("Country", Volpe$Nazione),
#                      c("#positive", Volpe$`N° Positivi`),
#                      c("#tested", Volpe$`N° animali analizzati`),
#                      c("Prevalence", round(Volpe$mean,2)),
#                      c("lim inf", round(Volpe$lower,2)), 
#                      c("lim sup", round(Volpe$upper, 2)))
# 
# mv<- c(NA, Volpe$mean)
# lv <- c(NA,Volpe$lower)
# uv <- c(NA, Volpe$upper)
# 
# 
# 
# 
# forestplot(tabletextV, mv, lv, uv)


## provare anche https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-intro.html

#______________________________________________________________________________________________________________
