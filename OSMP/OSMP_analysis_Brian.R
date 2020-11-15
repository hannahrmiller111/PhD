library(ggplot2)
library(dplyr)
library(lme4)
library(LMERConvenienceFunctions)

options(stringsAsFactors = F)
setwd('C:/Users/anacb1/OneDrive - City of Boulder/Desktop/OsmpLocal/TallOatN/')
d <- N_cycling


# d %>%
#   group_by(Season, Invasion, Site) %>%
#   summarise(mn = mean(get(var)),
#             sd = sd(get(var)),
#             n = length(get(var)),
#             se = sd / sqrt(n)) %>%
#   ggplot(aes(x = Invasion, y = mn)) +
#   geom_point() +
#   geom_errorbar(aes(ymax = mn + se, ymin = mn - se), width = 0.1) +
#   facet_grid(~Season ~ Site) +
#   geom_jitter(data = d, aes(x = Invasion, y = get(var), color = plot), width = .1) +
#   ylab(var)

# 1. Visualize
## Net Mineralization
d %>% 
  group_by(Season, Invasion, Site) %>% #why important to group?
  summarise(mn = mean(Net_Mineralization), 
            sd = sd(Net_Mineralization), 
            n = length(Net_Mineralization), 
            se = sd / sqrt(n)) %>% 
    ggplot(aes(x = Invasion, y = mn)) + labs(title = "Net Mineralization") +
      geom_point() +
      geom_errorbar(aes(ymax = mn + se, ymin = mn - se), width = 0.1) +
      facet_grid(~Season ~ Site) +
      geom_jitter(data = d, aes(x = Invasion, y = Net_Mineralization), col = 2, width = .1)

# Net Nitrification
d %>% 
  group_by(Season, Invasion, Site) %>% 
  summarise(mn = mean(Net_Nitrification), 
            sd = sd(Net_Nitrification), 
            n = length(Net_Nitrification), 
            se = sd / sqrt(n)) %>% 
  ggplot(aes(x = Invasion, y = mn)) + labs(title = "Net Nitrification") +
  geom_point() +
  geom_errorbar(aes(ymax = mn + se, ymin = mn - se), width = 0.1) +
  facet_grid(~Season ~ Site) +
  geom_jitter(data = d, aes(x = Invasion, y = Net_Nitrification), col = 2, width = .1)

# % water content
d %>% 
  group_by(Season, Invasion, Site) %>% 
  summarise(mn = mean(percent_water), 
            sd = sd(percent_water), 
            n = length(percent_water), 
            se = sd / sqrt(n)) %>% 
  ggplot(aes(x = Invasion, y = mn)) + labs(title = "% Water Content") +
  geom_point() +
  geom_errorbar(aes(ymax = mn + se, ymin = mn - se), width = 0.1) +
  facet_grid(~Season ~ Site) +
  geom_jitter(data = d, aes(x = Invasion, y = percent_water), col = 2, width = .1)

# Biomass versus % TO
d %>% subset(Invasion=='Invaded') %>% 
  ggplot(aes(x = Biomass_.g._per_m.2, y = percent_TO, color = Site)) + 
  geom_point() + 
  ggtitle("Biomass versus %TO") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("%TO")


# N Min versus % water content
d %>% subset(Season=='Summer20') %>% 
  ggplot(aes(x = percent_water, y = Net_Mineralization, color = Site)) + 
  geom_point() + 
  ggtitle("% water versus Net Min") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Net Mineralization") +
  geom_smooth(method = "lm", se = FALSE)

# N Nitrification versus % water content
d %>% subset(Season=='Summer20') %>% 
  ggplot(aes(x = percent_water, y = Net_Nitrification, color = Site)) + 
  geom_point() + 
  ggtitle("% water versus Net Nit") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Net Nitrification") #+
  #geom_smooth(method = "lm", se = FALSE)

## clearly, Spring20 shows no patterns, so don't ever bother fitting models to the data
## if you look at both when they're strongly correlated you end up ignoring one. But probably worth  looking at both
## approach two steps: show correlation, then show basic models the same result. Don't choose one that looks better, choose one that makes more sense
## choose which one is most getting at our question/what we are hypothesizing 
## probably something going on and don't have enough power to see it. 

# 2. Note high correlation of the two response variables, so not bother testing 2nd response variable
## Assume that anything that is significant wih mineralization would also be for nitrification?
cor.test(d$Net_Mineralization, d$Net_Nitrification)


# 3. Basic Model

# Net Min as function of invasion
m1 <- lm(Net_Mineralization ~ Invasion * Site, data = subset(d, Season == 'Summer20'))
#does this assume Site is also fixed in this case? 

summary(aov(m1)) # sign interaction only ## Is this an anova? don't understand this part 
summary(m1) # r2  = 0.45; 
  # coyote b = -0.5554 (biggest difference)
  # ncar b = -0.5554 + 0.2794 = -0.276 (1/2 as big as coyote)
  # shar b = -0.5554 + 0.8031 = 0.2477 (wrong direction!)
  # for reference later, the avg of these three betas is -0.1945667

# Net Nit as function of invasion
m2 <- lm(Net_Nitrification ~ Invasion * Site, data = subset(d, Season == 'Summer20'))

summary(aov(m2)) # sign interaction only ## Is this an anova? don't understand this part 
summary(m2) # r2  = 0.45; 
# coyote b = -0.4945 (biggest difference)
# ncar b = -0.4945 + 0.2514 = -0.2431 (1/2 as big as coyote)
# shar b = -0.4945 + 0.6071 = 0.1126 (wrong direction!)

# Net Min as function of invasion
m3 <- lm(Net_Mineralization ~ Invasion * Biomass_.g._per_m.2, data = subset(d, Season == 'Summer20'))

summary(aov(m3)) # sign interaction only ## Is this an anova? don't understand this part 
summary(m3) # r2  = 0.45; 
# none are significant

# Net Nit as function of biomass
m4 <- lm(Net_Nitrification ~ Biomass_.g._per_m.2 * Site, data = subset(d, Season == 'Summer20'))

summary(aov(m4)) # sign interaction only ## Is this an anova? don't understand this part 
summary(m4) # r2  = 0.45; 
# none are significant


# 4. Three constituent models

lapply(split(d, d$Site), function(x){ #what does the split do? divides data into three things, one per site
  #stored alphabetically. for each instance in the list, do the following. 
  # data becomes x for first instance in the list. 
  #doesn't include site anymore because pointing at just one site each time it runs
  m <- lm(Net_Mineralization ~ Invasion, data = subset(x, Season == 'Summer20'))
  summary(m)
})
  
## NCAR (coyote?) significant (b = -0.55 diff), Coyote (NCAR?) trending (b = -0.276), Shan not significant


# 5. Mixed effects approach 

# main advantage of mixed effects model is they "economize on the number of degrees
# of freedom used up by the factor levels"    


# 5A. varying intercepts model (ea site gets an intercept)

me1 <- lmer(Net_Mineralization ~ Invasion + (1 | Site), data = subset(d, Season == 'Summer20'))

#Add Season if analyzing data across seasons?
#me2 <- lmer(Net_Mineralization ~ Invasion + Season + (1 | Site), data = subset(d, Season == d))

fixef(me1)[2] # fixed effect; b = -0.194552, a decrease in mineralization in uninvaded sites
 # note, this matches the average beta above


confint(me1) # effectively a signficance test;
  # if conf interval doesn't overlap 0, then it's significant; 
  # in this case the beta of -0.1945 has a CI of (-0.45, 0.067), 
  # which overlaps 0, meaning it's not signficant, but it's kind of close

# there is a cheater/unofficial way to get pvalues, like this
pamer.fnc(me1) # p = 0.15

coef(me1) # random effects; diff intercepts for ea. Site; coyote > ncar > shanahan


# 5B. varying intercepts and slopes model (ea site gets an intercept *and* a slope)

me2 <- lmer(Net_Mineralization ~ Invasion + (Invasion | Site), data = subset(d, Season == 'Summer20'))
fixef(me2)[2] # same as me1 above
confint(me2) # confidence interval way wider than me1 above
pamer.fnc(me2)
coef(me2) # random effects; note, diff intercepts and slopes for each random effect

# note, lack on model convergence and the -1 Corr in the summary(me2) for random effects
# means that the model needs a lot more Sites to work well
# in other words, this model is overparameterized and sucks



