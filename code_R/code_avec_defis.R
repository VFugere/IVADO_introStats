
### IVADO Data Trek 2022 : Intro stats avec R

rm(list=ls())

library(dplyr)
library(ggplot2)
library(palmerpenguins)
library(gapminder)
data("starwars")
data("penguins")
data("gapminder")
# #au besoin
# install.packages(c("penguins","gapminder"))

head(penguins)

## Défi #1
head(starwars)
glimpse(starwars)
## Défi #1

## Test de t

hist(rnorm(10000))

penguins %>%
  filter(species == 'Adelie') %>% 
  group_by(sex) %>% 
  summarize(masse.moy = mean(body_mass_g))

penguins %>%
  filter(species == 'Adelie', sex != 'NA') %>%
  ggplot(aes(x=body_mass_g, colour=sex)) + geom_density()

adelie <- penguins %>%
  filter(species == 'Adelie', sex != 'NA')

t.test(body_mass_g ~ sex, data = adelie)

format(2.2e-16, scientific = F)

modele <- lm(body_mass_g ~ sex, data = adelie)
summary(modele)

nab_tat <- starwars %>% filter(homeworld %in% c('Naboo','Tatooine'))
ggplot(aes(x=homeworld,y=height),data=nab_tat) + geom_boxplot()

## Défi #2
t.test(height ~ homeworld, data = nab_tat)
modele <- lm(height ~ homeworld, data = nab_tat)
summary(modele)
## Défi #2

## ANOVA

modele <- lm(body_mass_g ~ species, data = penguins)

summary(modele)
levels(penguins$species)
coef(modele)

ggplot(aes(x=species,y=body_mass_g),data=penguins) + geom_boxplot()

anova(modele)
TukeyHSD(aov(modele))

donnees_2007 <- gapminder %>% filter(year == 2007)
ggplot(aes(x=continent,y=lifeExp),data=donnees_2007) + geom_boxplot()

## Défi #3
modele <- lm(lifeExp ~ continent, donnees_2007)
anova(modele)
TukeyHSD(aov(modele))
## Défi #3

## Suppositions des modèles linéaires

mod.resid <- resid(modele)  # Sortir les résidus
shapiro.test(mod.resid)  # Test de Shapiro-Wilk: p < 0.05 = MAUVAIS!

bartlett.test(lifeExp ~ continent, donnees_2007) # Test de Bartlett: p < 0.05 = MAUVAIS!

## ANOVA avec deux facteurs

penguins <- tidyr::drop_na(penguins)
ggplot(aes(x=species,y=body_mass_g,fill=sex),data=penguins) + geom_boxplot()

modele <- lm(body_mass_g ~ species + sex, data = penguins)
anova(modele)

modele <- lm(body_mass_g ~ species * sex, data = penguins)
anova(modele)

summary(modele)

## Régression

ggplot(aes(x=height,y=mass),data=starwars) + geom_point()

sw <- starwars %>% filter(mass < 1000)
ggplot(aes(x=height,y=mass),data=sw) + geom_point()

modele <- lm(mass ~ height, data = sw)
summary(modele)

mod.resid <- resid(modele)
shapiro.test(mod.resid)  # Test de Shapiro-Wilk: p < 0.05 = MAUVAIS!

sw$log_masse <- log10(sw$mass)
modele <- lm(log_masse ~ height, data = sw)
mod.resid <- resid(modele)
shapiro.test(mod.resid) #yé!

plot(modele, which = 1)

summary(modele)

ggplot(aes(x=height,y=log_masse),data=sw) +
  geom_point() + 
  geom_smooth(method='lm')

coef(modele)
b0 <-  coef(modele)[1]
b1 <-  coef(modele)[2]
c(b0,b1)

ggplot(aes(x=height,y=log_masse),data=sw) +
  geom_point() + 
  geom_abline(intercept = b0, slope = b1)

## Défi #4
modele <- lm(bill_length_mm ~ flipper_length_mm, penguins)
par(mfrow=c(1,2))
plot(modele, which = 1:2)
summary(modele)
ggplot(aes(x=flipper_length_mm,y=bill_length_mm),data=penguins) +
  geom_point() + 
  geom_smooth(method='lm')
## Défi #4

## ANCOVA

ggplot(aes( x=body_mass_g, y=bill_length_mm, colour = species), data = penguins) +
  geom_point() + geom_smooth(method = "lm", aes(fill = species))

modele.sans.interaction <- lm(bill_length_mm ~ body_mass_g + species, data = penguins)
summary(modele.sans.interaction)

modele.avec.interaction <- lm(bill_length_mm ~ body_mass_g * species, data = penguins)
summary(modele.avec.interaction)

## Régression multiple

modele <- lm(bill_length_mm ~ body_mass_g + species + year, data = penguins)
summary(modele)

par(mfrow=c(1,2))
plot(modele, which = 1:2)

sjPlot::plot_model(modele)

modele <- lm(bill_length_mm ~ scale(body_mass_g) + species + scale(year), data = penguins)
sjPlot::plot_model(modele)
