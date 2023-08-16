
rda<-data.frame(nutrient = c('calcium', 'iron', 'selenium', 'zinc', 'vitamin_a', 'omega_3'),
				nutrient2 = c('calcium.mg', 'iron.mg', 'selenium.mug', 'zinc.mg', 'vitamin_a.mug', 'omega_3.g'))


## RNI: Recommended Nutrient Intake 
## women betwee 18-65 years, RNI per day
ca<-1000
fe<-29.4
se<-26
zn<-4.9
vita<-500
omega<-1.1

rda$rni_women = c(ca, fe, se, zn, vita, omega)

## RDA: Recommended Daily Allowance for kids 6-months to 5 years
# https://www.nationalacademies.org/our-work/summary-report-of-the-dietary-reference-intakes
## calculating average for 6 months - <5 years (4.5 years)
ca<-round((260*182.5 + 700 * 1095 + 1000*365) / (1095+365+182.5), 2)
fe<-round((11*182.5 + 7 * 1095 + 10*365) / (1095+365+182.5), 2)
se<-round((20*182.5 + 20 * 1095 + 30*365) / (1095+365+182.5), 2)
zn<-round((3*182.5 + 3 * 1095 + 5*365) / (1095+365+182.5), 2)
vita<-round((500*182.5 + 300 * 1095 + 400*365) / (1095+365+182.5), 2)
omega<-round((0.5*182.5 + 0.7 * 1095 + 0.9*365) / (1095+365+182.5), 2)

rda$rda_kids = c(ca, fe, se, zn, vita, omega)