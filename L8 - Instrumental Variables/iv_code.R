babydata <- data.frame(AE98) %>% mutate(BoyBoy = 1* (boy1st ==1 & boy2nd==1),
                                        GirlGirl = 1* (boy1st ==0 & boy2nd==0))
 
m5= feols(worked ~ morekids + black + hisp +othrace + age + agefst |
        samesex ~ boy1st + boy2nd, data=babydata)

flm1 = feols(morekids ~ samesex + black + hisp + othrace, data=babydata)
lh1 = linearHypothesis(flm1,"samesex = 0", test="F")

# First Stage Test in AE w/ Multiple Instruments
flm2 = feols(morekids ~ BoyBoy + GirlGirl + black + hisp + othrace, data=babydata)
lh2 = linearHypothesis(flm2,c("BoyBoy","GirlGirl"), test="F")

