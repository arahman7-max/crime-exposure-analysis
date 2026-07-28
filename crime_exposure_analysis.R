#Assignment 5


#A.1

ggplot(crime_exposure, aes(x = tstatus, y = probcrime)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult = 1.96))+
  facet_wrap(vars(After))

#Before the experiment, the probability of commiting a crime after 
#being exposed to a crime increases slightly compared to people who 
#were not exposed by a crime. After the experiment, this increase 
#quadruples. 

#A.2 

A.2 <- lm(probcrime ~ tstatus + After + 
                  (tstatus*After), data= crime_exposure)
tidy(A.2)

#The probability of committing a crime after exposure to a successful
#crime increases by 29.8 percent. The results is both positive and 
#significant.


#A.3

A.3 <- lm(probcrime ~ tstatus + After + 
            (tstatus*After)+male+white+SocialM, data= crime_exposure)

modelsummary(list(A.2, A.3))

#The causal effect of exposure to crime does not change after 
#controlling for variables such as sex, race, and social mimicry.
#The estimate values are roughly the same as they were in the
#regression in A.2, suggesting that there was no OVB.


#B.1 

mean(smoking$smkstatus)

#About 54.8% of females in the dataset are smokers.


#B.2

mean(smoking$totbmc[smoking$smkstatus==1], na.rm=TRUE) 

#Non-smoking females have a total body bone mineral content of 
#2093.408. Smoking females have a total body bone mineral content of
#1791.067.


#B.3

B.3 <- lm(totbmc ~ smkstatus, data = smoking)
summary(B.3)

#The effect of smoking on total body bone mineral content is 
#significant in this regression. Total body bone mineral content
#increased by 302.34 for non-smoking females.


#B.4.1

matched_data <- matchit(smkstatus ~ Menarche + Race, 
data= smoking, method = "nearest", distance = 'mahalanobis', 
replace = TRUE)

matched_final <- match.data(matched_data)

B.4.1 <- lm(totbmc ~ smkstatus, data = matched_final)

tidy(B.4.1)

#The effect of smoking on total body bone mineral content is not
#significant in this test. 


#B.4.2

matched_data <- matchit(smkstatus ~ Menarche + Race + ses + bmi, 
      data= smoking, method = "nearest", distance = 'mahalanobis', 
                        replace = TRUE)

matched_final <- match.data(matched_data)

B.5.1 <- lm(totbmc ~ smkstatus, data = matched_final)

tidy(B.5.1)

#The effect of smoking on total body bone mineral content is 
#significant in this test. The tests suggests that total body bone
#mineral content increased for non-smoking females by 357 when 
#adding body max index and SES value as controls.


#B.5.1

matched_data <- glm(smkstatus ~ Menarche + Race + ses + bmi, data = 
                      smoking, family = binomial(link = "logit"))

matched_psm <- augment_columns(matched_data, smoking, type.predict 
= "response")%>% rename(psm = .fitted) %>% mutate(ipw = 
      (smkstatus / psm) +  ((1 - smkstatus) / (1 - psm)))

B.5.1 <- lm(totbmc ~ smkstatus, data = matched_psm, 
                weights = ipw)

tidy(B.5.1)

#The effect of smoking on total body bone mineral content is 
#significant in this test. The tests suggests that total body bone
#mineral content increased for non-smoking females by 195. This
#increase, however, decreased from the model in 4.B by 162.


#B.5.2

matched_data <- glm(smkstatus ~ Menarche + Race*ses + bmi, data = 
                      smoking, family = binomial(link = "logit"))

matched_psm <- augment_columns(matched_data, smoking, type.predict 
                               = "response")%>% rename(psm = .fitted) %>% 
  mutate(ipw = (smkstatus / psm) +  ((1 - smkstatus) / (1 - psm)))

B.5.2 <- lm(totbmc ~ smkstatus, data = matched_psm, 
            weights = ipw)
tidy(B.5.2)

#The effect of smoking on total body bone mineral content is 
#significant in this test. The tests suggests that total body bone
#mineral content increased for non-smoking females by 201. This
#value increased from the model in B.5.1 by only about 6


#B.5.3

matched_data <- glm(smkstatus ~ Menarche + Race*ses + bmi + smkhome + frndsmk2, 
                    data = 
                    smoking, family = binomial(link = "logit"))


matched_psm <- augment_columns(matched_data, smoking, type.predict 
                        = "response")%>% rename(psm = .fitted) %>% 
  mutate(ipw = (smkstatus / psm) +  ((1 - smkstatus) / (1 - psm)))

B.5.3 <- lm(totbmc ~ smkstatus, data = matched_psm, 
            weights = ipw)

tidy(B.5.3)

#The effect of smoking on total body bone mineral content is 
#significant in this test. The tests suggests that total body bone
#mineral content increased for non-smoking females by 201. This
#value decreased from the model in B.5.2 by only 2.


modelsummary( list(B.5.1, B.5.2, B.5.3)) 





