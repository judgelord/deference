rulemake.logit <- glm(votedef ~ 
                        #aft_chev + 
                        #yrdecid +
                        #attitude +
                        #agncydir +
                        agncydir*attitude +
                        rev_amic +
                        def_amic +
                        amicus_u +
                        #as.factor(party) +
                        #as.factor(dparty) +
                        agencyhd +
                        columncm +
                        rulemake,
                      #rulemaking,
                      #specexpt +
                      #log(rulecomments) + 
                      #morepro + 
                      # overreach, 
                      family=binomial(link="logit"), data=votes2)
summary(rulemake.logit)
#sjp.glm(rulemake.logit, sortOdds = T)
#visreg(new.rulemaking.logit, scale=c("response"))


rulemaking.logit <- glm(votedef ~ 
                          #aft_chev + 
                          #yrdecid +
                          agncydir*attitude +
                          rev_amic +
                          def_amic +
                          amicus_u +
                          #as.factor(party) +
                          #as.factor(dparty) +
                          agencyhd +
                          columncm +
                          # rulemake,
                          rulemaking,
                        family=binomial(link="logit"), data=votes2)
#sjp.glm(rulemaking.logit, sortOdds = T, axisLabels.y = rulemaking.y)
summary(rulemaking.logit)
#visreg(rulemaking.logit, scale=c("response"))
plot(5)
coefplot(rulemaking.logit, intercept=F, varnames = rulemaking.y)
coefplot(rulemake.logit, add=TRUE, col.pts="red",  intercept=F, offset=0.2)
#coefplot(M3, add=TRUE, col.pts="blue", intercept=F, offset=0.2)

rulemaking.y<-rev(c("Policy*Justice Ideology",
                    "Rulemaking",
                    "Statute Length",
                    "Pres. Can Fire Head",
                    "Unclear Amici",
                    "Deference Amici",
                    "Amici Opposed",
                    "Justice Ideology",
                    "Agency Policy",
                    "Intercept"))