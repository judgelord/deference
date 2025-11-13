install.packages("stargazer")
library(stargazer)
library(ggplot2)
?coefficients
install.packages("stargazer")
library(stargazer)
library(ggplot2)
?ggplot2
?coefplot
data=votes2
after<-votes2[which(votes2$yrdecid>1983),]
ruledata<-votes2[which(votes$rulemake==1),]


SCOTUSdata <- votes2
names(SCOTUSdata)[names(SCOTUSdata)=="agncydir"] <- "agencypolicydirection"
names(SCOTUSdata)[names(SCOTUSdata)=="aft_chev"] <- "post_regime"
names(SCOTUSdata)[names(SCOTUSdata)=="attitude"] <- "ideo"
names(SCOTUSdata)[names(SCOTUSdata)=="def_amic"] <- "amiciDefer"
names(SCOTUSdata)[names(SCOTUSdata)=="rev_amic"] <- "amiciOppose"
names(SCOTUSdata)[names(SCOTUSdata)=="agencyhd"] <- "fire"
names(SCOTUSdata)[names(SCOTUSdata)=="columncm"] <- "length"
names(SCOTUSdata)[names(SCOTUSdata)=="agencyhd"] <- "fire"
names(SCOTUSdata)[names(SCOTUSdata)=="party"] <- "typeofpartyopposingdeference"
names(SCOTUSdata)[names(SCOTUSdata)=="dparty"] <- "typeofpartyadvocatingdeference"
names(SCOTUSdata)[names(SCOTUSdata)=="rulemaking"] <- "NPRMrulemaking"
names(SCOTUSdata)[names(SCOTUSdata)=="rulemake"] <- "rulemaking"
names(SCOTUSdata)[names(SCOTUSdata)=="votedef"] <- "deference"
SCOTUSdata$scotus = 1
write.csv(SCOTUSdata, file="SCOTUSadmincases")

# in SOTUSadmincases dataset "scotus" = 1
# new var in lower court cases "scotus" = 0
data$scotus = 0
combined <- merge(read.csv("SCOTUSadmincases.csv"), data)



# do scotus admin cases differ from lower court cases on key vars? 
scotus.selection <- glm(scotus ~
      post_regime + 
      agencypolicydirection * ideo + 
      amiciDefer + 
      amiciOppose + 
      #as.factor(typeofpartyopposingdeference) + 
      #as.factor(typeofpartyadvocatingdeference) + 
      fire + 
      rulemaking + 
      length, 
    family = binomial(link = "logit"), 
          data = combined)
summary(scotus.selection)
visreg(scotus.selection, scale=c("response"))



rulemake.logit <- glm(votedef ~ 
      aft_chev + 
      yrdecid +
      #attitude +
      #agncydir +
      agncydir*attitude +
      rev_amic +
      def_amic +
      amicus_u +
      as.factor(party) +
      as.factor(dparty) +
      agencyhd +
      columncm +
      rulemake,
      #rulemaking,
      #specexpt +
      #log(rulecomments) + 
      #morepro + 
      # overreach, 
      family=binomial(link="logit"), data=after)
summary(rulemake.logit)
visreg(new.rulemaking.logit, scale=c("response"))

rulemaking.logit <- glm(votedef ~ 
                              aft_chev + 
                              yrdecid +
                              agncydir*attitude +
                              rev_amic +
                              def_amic +
                              amicus_u +
                              as.factor(party) +
                              as.factor(dparty) +
                              agencyhd +
                              columncm +
                              # rulemake,
                              rulemaking,
                            family=binomial(link="logit"), data=after)
summary(rulemaking.logit)
visreg(rulemaking.logit, scale=c("response"))


rulerule.logit <- glm(votedef ~ 
                        aft_chev + 
                        yrdecid +
                        # attitude +
                        #agncydir +
                        agncydir*attitude +
                        rev_amic +
                        def_amic +
                        amicus_u +
                        #as.factor(party) +
                        #as.factor(dparty) +
                        agencyhd +
                        columncm +
                        #rulemake,
                        rulemaking,
                      family=binomial(link="logit"), data=ruledata)
summary(rulerule.logit)
visreg(rulerule.logit, scale=c("response"))

rulemake.after.logit <- glm(votedef ~ 
                              #aft_chev + 
                              yrdecid +
                              # attitude +
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
                            family=binomial(link="logit"), data=after)
summary(rulemake.after.logit)
visreg(rulemake.after.logit, scale=c("response"))




rulemake.more.logit <- glm(votedef ~ 
                               specexpt +
                             reasoned +
                               conflcts +
                               apamenti +
                               leghist +
                               #aft_chev + 
                               agncydir*attitude +
                               rev_amic +
                               def_amic +
                               amicus_u +
                               as.factor(party) +
                               as.factor(dparty) +
                               agencyhd +
                               columncm +
                               rulemake,
                               #rulemaking,
                             #log(rulecomments) + 
                             #morepro +
                             #overreach, 
                             family=binomial(link="logit"), data=after)
summary(rulemake.more.logit)
visreg(rulemake.more.logit, scale=c("response"))

rulemaking.more.logit <- glm(votedef ~ 
                               specexpt +
                               conflcts +
                               apamenti +
                               leghist +
                               aft_chev + 
                               agncydir*attitude +
                               rev_amic +
                               def_amic +
                               amicus_u +
                               as.factor(party) +
                               as.factor(dparty) +
                               agencyhd +
                               columncm +
                               #reasoned +
                               #rulemake,
                               rulemaking,
                               #log(rulecomments) + 
                               #morepro +
                               #overreach, 
                             family=binomial(link="logit"), data=votes2)
summary(rulemaking.more.logit)
visreg(rulemaking.more.logit, scale=c("response"))

rulemaking.vars.logit <- glm(votedef ~ 
                               aft_chev + 
                               #conflcts +
                               agncydir*attitude +
                               #rev_amic +
                               #def_amic +
                               #amicus_u +
                              #as.factor(party) +
                               #as.factor(dparty) +
                               agencyhd +
                               columncm +
                               #rulemake,
                               #rulemaking,
                               #specexpt +
                               log(rulecomments) + 
                               morepro +
                               overreach, 
                             family=binomial(link="logit"), data=votes2)
summary(rulemaking.vars.logit)
visreg(rulemaking.vars.logit, scale=c("response"))

stargazer(rulemake.more.logit, rulemaking.more.logit, rulemaking.vars.logit, title="Deference in Rulemaking Cases (including policymaking variables)")







rulemake.ideological <- glm(rulemake ~ 
                              aft_chev + 
                              #yrdecid +
                              agncydir*attitude +
                              rev_amic +
                              def_amic +
                              amicus_u +
                              as.factor(party) +
                              as.factor(dparty) +
                              agencyhd +
                              columncm,
                            family=binomial(link="logit"), data=after)
summary(rulemake.ideological)
visreg(new.rulemaking.logit, scale=c("response"))

rulemaking.ideological <- glm(rulemaking ~ 
                              aft_chev + 
                              #yrdecid +
                              agncydir*attitude +
                              rev_amic +
                              def_amic +
                              amicus_u +
                              as.factor(party) +
                              as.factor(dparty) +
                              agencyhd +
                              columncm,
                            family=binomial(link="logit"), data=after)
summary(rulemaking.ideological)
visreg(rulemaking.idoelogical, scale=c("response"))

stargazer(rulemake.ideological, rulemaking.ideological, title="Rulemaking Covariates")




rulemake.selection <- glm(rulemake ~ 
                              #aft_chev + 
                              #agncydir*attitude +
                              as.factor(party) +
                              as.factor(dparty) +
                              agencyhd +
                              columncm +
                              conflcts +
                              as.factor(rev_stan),
                            family=binomial(link="logit"), data=after)
summary(rulemake.selection)


rulemaking.selection <- glm(rulemaking ~ 
                                #aft_chev + 
                                #agncydir*attitude +
                                as.factor(party) +
                                as.factor(dparty) +
                                agencyhd +
                                columncm +
                              conflcts +
                              as.factor(rev_stan),
                            family=binomial(link="logit"), data=after)
summary(rulemaking.selection)
visreg(new.rulemaking.logit, scale=c("response"))

rulerule.selection <- glm(rulemaking ~ 
                              #aft_chev + 
                              #agncydir*attitude +
                              as.factor(party) +
                              as.factor(dparty) +
                              agencyhd +
                              columncm +
                              conflcts +
                              as.factor(rev_stan),
                            family=binomial(link="logit"), data=ruledata)
summary(rulerule.selection)
visreg(rulerule.selection, scale=c("response"))

# rev_stan
# 1 law, 2 evidence, 3 descretion abuse

stargazer(rulemake.selection, rulemaking.selection, rulerule.selection, title="Comparing Rulmaking and Non-Rulemaking Administrative Law Cases 1984-2001")











threeway.rulemaking.logit <- glm(votedef ~ 
                                   #aft_chev + 
                                   #yrdecid +
                                   agncydir*attitude*rulemaking +
                                   #rev_amic +
                                   #def_amic +
                                   #amicus_u +
                                   #as.factor(party) +
                                   #as.factor(dparty) +
                                   #agencyhd +
                                   #columncm +
                                   rulemaking,
                                 #specexpt +
                                 #log(rulecomments) + 
                                 #morepro + 
                                 #overreach, 
                                 family=binomial(link="logit"), data=votes2)
summary(threeway.rulemaking.logit)
# all specifications only rulemaking and agncydir*ideology are sig (except def amici)


rulerule.coef<-coef(rulerule.logit)
plot(rulerule.coef)


stargazer(rulemake.logit, rulemaking.logit, rulerule.logit, title="Votes for Deference in Administrative Law Cases")
stargazer(rulemake.ideological, rulemaking.ideological, title="Rulemaking Covariates")