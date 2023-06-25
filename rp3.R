library(haven)
library(visdat)

cfps2014comm <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014comm_201906.dta")
cfps2014comm <- cfps2014comm%>%
  rename(clan=ca201_a_8)%>%
  dplyr::filter(clan!=-9)%>%
  dplyr::filter(clan!=-1)%>%
  dplyr::filter(clan!=-2)%>%
  dplyr::filter(clan!=-10)
#dplyr::filter(clan!=-1)
nrow(cfps2014comm)
cfps2014comm$clan

str(cfps2014comm$clan)
table(cfps2014comm$clan)# 这里的-8实际上就是为没有
cfps2014comm$clan[which(cfps2014comm$clan==-8)]=0
table(cfps2014comm$clan)



#View(cfps2014comm)随时观察变量
cfps2014comm%>%
  select(cid14,cid10,ca3_s_6,ca3_s_7)->comm14

famconf14 <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014famconf_170630.dta")


fa_male <- famconf14 %>%
  dplyr::filter(tb2_a_p==1)%>%
  dplyr::filter(tb3_a14_p==2)%>%
  select(fid14,pid_s,countyid14,cid14,code_a_p,tb1y_a_p,tb4_a14_p,qa301_a14_p,qa302_a14_p,code_a_s)%>%
  rename(code_w=code_a_s)%>%
  rename(pid_w=pid_s)%>%
  rename(code_h=code_a_p)%>%
  rename(birth_h=tb1y_a_p)%>%
  rename(edu_h=tb4_a14_p)%>%
  rename(hukou_h=qa301_a14_p)%>%
  rename(hukou_area_h=qa302_a14_p)

dim(fa_male)
dim(fa_female)
fa_female <- famconf14 %>%
  dplyr::filter(tb2_a_p==0)%>%
  dplyr::filter(tb3_a14_p==2)%>%
  select(pid,code_a_p,tb1y_a_p,tb4_a14_p,qa301_a14_p,qa302_a14_p)%>%
  rename(code_w=code_a_p)%>%
  rename(pid_w=pid)%>%
  rename(birth_w=tb1y_a_p)%>%
  rename(edu_w=tb4_a14_p)%>%
  rename(hukou_w=qa301_a14_p)%>%
  rename(hukou_area_w=qa302_a14_p)

fa_couple <- inner_join(fa_male,fa_female,by="pid_w",relationship = "many-to-many")
#dim(fa_couple)
#dim(fa_female)
cou_clan <- left_join(fa_couple,cfps2014comm,by="cid14")
famecon14 <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014famecon_201906.dta")
cou_clan <- left_join(cou_clan,famecon14,by="fid14")

cou_clan$hukou_h

dim(cou_clan)
# 配偶年龄差异

cou_clan%>%
  mutate(age_gap=birth_w-birth_h)%>%
  dplyr::filter(abs(age_gap)<=100)%>%
  #summarise(mean(age_gap),n=n())
ggplot()+
  geom_density(mapping = aes(age_gap))+theme_bw()

mean(cou_clan_clean$age_gap)

reg <- lm(data = cou_clan_clean,age_gap~clan+edu_w+edu_h+hukou+fz202)
#主回归系数为负，则说明clan降低 age gap
corr()

summary(reg)
# 再对家庭进行控制
reg2 <- lm(data = cou_clan_clean,age_gap~clan+edu_h+hukou)

summary(reg2)
#table(cfps2014comm$ch6)


library(tidyverse)
  
cou_clan_clean <- cou_clan%>%
  mutate(age_gap=birth_w-birth_h)%>%
  dplyr::filter(abs(age_gap)<=100)%>%
  dplyr::filter(edu_w>=1)%>%
  dplyr::filter(edu_h>=1)%>%
  dplyr::filter(hukou_h==3|hukou_h==1)%>%
  mutate(hukou=case_when(
    hukou_h==1~0,
    hukou_h==3~1
  ))%>%
  dplyr::filter(hukou_area_h>=1)%>%
  drop_na(fz202)%>%
  rename(fz202)
  drop_na(fincome1)%>%
  dplyr::filter(fz202>=0)%>%
  dplyr::filter(1000000>=fincome1&fincome1>=0)
  
 
cou_clan_clean$fz202
cou_clan_clean$age_gap


table(cou_clan_clean$age_gap)
nrow(facon14_clan_clean2)

table(facon14_clan_clean2$clan)

#facon14_clan_clean2$gender
reg2 <- lm(data = facon14_clan_clean2,formula = age_gap~clan+hukou+age_h+eduyear+fincome)
summary(reg2)

ggplot(data = facon14_clan_clean2,aes(x=clan,y=age_gap))+
  geom_point()+geom_smooth(method = lm)+theme_bw()
