library(haven)
library(visdat)

cfps2014comm <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014comm_201906.dta")
cfps2014comm <- cfps2014comm%>%
  filter(ca201_a_)
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
famcon14_clan <- left_join(famconf14,cfps2014comm,by="cid14")
famecon14 <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014famecon_201906.dta")
famcon14_clan_full <- full_join(famcon14_clan,famecon14,by="fid14")

#str(famecon14)
#dim(famconf14)
#dim(famcon14_clan)
#str(famcon14_clan$tb4_a14_p)

dim(famcon14_clan_full)
identical(famcon14_clan$urban14.x,famcon14_clan$urban14.y)

#table(famcon14_clan_full$cid10==-8)


table(famcon14_clan_full$tb3_a14_p)
famcon14_clan_full$tb4_a14_p


facon14_clan_clean <- famcon14_clan_full %>%
  dplyr::filter(clan!=-1)%>%
  dplyr::filter(!is.na(cid10))%>%
  dplyr::filter(tb2_a_p!=-8)%>%
  #dplyr::filter(qa301_a14_f==1|qa301_a14_f==3)%>% 
  dplyr::filter(qa301_a14_p==1|qa301_a14_p==3)%>% #本人户口类型
  #dplyr::filter(qa301_a14_p!=-8)%>% #母亲
  dplyr::filter(urban14.x!=-8)%>%
  rename(urban = urban14.x)%>%
  dplyr::filter(fincome1>=0)%>% #收入
  rename(fincome = fincome1)%>%
  mutate(hukou=case_when(
    qa301_a14_p==3~1,
    qa301_a14_p==1~0
  )) %>%
  mutate(gender = case_when(
    tb2_a_p == 1 ~1,
    tb2_a_p == 0 ~0
  ))%>%
  mutate(age=2014-tb1y_a_p)%>%
  dplyr::filter(tb3_a14_p>=1)%>%
  mutate(marriage=case_when(
    tb3_a14_p==1~0,
    tb3_a14_p>=2~1))%>%
  mutate(firchild_year=case_when( #通过计算长子年龄来计算结婚年龄
    tb1y_a_c1==-8~2015,
    tb1y_a_c1>=0~tb1y_a_c1
  ))%>%
  dplyr::filter(firchild_year<=2014)%>%  # 剔除了未婚个体
  mutate(marry_age=firchild_year-tb1y_a_p)%>% 
  dplyr::filter(marry_age<=60&marry_age>=14) #剔除异常值

table(facon14_clan_clean$marry_age)

facon14_clan_clean%>%
  ggplot(aes(x=clan,y=marry_age))+
  geom_point()+geom_smooth(method = lm)


#nrow(facon14_clan_clean)
# 控制变量，收入，户口，性别，年龄
reg <- lm(data = facon14_clan_clean,formula = marry_age~clan+gender+fincome+age+urban)

# 回答的时候不会涉及到有没有结婚证的问题

summary(reg)
