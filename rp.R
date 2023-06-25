library(haven)
library(visdat)

cfps2014comm <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014comm_201906.dta")
cfps2014comm <- cfps2014comm%>%
  rename(clan=ca201_a_8)
  #dplyr::filter(clan!=-1)

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

mainvar <- c("clan","gender","eduyear","eduyear_fa","eduyear_ma","age","urban","fincome","hukou_fa")


facon14_clan_clean <- famcon14_clan_full %>%
  dplyr::filter(clan!=-1)%>%
  dplyr::filter(!is.na(cid10))%>%
  dplyr::filter(tb2_a_p!=-8)%>%
  dplyr::filter(qa301_a14_f==1|qa301_a14_f==3)%>%
  dplyr::filter(tb4_a14_p!=-8)%>%
  dplyr::filter(tb4_a14_f!=-8)%>%
  dplyr::filter(urban14.x!=-8)%>%
  rename(urban = urban14.x)%>%
  dplyr::filter(fincome1>=0)%>%
  rename(fincome = fincome1)%>%
  rename(hukou_fa=qa301_a14_f)%>%
  mutate(gender = case_when(
    tb2_a_p == 1 ~1,
    tb2_a_p == 0 ~0
  ))%>%
  mutate(age=2014-tb1y_a_p)%>%
  mutate(eduyear = case_when(
    tb4_a14_p==8 ~ 23,
    tb4_a14_p==7 ~ 19,
    tb4_a14_p==6 ~ 16,
    tb4_a14_p==5 ~ 15,
    tb4_a14_p==4 ~ 12,
    tb4_a14_p==3 ~ 9,
    tb4_a14_p==2 ~ 6,
    tb4_a14_p==1 ~ 0
  ))%>%
  mutate(eduyear_fa = case_when(
    tb4_a14_f==8 ~ 23,
    tb4_a14_f==7 ~ 19,
    tb4_a14_f==6 ~ 16,
    tb4_a14_f==5 ~ 15,
    tb4_a14_f==4 ~ 12,
    tb4_a14_f==3 ~ 9,
    tb4_a14_f==2 ~ 6,
    tb4_a14_f==1 ~ 0
  )) %>%
  mutate(eduyear_ma = case_when(
      tb4_a14_m==8 ~ 23,
      tb4_a14_m==7 ~ 19,
      tb4_a14_m==6 ~ 16,
      tb4_a14_m==5 ~ 15,
      tb4_a14_m==4 ~ 12,
      tb4_a14_m==3 ~ 9,
      tb4_a14_m==2 ~ 6,
      tb4_a14_m==1 ~ 0
  ))%>%
  dplyr::filter(age>=20&age<=100)%>%
  dplyr::filter(!is.na(eduyear))%>%
  dplyr::filter(!is.na(eduyear_fa))%>%
  dplyr::filter(!is.na(eduyear_ma))%>%
  dplyr::filter(!is.na(gender))%>%
  select(all_of(mainvar))

dim(facon14_clan_clean)
library(VIM)
aggr(facon14_clan_clean, prop = F, number = T)


fin_df <- as.data.frame(facon14_clan_clean)
dim(fin_df)
stargazer::stargazer(fin_df,type = "latex",out = "tex/tab_tex/des.tex",title = "变量统计性描述")


df_fe <- fin_df%>%
  dplyr::filter(gender==0)

df_ma <- fin_df%>%
  dplyr::filter(gender==1)

reg1 <- lm(data = df_fe,formula = eduyear~clan+eduyear_fa+age+hukou_fa)
summary(reg1)

reg2 <- lm(data = df_ma,formula = eduyear~clan+eduyear_fa+eduyear_ma+age+hukou_fa)
summary(reg2)
library(stargazer)

stargazer(reg1,type = "latex",
          title = "回归结果")

table(facon14_clan_clean$clan)
table(facon14_clan_clean$eduyear)

# 加入更多的控制变量

## 家庭收入

formu2 <- formula(eduyear~clan+eduyear_fa+age+urban14.x+fincome1)

reg2 <- lm(data = facon14_clan_clean,formula = formu)

summary(reg2)


# 个体特征

## 性别 年龄

str(facon14_clan_clean$tb2_a_p)
table(facon14_clan_clean$tb2_a_p)
formu3 <- formula(eduyear~clan+eduyear_fa+age+urban14.x+fincome1)

reg3 <- lm(data = facon14_clan_clean,formula = formu3)

summary(reg3)

## 宗族文化与彩礼



## 文化资本
### 参书量 fs8 教育培训支出 fp510？应该是上一辈对该辈的影响

reg4 <- lm(data = facon14_clan_clean,formula = formu4)
summary(reg4)


formu5 <- formula(eduyear~clan+eduyear_fa+age+urban14.x+fincome1)
facon14_clan_clean%>%
  dplyr::filter(gender==1)%>%
  lm(formula = formu5)%>%
  summary()


formu5 <- formula(eduyear~clan+eduyear_fa+age+urban14.x+fincome1)
facon14_clan_clean%>%
  dplyr::filter(gender==0)%>%
  lm(formula = formu5)%>%
  summary()


## 村集体层面

## 教育投资
table(cfps2014comm$ck304)
## 人均纯收入
table(cfps2014comm$ch6)
## 到集镇距离
table(cfps2014comm$cg1)







