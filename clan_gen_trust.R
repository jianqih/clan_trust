library(haven)
library(visdat)
library(tidyverse)
famconf14 <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014famconf_170630.dta")
famecon14 <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014famecon_201906.dta")
ind_df <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014adult_201906.dta")
cfps2014comm <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014comm_201906.dta")
cfps2014comm <- cfps2014comm%>%
  rename(clan=ca201_a_8)%>%
  dplyr::filter(clan!=-9)%>%
  dplyr::filter(clan!=-1)%>%
  dplyr::filter(clan!=-2)%>%
  dplyr::filter(clan!=-10)

#table(cfps2014comm$clan)# 这里的-8实际上就是为没有
cfps2014comm$clan[which(cfps2014comm$clan==-8)]=0

ind_df_fil_1_2 <- ind_df%>%
  dplyr::filter(cid14!=-9)%>%
  dplyr::filter(qn1001>=0)%>%
  mutate(gen_trust=case_when(
    qn1001==5~0,
    qn1001==1~1
  ))

ind_df_join_2 <- left_join(ind_df_fil_1_2,cfps2014comm,by="cid14")

ind_df_fil_1_1 <- ind_df_join_2%>%
  drop_na(clan)

ind_df_fil_2_2 <- ind_df_join_2%>%
  drop_na(clan)%>%
  dplyr::filter(cfps2012_latest_edu>=1)%>%
  mutate(edu=case_when(
    cfps2012_latest_edu==1~0,
    cfps2012_latest_edu==2~6,
    cfps2012_latest_edu==3~9,
    cfps2012_latest_edu==4~12,
    cfps2012_latest_edu==5~15,
    cfps2012_latest_edu==6~16,
    cfps2012_latest_edu==7~19,
    cfps2012_latest_edu==8~23))%>%
  dplyr::filter(p_income>=0)%>%
  dplyr::filter(cfps2014_age>=0)%>%
  rename(marry=qea0)%>%
  rename(transfer=ck205)

ind_df_fil_2_2$p_income[which(ind_df_fil_2_2$p_income==0)]=1

# 家庭层面的控制变量：家庭规模、家庭收入对数、土地是否被征用
famecon14_mat_2 <- famecon14%>%
  select(fincome1,houseasset_gross,fv3,fv4,fv5,fml2014num,fid14)
reg_df_fam_2 <- left_join(ind_df_fil_2_2,famecon14_mat_2,by="fid14")

reg_df_fam_clean1_2 <- reg_df_fam_2%>%
  dplyr::filter(fincome1>=0)%>%
  dplyr::filter(houseasset_gross>=0)%>%
  dplyr::filter(fml2014num>=0)

# 村庄控制变量： 常住人口、少数民族集聚、村人均收入对数、到县城距离
reg_df_vill_2 <- reg_df_fam_clean1_2%>%
  mutate(minority=case_when(
    cb6==1~1,
    cb6==5~0
  ))%>%
  dplyr::filter(cb202>=0)%>%
  dplyr::filter(ch6>=0)%>%
  dplyr::filter(cg2>=0)%>%
  rename(dis_country=cg2)%>%
  rename(residual_vill=cb202)%>%
  rename(income_per_vill=ch6)%>%
  select(gen_trust,clan,cfps2014_age,cfps_minzu,p_income,edu,cfps_gender,cfps_party,marry,fml2014num,fincome1,residual_vill,income_per_vill,dis_country,minority,transfer,qn10021,qn10022,qn10023,qn10024,qn10026,qn701,qg1,qg2)

reg_wocontrol2 <- lm(data = ind_df_fil_1_1,formula = gen_trust~clan)
summary(reg_wocontrol2)
reg_ind2 <- lm(data = ind_df_fil_2_2,formula = gen_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marry)
reg_fam2 <- lm(data = reg_df_fam_clean1_2,formula = gen_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marry+fml2014num+log(fincome1))
reg_cou2 <- lm(data = reg_df_vill_2,formula = gen_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marry+fml2014num+log(fincome1)+minority+dis_country+log(income_per_vill)+residual_vill)

covar_label2 <-  c("宗族祠堂", "年龄","是否为少数民族", "个人对数收入", "受教育水平", "性别","是否党员","是否已婚","家庭规模","家庭对数收入","村庄是否为少数民族聚集区","村庄到县城距离","村庄人均对数收入","村庄常住人口数")

stargazer::stargazer(reg_wocontrol2,reg_ind2,reg_fam2,reg_cou2,
                     type = "latex",
                     out = "tex/tab_tex/benchmark2.tex",
                     title = "宗族文化与他人信任回归结果",
                     column.sep.width="0pt",
                     dep.var.labels="对他人的信任水平",
                     covariate.labels =covar_label2,
                     no.space=TRUE,
                     omit.stat=c("LL","ser","f"),
                     label = "regression2")

stargazer::stargazer(reg_wocontrol2)



reg_wocontrol2_logit <- glm(data = ind_df_fil_1_1,formula = gen_trust~clan,family = binomial(link = "logit"))
summary(reg_wocontrol2)
reg_ind2_logit <- glm(data = ind_df_fil_2_2,formula = gen_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marry,family = binomial(link = "logit"))
reg_fam2_logit <- glm(data = reg_df_fam_clean1_2,formula = gen_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marry+fml2014num+log(fincome1),family = binomial(link = "logit"))
reg_cou2_logit <- glm(data = reg_df_vill_2,formula = gen_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marry+fml2014num+log(fincome1)+minority+dis_country+log(income_per_vill)+residual_vill,family = binomial(link = "logit"))


stargazer::stargazer(reg_wocontrol2_logit,reg_ind2_logit,reg_fam2_logit,reg_cou2_logit,
                     type = "latex",
                     out = "tex/tab_tex/benchmark2_logit.tex",
                     title = "宗族文化与他人信任逻辑回归结果",
                     column.sep.width="0pt",
                     dep.var.labels="对他人的信任水平",
                     covariate.labels =covar_label2,
                     no.space=TRUE,
                     omit.stat=c("LL","ser","f"),
                     label = "regression2_logit")




# 进一步将被解释变量划分为高信任、中等水平信任、低水平信任（鲁棒性检验）
reg_df_vill_output <- as.data.frame(reg_df_vill)
stargazer::stargazer(reg_df_vill_output,out = "tex/tab_tex/statistics.tex",type = "latex",covariate.labels = covar_label_2,title = "变量统计性描述",label = "summary",digits = 2)


# 将变量转换为社会信任

reg_df_vill2 <- reg_df_vill%>%
  dplyr::filter(transfer>=0)

table(reg_df_vill2$transfer)


reg_vill2 <- lm(data=reg_df_vill2,formula = ins_trust~clan+transfer+clan*transfer)

summary(reg_vill2)
lm(data = reg_df_vill,formula = )


lm(data=reg_df_vill,formula = qn10021~clan)%>%
  summary()#对父母的信任，负（不显著）
lm(data=reg_df_vill,formula = qn10022~clan)%>%
  summary()#对邻居的信任，负（不显著）
lm(data=reg_df_vill,formula = qn10023~clan)%>%
  summary()# 对美国人的信任，正（显著）
lm(data=reg_df_vill,formula = qn10024~clan)%>%
  summary()#对陌生人的信任，正（弱显著）
lm(data=reg_df_vill,formula = qn10026~clan)%>%
  summary()# 对医生的信任，负向（显著）


reg_df_vill_mech <- reg_df_vill%>%
  dplyr::filter(qn701>=1)%>%
  mutate(pol_part=case_when(
    qn701==1~1,
    qn701==5~0
  ))
lm(data=reg_df_vill_mech,formula = ins_trust~clan*pol_part)%>%
  summary()

reg_df_mech <- reg_df_fam%>%
  #dplyr::filter(qg2>=0)%>%
  dplyr::filter(qg1>=0)%>%
  mutate(job=case_when(
    qg1==2~0,
    qg1==1~0,
    qg2<=3~2,
    qg2<=8&qg2>=3~1
  ))%>%
  drop_na(job)
lm(data = reg_df_mech,formula = ins_trust~job*clan)%>%
  summary()

table(reg_df_mech$job)

reg_df_mech$jobclass
