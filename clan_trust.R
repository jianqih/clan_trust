library(haven)
library(visdat)
library(tidyverse)

cfps2014comm <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014comm_201906.dta")
cfps2014comm_1 <- cfps2014comm%>%
  rename(clan=ca201_a_8)%>%
  dplyr::filter(clan!=-9)%>%
  dplyr::filter(clan!=-1)%>%
  dplyr::filter(clan!=-2)%>%
  dplyr::filter(clan!=-10)


table(cfps2014comm$ca201_a_8)
#table(cfps2014comm$clan)# 这里的-8实际上就是为没有
cfps2014comm_1$clan[which(cfps2014comm_1$clan==-8)]=0

ind_df <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014adult_201906.dta")

ind_df_fil <- ind_df%>%
  dplyr::filter(cid14!=-9)%>%
  drop_na(qn10025)%>%
  dplyr::filter(qn10025>=0)%>%
  rename(ins_trust=qn10025)

ind_df_join <- left_join(ind_df_fil,cfps2014comm_1,by="cid14")

ind_df_fil_1 <- ind_df_join%>%
  drop_na(clan)

# dim(ind_df_join)
ind_df_fil_2 <- ind_df_join%>%
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

ind_df_fil_2$p_income[which(ind_df_fil_2$p_income==0)]=1

famconf14 <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014famconf_170630.dta")
# 家庭层面的控制变量：家庭规模、家庭收入对数、土地是否被征用
famecon14 <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014famecon_201906.dta")
famecon14_mat <- famecon14%>%
  select(fincome1,houseasset_gross,fv3,fv4,fv5,fml2014num,fid14)
reg_df_fam <- left_join(ind_df_fil_2,famecon14_mat,by="fid14")
reg_df_fam_clean <- reg_df_fam%>%
  dplyr::filter(fincome1>=0)%>%
  dplyr::filter(houseasset_gross>=0)%>%
  dplyr::filter(fml2014num>=0)
# 村庄控制变量： 常住人口、少数民族集聚、村人均收入对数、到县城距离
reg_df_vill <- reg_df_fam_clean%>%
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
  select(ins_trust,clan,cfps2014_age,cfps_minzu,p_income,edu,cfps_gender,cfps_party,marry,fml2014num,fincome1,residual_vill,income_per_vill,dis_country,minority,transfer,qn10021,qn10022,qn10023,qn10024,qn10026,qn701,qg1)

reg_wocontrol <- lm(data = ind_df_fil_1,formula = ins_trust~clan)
reg_ind <- lm(data = ind_df_fil_2,formula = ins_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marry)
reg_fam <- lm(data = reg_df_fam_clean,formula = ins_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marry+fml2014num+log(fincome1))
reg_cou <- lm(data = reg_df_vill,formula = ins_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marry+fml2014num+log(fincome1)+minority+dis_country+log(income_per_vill)+residual_vill)

covar_label <-  c("宗族祠堂", "年龄","是否为少数民族", "个人对数收入", "受教育水平", "性别","是否党员","是否已婚","家庭规模","家庭对数收入","村庄是否为少数民族聚集区","村庄到县城距离","村庄人均对数收入","村庄常住人口数")
stargazer::stargazer(reg_wocontrol,reg_ind,reg_fam,reg_cou,
                     type = "latex",
                     out = "tex/tab_tex/benchmark.tex",
                     title = "宗族文化与制度信任回归结果",
                     column.sep.width="0pt",
                     dep.var.labels="官员信任水平",
                     covariate.labels =covar_label,
                     no.space=TRUE,
                     omit.stat=c("LL","ser","f"),
                     label = "regression1")



# 异质分析



df <- ind_df_fil_1[,c("clan","ins_trust")]
df%>%
  group_by(clan)%>%
  summarise(trust_mean=mean(ins_trust))->df_mean
ggplot(data = ind_df_fil_1)+
  geom_density(aes(clan))
ggplot(data = ind_df_fil_1)+
  geom_density(aes(ins_trust))+theme_bw()+xlab("制度信任")+ylab("密度分布")


ind_df_fil_1%>%
  dplyr::filter(clan>0)%>%
ggplot()+
  geom_boxplot(aes(clan,ins_trust))



ggplot(data = ind_df_fil_1)+
  geom_density(aes(ins_trust))


# robust
