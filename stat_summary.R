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
  rename(transfer=ck205)%>%
  dplyr::filter(cfps2012_marriage_update>0)%>%
  mutate(marriage=case_when(
    cfps2012_marriage_update>=2~1,
    cfps2012_marriage_update==1~0
  ))

ind_df_fil_2$p_income[which(ind_df_fil_2$p_income==0)]=1
  
famconf14 <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014famconf_170630.dta")
# 家庭层面的控制变量：家庭规模、家庭收入对数、土地是否被征用
famecon14 <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014famecon_201906.dta")
famecon14_mat <- famecon14%>%
  select(fincome1,houseasset_gross,fv3,fv4,fv5,fml2014num,fid14)%>%
  dplyr::filter(fml2014num>=0)
reg_df_fam <- left_join(ind_df_fil_2,famecon14_mat,by="fid14")
reg_df_fam_clean <- reg_df_fam%>%
  dplyr::filter(fincome1>=0)%>%
  dplyr::filter(houseasset_gross>=0)

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
  rename(income_per_vill=ch6)



ind_df_fil_1_2 <- ind_df%>%
  dplyr::filter(cid14!=-9)%>%
  dplyr::filter(qn1001>=0)%>%
  mutate(qn1001=case_when(
    qn1001==5~0,
    qn1001==1~1
  ))


library(psych)
df_stat1 <- as.data.frame(ind_df_fil_1_2[,"qn1001"])

d1 <- describe(df_stat1,skew = FALSE)

df_stat2 <- as.data.frame(ind_df_fil_1[,c("ins_trust","clan")])
d2 <- describe(df_stat2,skew = FALSE)

df_stat3 <- as.data.frame(ind_df_fil_2[,c("cfps2014_age","cfps_minzu","p_income","edu","cfps_gender","cfps_party","marriage")])
stargazer::stargazer(df_stat5)
df_stat4 <- as.data.frame(reg_df_fam_clean[,c("fml2014num","fincome1")])

d4 <- describe(df_stat4,skew = FALSE)
df_stat5 <- as.data.frame(reg_df_vill[,c("minority","dis_country","income_per_vill","residual_vill")])
d5 <- describe(df_stat5,skew = FALSE)
rd1 <- rbind(d2,d1)
rd2 <- rbind(d3,d4)
rd3 <- rbind(rd1,rd2)
rd4 <- rbind(rd3,d5)

rd4$vars <- 1:dim(rd4)[1]

labels=c("制度信任","人际信任","家庭祠堂", "年龄","少数民族", "个人年收入", "受教育水平", "性别","是否党员","是否已婚","家庭规模","家庭年收入","少数民族聚集区","村庄到县城距离","村庄人均年收入","村庄常住人口数")
  
kable(rd4,format = "latex")

res_kbl = kableExtra::kbl(merg, longtable=TRUE, booktabs = T, caption = "Table 1", digits = 2) %>%
  kableExtra::kable_styling(latex_options = c("striped", "HOLD_position"), font_size = 10) %>%
  kableExtra::add_header_above(c(" " = 1, "in Complete sample" = 2, "in Restricted sample" = 2, "MAJ" = 4, "MIN" = 4, "MAJ" = 4, "MIN" = 4)) %>%
  kableExtra::add_header_above(c(" " = 1, "No. of funds" = 4, "Fund Size Complete sample (USD million nominal)" = 8, "Fund Size Restricted sample (USD million nominal)" = 8)) %>%
  kableExtra::add_header_above(c(" " = 5, "Complete sample" = 8, "Restricted sample" = 8)) %>%
  kableExtra::row_spec(row = nrow(merg) - 1, underline = T, extra_css = "border-bottom: 2px solid;")

library(knitr)

library(modelsummary)
g1 <- stargazer::stargazer(df_stat1,type = "latex")
stargazer::stargazer(df_stat4,type = "text")