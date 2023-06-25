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


ind_df <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014adult_201906.dta")
dim(ind_df)

ind_df_fil <- ind_df%>%
  dplyr::filter(cid14!=-9)

#dim(ind_df_fil)
famconf14 <- read_dta("/Volumes/Expansion/micro-base-data/cfps/2014/cfps2014famconf_170630.dta")

#ind_df_join_fam <- left_join(ind_df_fil,faconf,by="pid")
#dim(ind_df_join_fam)
#ind_df_join_fam$cid14
cfps2014comm$cid14
cfps2014comm$clan
ind_df_join_fam_comm <- left_join(ind_df,cfps2014comm,by="cid14")
#dim(ind_df_join_fam_comm)
ind_df_join_fam_comm$clan
# 官员信任～clan
# 参保～clan

ind_df_join_fam_comm%>%
  select(qn1001,qn10021,qn10022,qn10023,qn10024,qn10025,qn10026)

ind_df_join_fam_comm$hukou
df_reg <- ind_df_join_fam_comm%>%
  dplyr::filter(urban14.x==0)%>% #乡村
  dplyr::filter(qi301_s_1>=1)%>%
  mutate(age=2014-birth)%>%
  mutate(ins=case_when(
    qi301_s_1==78~0,
    qi301_s_1<=77~1))


#dim(df_reg)

# 个体层面的控制变量 数据未清洗
reg1 <- lm(data = df_reg,formula = ins~clan+te4+p_income+cfps_gender+cfps2014_age+qea0+qp201+cfps_party+cfps_minzu)
summary(reg1)


df_reg$clan
df_reg$qea0
#df_reg$ku2

ind_df_join_fam_comm$qi301_s_1
table(df_reg$ins)
