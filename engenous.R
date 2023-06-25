library(plm)
reg_wocontrol_fix <- ind_df_fil_1%>%
  dplyr::filter(provcd14.x>=10)%>%
  lm(formula = ins_trust~clan+factor(provcd14.x)-1)
reg_ind_fix <- ind_df_fil_2%>%
  dplyr::filter(provcd14.x>=10)%>%
  lm(formula = ins_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marriage+factor(provcd14.x)-1)
reg_fam_fix <- reg_df_fam_clean %>%
  dplyr::filter(provcd14.x>=10)%>%
  lm(formula = ins_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marriage+fml2014num+log(fincome1)+factor(provcd14.x)-1)
reg_cou_fix <- reg_df_vill%>%
  dplyr::filter(provcd14.x>=10)%>%
  dplyr::filter(clan>0)%>%
  lm(formula = ins_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marriage+fml2014num+log(fincome1)+minority+dis_country+log(income_per_vill)+residual_vill+factor(provcd14.x)-1)

stargazer::stargazer(reg_wocontrol_fix,reg_ind_fix,reg_fam_fix,reg_cou_fix,
                     type = "latex",
                     out = "tex/tab_tex/benchmark_fix.tex",
                     dep.var.caption="被解释变量：制度信任",
                     dep.var.labels.include = FALSE,
                     title = "宗族文化与制度信任固定效应回归",
                     column.sep.width="0pt",
                     covariate.labels =covar_label,
                     no.space=TRUE,
                     omit = "provcd14.x",
                     add.lines=list(c('省级固定效应', '是','是','是','是')),
                     keep.stat = "n",
                     label = "regression_fix")
# 异质分析


reg_agri <- reg_df_vill%>%
  dplyr::filter(qa301==1)%>%
  lm(formula = ins_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marriage+fml2014num+log(fincome1)+minority+dis_country+log(income_per_vill)+residual_vill)

reg_nonagri <- reg_df_vill%>%
  dplyr::filter(qa301==3)%>%
  lm(formula = ins_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marriage+fml2014num+log(fincome1)+minority+dis_country+log(income_per_vill)+residual_vill)

stargazer::stargazer(reg_agri, reg_nonagri,
                     type="latex",keep = c("clan"),
                     dep.var.caption="被解释变量：制度信任",
                     dep.var.labels.include = FALSE,
                     column.labels = c("农业户口","非农户口"),
                     column.sep.width="0pt",
                     no.space=TRUE,
                     covariate.labels = c("家族祠堂"),
                     omit.stat=c("LL","ser","f"),
                     title="农业户口与非农户口回归", single.row=TRUE,
                     label = "hetero_hukou",
                     out = "tex/tab_tex/hetero_hukou.tex",
                     keep.stat = "n",
                     notes = "经验p值为0，可认为两组间具有显著差异"
                     )

SSR_pooled = sum(resid(reg_cou)^2)
SSR_agri = sum(resid(reg_agri)^2)
SSR_nonagri = sum(resid(reg_nonagri)^2)

F = SSR_pooled-(SSR_agri+SSR_nonagri)*(17073-2*17)/((SSR_agri+SSR_nonagri)*16)

F

1-pf(F,16,17073-2*17)
#summary(reg_nonagri)
## plot
df <- ind_df_fil_1[,c("clan","ins_trust")]
df%>%
  group_by(clan)%>%
  summarise(trust_mean=mean(ins_trust))->df_mean
ggplot(data = df_mean,aes(clan,trust_mean))+
  geom_point()+geom_smooth(method = "lm")+xlab("家族祠堂")+ylab("制度信任均值")



# robust



reg1 <- ind_df_fil_1%>%
  #dplyr::filter(provcd14.x>=10)%>%
  dplyr::filter(clan>0)%>%
  lm(formula = ins_trust~clan)
#summary(reg_wocontrol_fix_trun)
reg_ind_fix_trun <- ind_df_fil_2%>%
  #dplyr::filter(provcd14.x>=10)%>%
  dplyr::filter(clan>0)%>%
  lm(formula = ins_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marriage)
#summary(reg_ind_fix_trun)
reg_fam_fix_trun <- reg_df_fam_clean %>%
  # dplyr::filter(provcd14.x>=10)%>%
  dplyr::filter(clan>0)%>%
  lm(formula = ins_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marriage+fml2014num+log(fincome1))
summary(reg_fam_fix_trun)
reg_cou_fix_trun <- reg_df_vill%>%
  #dplyr::filter(provcd14.x>=10)%>%
  dplyr::filter(clan>0)%>%
  lm(formula = ins_trust~clan+cfps2014_age+cfps_minzu+log(p_income)+edu+cfps_gender+cfps_party+marriage+fml2014num+log(fincome1)+minority+dis_country+log(income_per_vill)+residual_vill)

summary(reg_cou_fix_trun)

stargazer::stargazer(reg1,reg_ind_fix_trun,reg_fam_fix_trun,reg_cou_fix_trun,
                     type = "latex",
                     out = "tex/tab_tex/benchmark_fix_trun.tex",
                     dep.var.caption="被解释变量：制度信任",
                     dep.var.labels.include = FALSE,
                     title = "宗族文化与制度信任回归子样本",
                     column.sep.width="0pt",
                     #covariate.labels =covar_label,
                     no.space=TRUE,
                     keep.stat = "n",
                     label = "regression_trun")
stargazer::stargazer(reg_ind_fix_trun,reg_fam_fix_trun,reg_cou_fix_trun)
