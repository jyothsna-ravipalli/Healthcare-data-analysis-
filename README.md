 # TUBERCULOSIS (TB) BURDEN ANALYSIS BY AGE AND GENDER

 # A COMPREHENSIVE DATA ANALYTIC APPROACH USING THE SEER 1992-2020 DATASET

# SAS CODE FOR THE PROJECT 
/* Import the CSV file */
proc import datafile='C:\Users\jyo\desktop\TB_burden_age_sex_2023-10-28.csv'
    out=TB_burden
    dbms=csv
    replace;
    getnames=yes;
run;

/* Check for missing data */
proc freq data=TB_burden;
  tables _all_ / missing;
run;

proc freq data =TB_burden_clean;
  tables age_group*risk_factor / chisq cmh;
  where age_group in ('0-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65+');
run;

/* Impute missing data or remove missing data - Example of removing missing data */
data TB_burden_clean;
  set TB_burden;
  if cmiss(of _all_) = 0;
run;

/* Descriptive Statistics and Visualization for Continuous Variable 'best' */
proc means data=TB_burden_clean n mean std min q1 median q3 max;
  var best;
run;

 /* Bar Chart for Age Groups vs Risk Factors */  
/* This bar chart shows the distribution of different risk factors across age groups. */

Proc sgplot data=TB_burden_clean;
  vbar age_group / group=risk_factor response=best stat=mean;
  title "Distribution of Risk Factors Across Age Groups";
run;

/* Heatmap for Age Groups and Risk Factors:
the heatmap can be useful to visualize the intensity of TB burden across different age groups and risk factors. */


Proc sgplot data=TB_burden_clean;
  heatmap x=age_group y=risk_factor / colorresponse=best colormodel=(blue yellow red);
  title "Heatmap of TB Burden by Age Group and Risk Factor";
run;


proc univariate data=TB_burden_clean;
  var best;
  histogram best / normalcurve;
  inset n mean std / pos=ne;
run;

/* Descriptive Statistics and Visualization for Categorical Variable 'age_group' */
proc freq data=TB_burden_clean;
  tables age_group / nocum nopercent missing;
run;

/* PROC SGPLOT procedure to create a vertical bar chart */
proc sgplot data=TB_burden_clean;
  vbar age_group / response=best stat=mean;
run;

/* Descriptive Statistics for 'best' Grouped by Categorical Variable 'age_group' */
proc means data=TB_burden_clean n mean std min q1 median q3 max;
  class age_group;
  var best;
run;

/* Statistical graphics */
proc sgplot data=TB_burden_clean;
  vbox best / category=age_group;
run;
/* Test of Normality for 'best' */
proc univariate data=TB_burden_clean;
  var best;
  histogram best / normalcurve;
  qqplot best / normal(mu=est sigma=est);
run;

/* One-Sample T-Test for 'best' */
proc ttest data=TB_burden_clean h0=10000;
  var best;
run;

/* Chi-Square Test for Independence with Odds Ratio and Relative Risk */
proc freq data=TB_burden_clean;
  tables sex*risk_factor / chisq relrisk;
run;

/* ANOVA for 'best' by 'sex' */
proc glm data=TB_burden_clean;
  class sex;
  model best = sex;
  means sex / tukey; /* Tukey's honestly significant difference test */
run;

/* Scatter Plot and Correlation Test for 'best' and 'lo' */
proc sgscatter data=TB_burden_clean;
  plot best*lo;
run;

/* correlation coefficients between variables in a dataset*/
proc corr data=TB_burden_clean;
  var best lo;
run;

/* Simple Linear Regression with Diagnostics */
proc reg data=TB_burden_clean;
  model best=lo;
  plot r.*p. rstudent.*p. / cooksd;
run; 








/* Multiple Regression Analysis with Categorical Predictors */
proc glmselect data=TB_burden_clean plots=all;
  class age_group sex risk_factor; /* Declare categorical variables */
  model best=lo hi age_group sex risk_factor / selection=stepwise; /* Stepwise selection */
  output out=reg_output p=predicted r=residuals; /* Output dataset with predicted values and residuals */
run;


/* Plot Residuals */
proc sgplot data=reg_output;
  scatter x=predicted y=residuals;
  refline 0 / axis=y;
run;


/* Check the levels of the 'sex' variable */
proc freq data=TB_burden_clean;
  tables sex;
run;


/* Create a binary outcome variable for logistic regression */
data TB_burden_clean;
  set TB_burden_clean;
  if best > 10000 then outcome=1;
  else outcome=0;
run;

/* Logistic Regression Analysis with Model Fit Statistics */
proc logistic data=TB_burden_clean desc;
  class sex(ref='a') age_group(ref='0-14') risk_factor(ref='smk') / param=ref;
  model outcome(event='1') = lo hi age_group sex risk_factor / selection=stepwise lackfit;
  output out=predicted_values p=probabilities;
run;


/* ROC Curve */
proc logistic data=TB_burden_clean plots(only)=roc;
  class age_group(ref='0-14') sex(ref='a') risk_factor(ref='smk') / param=ref;
  model outcome(event='1') = lo hi age_group sex risk_factor;
run;


/* Independent t-test for 'best' between two age groups */
proc ttest data=TB_burden_clean;
  class age_group;
  var best;
  where age_group in ('0-14', '15-24'); /* For 2 selected age groups */
run;

