---
# "Categoricals"
---


```R
library(mlbench)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MultinomialCI)
```


```R
data(PimaIndiansDiabetes2)
df <- PimaIndiansDiabetes2
```

## Categoricals

### Binomial


```R
summary(df$mass)
# BMI: let's assume normal as <25 cut()
df$bmi <- as.factor(ifelse(df$mass < 25, "Normal", "Abnormal"))
summary(df$bmi)
```


       Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
      18.20   27.50   32.30   32.46   36.60   67.10      11 



<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>Abnormal</dt><dd>651</dd><dt>Normal</dt><dd>106</dd><dt>NA's</dt><dd>11</dd></dl>




```R
vec_cat <- df$bmi
summary(vec_cat)
table(vec_cat)
```


<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>Abnormal</dt><dd>651</dd><dt>Normal</dt><dd>106</dd><dt>NA's</dt><dd>11</dd></dl>




    vec_cat
    Abnormal   Normal 
         651      106 



```R
length(vec_cat)  # aware of NA!
sum(table(vec_cat))
```


768



757






```R
x <- 106
n <- 757
x/n * 100
```


14.002642007926



```R
# Exact Binomial Test
binom.test(x, n, conf.level = 0.9)
binom.test(x, n)
```


    
    	Exact binomial test
    
    data:  x and n
    number of successes = 106, number of trials = 757, p-value < 2.2e-16
    alternative hypothesis: true probability of success is not equal to 0.5
    90 percent confidence interval:
     0.1196874 0.1624759
    sample estimates:
    probability of success 
                 0.1400264 
    



    
    	Exact binomial test
    
    data:  x and n
    number of successes = 106, number of trials = 757, p-value < 2.2e-16
    alternative hypothesis: true probability of success is not equal to 0.5
    95 percent confidence interval:
     0.1160869 0.1668092
    sample estimates:
    probability of success 
                 0.1400264 
    


#### TASK #3.1 Categorical Output 1
- Create functions for categorical output for binomial factor


```R
freq_output <- function(x, n) paste(x, "/", n, "(", round((x/n) * 100), "%)")

ci_cat_output <- function(x, n) paste("95% CI: [", round(as.vector(binom.test(x, n)$conf.int) * 
    100, 2)[1], "%,", round(as.vector(binom.test(x, n)$conf.int) * 100, 2)[2], "%]")
```


```R
freq_output(x, n)
ci_cat_output(x, n)

freq_output(n - x, n)
ci_cat_output(n - x, n)
```


'106 / 757 ( 14 %)'



'95% CI: [ 11.61 %, 16.68 %]'



'651 / 757 ( 86 %)'



'95% CI: [ 83.32 %, 88.39 %]'


### Multinomial


```R
# BMI: let's assume normal as <25, non-critical abnormal < 35, else - critical
df$bmi_multi <- as.factor(ifelse(df$mass < 25, "Normal", ifelse(df$mass < 35, "NoSig Abnorm", 
    "Sig Abnorm")))

t(as.data.frame(summary(df$bmi_multi)))
```


<table class="dataframe">
<caption>A matrix: 1 × 4 of type int</caption>
<thead>
	<tr><th></th><th scope=col>Normal</th><th scope=col>NoSig Abnorm</th><th scope=col>Sig Abnorm</th><th scope=col>NA's</th></tr>
</thead>
<tbody>
	<tr><th scope=row>summary(df$bmi_multi)</th><td>106</td><td>403</td><td>248</td><td>11</td></tr>
</tbody>
</table>




```R
vec_cat_m <- df$bmi_multi

tt <- table(vec_cat_m)
tt_p <- tt/sum(tt)
round(tt_p * 100, 2)
```


    vec_cat_m
          Normal NoSig Abnorm   Sig Abnorm 
           14.00        53.24        32.76 



```R
# Sison-Graz Method
tt_p
multinomialCI(tt, alpha = 0.05)
```


    vec_cat_m
          Normal NoSig Abnorm   Sig Abnorm 
       0.1400264    0.5323646    0.3276090 



<table class="dataframe">
<caption>A matrix: 3 × 2 of type dbl</caption>
<tbody>
	<tr><td>0.1030383</td><td>0.1778334</td></tr>
	<tr><td>0.4953765</td><td>0.5701715</td></tr>
	<tr><td>0.2906209</td><td>0.3654159</td></tr>
</tbody>
</table>



#### TASK #3.2 Categorical Output 2
- Create function for categorical output for multinomial factor


```R
ci_multicat_output = function(ft, k) paste("[", round(as.vector(multinomialCI(tt, alpha = 0.05)[k, 
    ]) * 100, 2)[1], "%,", round(as.vector(multinomialCI(tt, alpha = 0.05)[k, ]) * 
    100, 2)[2], "%]")
```


```R
ci_multicat_output(ft = tt, k = 1)
ci_multicat_output(ft = tt, k = 2)
ci_multicat_output(ft = tt, k = 3)
```


'[ 10.3 %, 17.78 %]'



'[ 49.54 %, 57.02 %]'



'[ 29.06 %, 36.54 %]'


### Contingency Table = Cross table


```R
table(df$bmi, df$diabetes)
table(df$bmi_multi, df$diabetes)
```


              
               neg pos
      Abnormal 392 259
      Normal    99   7



                  
                   neg pos
      Normal        99   7
      NoSig Abnorm 262 141
      Sig Abnorm   130 118



```R
# Pearson's Chi-squared Test
chisq.test(df$bmi, df$diabete)
chisq.test(table(df$bmi, df$diabetes))

chisq.test(df$bmi_multi, df$diabetes)
```


    
    	Pearson's Chi-squared test with Yates' continuity correction
    
    data:  df$bmi and df$diabete
    X-squared = 42.592, df = 1, p-value = 6.745e-11
    



    
    	Pearson's Chi-squared test with Yates' continuity correction
    
    data:  table(df$bmi, df$diabetes)
    X-squared = 42.592, df = 1, p-value = 6.745e-11
    



    
    	Pearson's Chi-squared test
    
    data:  df$bmi_multi and df$diabetes
    X-squared = 54.718, df = 2, p-value = 1.313e-12
    



```R
# Fisher's Exact Test - for 2x2 cross tabs (but not only) - when for some cell
# count < 10 or <5 (you can meet different in literature)

fisher.test(df$bmi, df$diabete)

fisher.test(df$bmi_multi, df$diabete)
fisher.test(table(df$bmi_multi, df$diabetes))
```


    
    	Fisher's Exact Test for Count Data
    
    data:  df$bmi and df$diabete
    p-value = 3.322e-13
    alternative hypothesis: true odds ratio is not equal to 1
    95 percent confidence interval:
     0.04137552 0.23436448
    sample estimates:
    odds ratio 
     0.1072507 
    



    
    	Fisher's Exact Test for Count Data
    
    data:  df$bmi_multi and df$diabete
    p-value = 1.269e-14
    alternative hypothesis: two.sided
    



    
    	Fisher's Exact Test for Count Data
    
    data:  table(df$bmi_multi, df$diabetes)
    p-value = 1.269e-14
    alternative hypothesis: two.sided
    


#### TASK #3.3 Categorical Output Fully
- Study the example
- Perform categorical stats output both(!) for bmi and bmi_multi within the same report


```R
measure_order_cat <- c("N", "Missing",
                       "NormN", "NormF", "NormCi",
                       "AbnormN", "AbnormF", "AbnormCi")
df %>% select(bmi, diabetes) %>% 
    gather(key = 'Parameter', value = 'Value', -diabetes) %>% 
    group_by(diabetes, Parameter) %>% 
    summarise(N = n() - sum(is.na(Value)),
              Missing = sum(is.na(Value)),
              NormN = sum(Value == 'Normal', na.rm = T),
              NormF = freq_output(NormN, N),
              NormCi = ci_cat_output(NormN, N),
              AbnormN = sum(Value == 'Abnormal', na.rm = T),
              AbnormF = freq_output(AbnormN, N),
              AbnormCi = ci_cat_output(AbnormN, N)
    ) %>% ungroup() %>% 
    gather(key = 'Measure', value = 'Value', -c(diabetes, Parameter)) %>% 
    spread(key = diabetes, value = Value) %>% 
    arrange(match(Measure, measure_order_cat)) %>% 
    knitr::kable()
```

    `summarise()` has grouped output by 'diabetes'. You can override using the `.groups` argument.
    


    
    
    |Parameter |Measure  |neg                        |pos                         |
    |:---------|:--------|:--------------------------|:---------------------------|
    |bmi       |N        |491                        |266                         |
    |bmi       |Missing  |9                          |2                           |
    |bmi       |NormN    |99                         |7                           |
    |bmi       |NormF    |99 / 491 ( 20 %)           |7 / 266 ( 3 %)              |
    |bmi       |NormCi   |95% CI: [ 16.7 %, 23.99 %] |95% CI: [ 1.06 %, 5.35 %]   |
    |bmi       |AbnormN  |392                        |259                         |
    |bmi       |AbnormF  |392 / 491 ( 80 %)          |259 / 266 ( 97 %)           |
    |bmi       |AbnormCi |95% CI: [ 76.01 %, 83.3 %] |95% CI: [ 94.65 %, 98.94 %] |








