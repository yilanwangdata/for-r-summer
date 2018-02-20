# for-r-summer--using multinomial logistic model
gss = read.csv(file.choose()) ## choose GSS.2006.csv

#install.packages("mlogit")
library(mlogit)


table(gss$natchld)

gss$rnatchld = 4-gss$natchld
table(gss$rnatchld)
gss2 = mlogit.data(gss, varying=NULL, choice="rnatchld", shape="wide")
table(gss2$rnatchld)
head(index(gss2))
head(gss2$rnatchld)
#c<-('rnatchld', 'chid', 'alt')


#summary(gss)
#summary(gss2)
## testing the grey peril hypothesis ##
ml1 = mlogit(rnatchld ~ 1 | age + educ + sex + prestg80 + as.factor(region), data=gss2, reflevel="2")
summary(ml1)

test = ((0.01872522-0.00445479)^2)/(0.00472158^2 + 0.00259763^2)
test
pchisq(test, df = 1, lower.tail = FALSE)
exp(coef(ml1))
## add in children ##
ml2 = mlogit(rnatchld ~ 1 | age + childs + educ + sex  + prestg80 + as.factor(region), data=gss2, reflevel="2")
summary(ml2)

test2 = (( 0.01692479-0.00632173)^2)/(0.00507468^2 + 0.00281551^2)
test2
pchisq(test2, df = 1, lower.tail = FALSE)

## interact children with age ##
ml3 = mlogit(rnatchld ~ 1 | age*childs + educ + sex  + prestg80 + as.factor(region), data=gss2, reflevel="2")
summary(ml2)

#install.packages("nnet")
library(nnet)

gss$ncok <- relevel(as.factor(gss$rnatchld), ref = 2)
head(gss$ncok)

summary(gss$ncok)
## same as above ##
mult1 = multinom(ncok ~ age + educ + sex + prestg80 + as.factor(region), data=gss)
summary(mult1)

z1 <- summary(mult1)$coefficients/summary(mult1)$standard.errors
z1

options(scipen=999) ## if you want to revert back, use options(scipen=0) 
p1 <- (1 - pnorm(abs(z1), 0, 1))*2
p1

pred1 <- predict(mult1, type = "probs")
head(pred1) ## overall probabilities

#data frame of values to use for predictions
data.child <- expand.grid(
  age = 20:95, # let age vary from 20 to 95 
  region = 3,  # set region to east north central 
  sex = 1, # fix sex as male
  prestg80 = mean(gss$prestg80,na.rm=T), # fix realinc at its mean
  educ = mean(gss$prestg80,na.rm=T)) # fix realinc at its mean


#combine age and predicted probabilities in a data frame
preds.child <- data.frame( 
  age = data.child$age, # polviews
  predict(mult1, newdata = data.child, type = "probs", se = TRUE)) # predicted probabilities

#install.packages("plyr")
library(plyr)

#avg predicted probabilities for each level of age
ddply(preds.child, "age", colMeans) 

table(gss$rnatchld) ## sanity check, small-medium-large in precentages

# for-r-summer--using poisson
##install.packages("data.table")
library(data.table)

vars <- c("memnum", "educ", "year", "sex", "relig", "age")

d <- data.table::fread(
  file.choose(), ## pick GSS-Cum.zip on Coursework under Data, unzip it and use the .csv##
  sep = ",",
  select = vars,
  data.table = FALSE)

head(d)

d$norelig = ifelse(d$relig==4,1,0)
d$seventies = ifelse(d$year<1980,1,0)
d$eighties = ifelse(d$year>1979 & d$year<1990,1,0)
d$nineties = ifelse(d$year>1989 & d$year<2000,1,0)
d$aughts = ifelse(d$year>=2000,1,0)
d$female = ifelse(d$sex==2,1,0)


ols1 = lm(memnum ~ female + eighties + nineties + aughts + age + educ + norelig, d)
summary(ols1)


gls1 = glm(memnum ~ female + eighties + nineties + aughts + age + educ + norelig, d, family="gaussian")
summary(gls1)

p1 = glm(memnum ~ female + eighties + nineties + aughts + age + educ + norelig, d, family="poisson")
summary(p1)

##install.packages("MASS")
library(MASS)

nb1 = glm.nb(memnum ~ female + eighties + nineties + aughts + age + educ + norelig, d)
summary(nb1)

nb2 = glm.nb(memnum ~ female + eighties + nineties + aughts + educ + norelig + offset(log(age)), d)
summary(nb2) 

##install.packages("pscl")
library(pscl)

znb1 <- zeroinfl(memnum ~ female + eighties + nineties + aughts + educ + norelig | age,
                 data = d, dist = "negbin", EM = TRUE)
summary(znb1)

install.packages("AICcmodavg")
library(AICcmodavg)
AICc(znb1, return.K = FALSE, second.ord = FALSE)

# r- some text analysis
##install.packages("installr"); library(installr) #load / install+load installr
##updateR() # updating R. ## code for installr


##install.packages("tidyverse")
##install.packages("tidytext" )
##install.packages("stringr")

library(tidyverse)
library(tidytext)
library(stringr)

##get Obama SOUs from 2013-2016 ##

paper_words <- data_frame(file = paste0("C:\\Users\\gme2101\\Documents\\Spring 2016\\sous\\", 
                                        c("2016-Obama.txt", "2015-Obama.txt",
                                          "2014-Obama.txt", "2013-Obama.txt"))) %>%
  mutate(text = map(file, read_lines)) %>%
  unnest() %>%
  group_by(file = str_sub(basename(file), 1, -5)) %>%
  mutate(line_number = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) 


##data_frame(txt = prideprejudice)
##unnest_tokens(word, txt) %>%
##mutate(word = wordStem(word))

head(paper_words)

##remove stop-words

data("stop_words")
paper_words <- paper_words %>%
  anti_join(stop_words)

##perform sentiment analysis on speeches ##

paper_sentiment <- inner_join(paper_words, get_sentiments("bing")) %>%
  count(file, index = round(line_number / max(line_number) * 100 / 5) * 5, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(net_sentiment = positive - negative)

##display sentiment across time ##
paper_sentiment %>% ggplot(aes(x = index, y = net_sentiment, fill = file)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  facet_wrap(~ file) + 
  scale_x_continuous("Location in paper (percent)") + 
  scale_y_continuous("Bing Net Sentiment")

##install.packages("tm")
library(tm)


##count each word per speech 

pw = paper_words[,c("file","word")]
  
d=  count_(pw, c("file", "word"))

##make a document term matrix ##

pwdtm = d %>%
  cast_dtm(file, word, n)

##make the dtm into a dataframe ##
mpwdtm=as.matrix(pwdtm)
df.mpwdtm=as.data.frame(mpwdtm)

##make the dtm into a tdm instead ##

t.t = t(mpwdtm)

cor(t.t)

# r-something I did using ggplot

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE,
                      message = FALSE)
```

```{r}
#install.packages("Sleuth3")
library(Sleuth3)
library(tidyverse)

# convert Educ from an integer to a factor, and make "<12" the first factor level 
mydata <- ex0525 %>% 
  dplyr::mutate(Educ = forcats::fct_relevel(Educ, "<12"))

ggplot(mydata, aes(Educ, Income2005)) +
  geom_boxplot() + 
  coord_flip()   # for horizontal boxplots
```

```{r}
ggplot(mydata, aes(Income2005)) +
  geom_histogram(bins=100)
```



```{r}
ggplot(mydata, aes(Income2005)) +
  geom_histogram(bins=50)+facet_wrap(~Educ)
```
```{r}
ggplot(ex0824, aes(Age)) +
  geom_histogram(closed = "right")

ggplot(ex0824, aes(Age)) +
  geom_histogram(closed = "left")


ggplot(ex0824, aes(Rate)) +
  geom_histogram(closed = "right")

ggplot(ex0824, aes(Rate)) +
  geom_histogram(closed = "left")

```

```{r}
ggplot(ex0824, aes(Rate)) +
  geom_histogram(closed = "right",binwidth = 3)

ggplot(ex0824, aes(Rate)) +
  geom_histogram(closed = "left", binwidth = 3)
```
```{r}
library(ggplot2movies)
ggplot(movies, aes(budget)) +
  geom_histogram(bins=200)
bd1=dplyr::filter(movies, budget<=10000000)
bd2=dplyr::filter(movies, budget>10000000 & budget<100000000)
bd3=dplyr::filter(movies, budget>=100000000)
ggplot(bd1, aes(budget)) +
  geom_histogram(bins=100)
ggplot(bd2, aes(budget)) +
  geom_histogram(bins=100)
ggplot(bd3, aes(budget)) +
  geom_histogram(bins=100)
```

```{r}
ggplot(case0201, aes(Depth)) +
  geom_histogram(aes(y=..density..))+geom_density()+facet_wrap(~Year)
```

```{r}
ggplot() +geom_density(aes(x=(case0201%>%dplyr::filter(Year==1978))$Depth), color="blue",fill="blue", alpha=0.3) +  geom_density(aes(x=(case0201%>%dplyr::filter(Year==1976))$Depth), color="yellow",fill="yellow", alpha=0.3)+ xlab("Depth: yellow=1976, blue=1978")
```

```{r}
qqnorm(case0102$Salary)
qqline(case0102$Salary, col='red')
library(StatDA)
ppplot.das(case0102$Salary)

library(gridExtra)
p1=ggplot(case0102, aes(x=Salary, y=..density..)) +
  geom_histogram()+geom_density(col='red')+facet_wrap(~Sex)
p2=ggplot(case0102, aes(x=Salary, y=..density..)) +
  geom_histogram()+geom_density(col='red')
x <- rnorm(1000)
p3=ggplot(NULL, aes(x=x, y=..density..)) + geom_density(col='red')+xlab("normal")
grid.arrange(p1, p2, p3, nrow = 2)
```

```{r}
#install.packages('DAAG')
library(DAAG)
library(tidyverse)
vlt
library(ggplot2)
library(purrr)
q1=gather(vlt,window,symbol,1:3)
q11=q1%>%group_by(window,symbol)%>%summarise(count=n())%>%group_by(window)%>%mutate(freq=count/sum(count))
ggplot(q11,aes(symbol,freq))+geom_bar(stat='identity')+facet_wrap(~window)
```



```{r}
library(dplyr)
library(tidyverse)
#summary(death2015)
#summary(death2015$Place_of_Death)
#install.packages("readxl")
library(readxl)
death2015 <- read_excel("E:/data visulization/death2015.xlsx")
count_place<-death2015%>%group_by(Place_of_Death)%>%summarize(count1=n())
ggplot(count_place,aes(reorder(Place_of_Death,count1),count1))+geom_col()+coord_flip()+theme_grey(18)
```
```{r}
#library(dplyr)
#summary(death2015)
#summary(death2015$Place_of_Death)
count_ten<-death2015%>%group_by(Ten_Year_Age_Groups)%>%summarize(count2=n())
ggplot(count_ten,aes(Ten_Year_Age_Groups,count2))+geom_col()+coord_flip()+theme_grey(18)
```
```{r}
count_icd<-death2015%>%group_by(ICD_Chapter_Code)%>%summarize(count3=n())
ggplot(count_icd,aes(reorder(ICD_Chapter_Code,count3),count3))+geom_col()+coord_flip()+theme_grey(18)
```

```{r}
sum_icd<-death2015%>%filter(!is.na(ICD_Chapter_Code))%>%group_by(ICD_Chapter_Code,ICD_Sub_Chapter_Code)
ggplot(sum_icd,aes(ICD_Sub_Chapter_Code,Deaths))+geom_col()+coord_flip()+facet_wrap(~ICD_Chapter_Code,scales = 'free')
```

```{r}       
ggplot(death2015,aes(ICD_Sub_Chapter_Code)) + geom_histogram(stat="count") + coord_flip() +facet_wrap(~ICD_Chapter_Code, scales = "free")
``` 



```{r}
sum_icd2<-death2015%>%filter(!is.na(ICD_Chapter_Code))%>%group_by(ICD_Chapter_Code,ICD_Sub_Chapter_Code)
ggplot(sum_icd,aes(ICD_Sub_Chapter_Code,Deaths))+geom_col()+coord_flip()+facet_wrap(~ICD_Chapter_Code,scales = 'free_y')
```
```{r}
sub_chap_new<-death2015%>%filter(!is.na(ICD_Chapter_Code))
ggplot(sub_chap_new,aes(ICD_Sub_Chapter_Code)) + geom_histogram(stat="count") + coord_flip() +facet_wrap(~ICD_Chapter_Code, scales = "free")
```

```{r}
ques2_d=death2015 %>%filter(!is.na(ICD_Chapter_Code))%>% group_by(ICD_Chapter_Code,ICD_Sub_Chapter_Code)%>%summarize(a=n())%>%group_by(ICD_Chapter_Code)%>%mutate(b=a/sum(a))
ggplot(ques2_d,aes(ICD_Sub_Chapter_Code,b))+geom_bar(stat = "identity") + coord_flip() + facet_wrap(~ICD_Chapter_Code, scales = "free_y", ncol = 5)
```


```{r}
ques2_e=death2015 %>% filter(ICD_Chapter=="Diseases of the ear and mastoid process")%>% group_by(ICD_Chapter,ICD_Sub_Chapter)%>%summarize(a=n())%>%group_by(ICD_Chapter)%>%mutate(b=a/sum(a))
ggplot(ques2_e)+geom_bar(aes(reorder(ICD_Sub_Chapter,b),b),stat = "identity") + coord_flip()
```

```{r}

library(ggplot2movies)
library(dplyr)
head(movies)
ggplot(movies,aes(length))+geom_histogram()+xlim(c(300,500))
movies_cut<-
  filter(movies, length<300)

```
```{r}
ggplot(movies_cut,aes(year,length))+geom_point(alpha=.1)
```
```{r}
ggplot(movies_cut,aes(year,length))+geom_point(alpha=.1)+geom_density2d()+theme_grey(18)
```
```{r}
#nstall.packages('hexbin')
library(hexbin)
ggplot(movies_cut,aes(year,length))+geom_hex(binwidth=c(3,3))
```

```{r}
ggplot(movies_cut,aes(year,length))+geom_bin2d(binwidth=c(3,3))
```

```{r}
#install.packages('Sleuth2')
#install.packages('lattice')
library(lattice)
library(DAAG)
splom(leafshape[,1:3])

```
```{r}
splom(leafshape[,5:7])
```
```{r}
splom(leafshape[,1:3], col=factor(leafshape$arch))
```
```{r}
splom(leafshape[,5:7], col=factor(leafshape$arch))
```
