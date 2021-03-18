library("AmesHousing")
library("tidyverse")
library("rmarkdown")
library("ggplot2")

AmesHousing::ames_raw
ames_raw %>% dim()
ggplot(ames_raw, aes(x = SalePrice)) + 
  geom_histogram(bins = 400)

Table_SP<-ames_raw %>% select(Neighborhood,SalePrice, `Overall Qual`) %>%  group_by(Neighborhood) %>% 
  mutate(SalePrice_mean = mean(SalePrice, na.rm = TRUE)) %>% 
  filter(SalePrice_mean>=200000) %>% arrange(desc(SalePrice_mean)) 

Table_SP %>% group_by(Neighborhood) %>% 
  summarize(SalePrice_mean = mean(SalePrice)) %>% 
  filter(SalePrice_mean>=200000) %>% arrange(desc(SalePrice_mean))

ggplot(Table_SP, aes(x = Neighborhood, y = SalePrice, fill=Neighborhood)) + geom_boxplot(alpha=0.5,show.legend = TRUE) +
  theme(legend.position="none") + stat_summary(geom='point',fun=mean,shape=23,size=3)

# NorthRidge 동네가 평균 판매가격이 가장 높다. 
# 평균 판매가격이 2위인 Stone Brook보다 중간값은 낮지만 Outlier들로 인해 평균 판매가격을 1위를 하게 됐다.
# NorthRidge Heights도 Outlier가 높은 편에 속한다.
# NorthRidge, NorthRigde Heights 두 동네는 서울의 성북동과 같이 소수의 부자들이 살고 있는 곳으로 유추할수 있다.


## Overall Qual 과 Overall Cond 에 대한 ANOVA 실행

EXP1 <- lm(SalePrice ~ `Overall Qual` +`Overall Cond`,data = Table_FY)
EXP2 <- lm(SalePrice ~ `Overall Qual` ,data = Table_FY)
summary(EXP1)
summary(EXP2)

## Overall Qual 이 상대적으로 높은 R^2값과 작은 p-value가 산출됨


ggplot(ames_raw, aes(x=`Overall Qual`, y=SalePrice ))+ 
  geom_point()+geom_smooth(method=lm, formula = y ~ x)


## 평균 집 판매가격 순위와 비슷하게 산출됨 - 당연한 결과

ames_raw %>% select(Neighborhood, `Overall Qual`) %>%
  filter(Neighborhood %in% c("NoRidge","StoneBr","NridgHt","GrnHill","Veenker","Timber","Somerst","ClearCr","Crawfor","CollgCr")) %>%
  group_by(Neighborhood) %>% summarize(MeanQual = mean(`Overall Qual`)) %>% arrange(desc(MeanQual))

# 전체적인 Overall Qual 평균과 비교
mean(Table_FY$`Overall Qual`)


## Boxplot 의 결과 역시나 North Ridge는 Outlier의 결과가 많이 나옴.
ggplot(Table_SP, aes(x = Neighborhood, y = `Overall Qual`, fill=Neighborhood)) + geom_boxplot(alpha=0.5,show.legend = TRUE) +
  theme(legend.position="none") + stat_summary(geom='point',fun=mean,shape=23,size=3)





## 최근에 지어지거나, 리모델링한  집일 수록 판매가격이 높음.

Table_FY %>%  select(Neighborhood,FinalYear,SalePrice) %>% group_by(Neighborhood) %>% 
  summarize(YearMean = mean(FinalYear),SPMean = mean(SalePrice)) %>% arrange(desc(YearMean), desc(SPMean))


EXP3 <- lm(SalePrice ~ FinalYear, data = ames_raw)
summary(EXP3)

EXP4 <- lm(`Overall Qual` ~ FinalYear, data = ames_raw)
summary(EXP4)

ggplot(ames_raw, aes(x=FinalYear, y=SalePrice))+ 
  geom_point()+geom_smooth(method=lm, formula = y ~ x)

ggplot(ames_raw, aes(x=FinalYear, y=`Overall Qual`))+ 
  geom_point()+geom_smooth(method=lm, formula = y ~ x)




## Fence 가 MnWw인 경우는 대부분 North Ames에 속함
## 총 12 건중 9건이 North Ames
ggplot(ames_raw, aes(x=Fence, y=SalePrice, color=Neighborhood))+ 
  geom_point()+geom_smooth(method=lm, formula = y ~ x)

ames_raw %>% select(Neighborhood, Fence) %>% filter(Fence == "MnWw")



ames_raw %>% count(`Sale Type`) %>%
  ggplot(aes(x = n, y = fct_reorder(`Sale Type`, n))) +
  geom_col()

ames_raw %>% count(`Sale Condition`) %>%
  ggplot(aes(x = n, y = fct_reorder(`Sale Condition`, n))) +
  geom_col()

ames_raw %>% count(Neighborhood) %>% filter(n>100) %>%
  ggplot(aes(x = n, y = fct_reorder(Neighborhood, n))) +
  geom_col()

## linear method를 통해 상관 관계가 있는 것을 발견함.


## 지붕은 대부분 Standard Shingle을 사용함
## 2930 집 중 2887개 

ames_raw %>% count(`Roof Matl`)


ames_raw %>% count(Neighborhood) %>% filter(n<50) %>%
  ggplot(aes(x = n, y = fct_reorder(Neighborhood, n))) +
  geom_col()

## 하지만 ClearCr 집은 다양하게 섞여있음
  
ames_raw %>% select(`Roof Matl`, Neighborhood) %>%
  group_by('Roof Matl') %>% filter(`Roof Matl` == "Tar&Grv") %>%
  count(Neighborhood)

ames_raw %>% select(`Roof Matl`, Neighborhood) %>%
  group_by('Roof Matl') %>% filter(`Roof Matl` == "WdShake") %>%
  count(Neighborhood)

ames_raw %>% select(`Roof Matl`, Neighborhood) %>%
  group_by('Roof Matl') %>% filter(`Roof Matl` == "WdShngl") %>%
  count(Neighborhood)

ames_raw %>%  count(Neighborhood) %>% filter(n<50) %>% muta(Neighborhood, `Roof Matl`)
ames_raw %>% select(Neighborhood, `Roof Matl`) %>% 
  filter(Neighborhood %in% c("Blmngtn","Blueste","BrDale","ClearCr","Greens","Veenker","GrnHill","Landmrk","MeadowV","NPkVill","SWISU")) %>%
  ggplot(aes(x=`Roof Matl`, y=`Neighborhood`))+ 
  geom_point()
  
## 하지만 ClearCr 지역만지붕 재료가 다양함을 확인 할 수 있다. 

ames_raw %>% select(Neighborhood, `Roof Matl`) %>% 
  filter(Neighborhood == "ClearCr") %>% count(`Roof Matl`) %>%
  ggplot(aes(x = n, y = fct_reorder(`Roof Matl`, n))) +
  geom_col()

ames_raw %>% select(Neighborhood, `Roof Matl`) %>% 
  filter(Neighborhood == "ClearCr") %>% count(`Roof Matl`)

# 44 집 중 12 (약 25%) 집이 다른 재료를 사용  
# 평균98.53%가 Standard Shingle을 사용한다는 점을 보면 큰 차이가 남.





#실수 목록들
#띄어쓰기된 열이름은 mean이 적용되지 않아서 이름을 변경함.
#Year Remod/Add -> FinalYear
# `을 ' 로 착각해서 이런 오류가 발생함, 띄어쓰기는 상관 없었다,,,

Table_FY<-ames_raw

names(Table_FY)
names(Table_FY)[22] <- c("FinalYear")
names(Table_FY)[79] <- c("YrSold")
names(Table_FY)[80] <- c("SaleType")
names(Table_FY)[81] <- c("SaleCondition")




