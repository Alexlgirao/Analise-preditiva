library(tidyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(nortest)
library(lmtest)
library(cars)
library("e1071")
library(caret)

sales <-  read.csv(file="sales.csv", header=TRUE, sep=",")
cp <- read.csv(file="comp_prices.csv", header=TRUE, sep=",")
str(sales)
str(cp)

summary(sales)
summary(cp)

head(sales)
head(cp)

tail(sales)
tail(cp)
#acertando os campos , transformando o campo data em YMD 

sales$DATE_ORDER <- as.Date(sales$DATE_ORDER)
cp$DATE_EXTRACTION <- as.Date(cp$DATE_EXTRACTION)

#verificando valores faltando
colSums(is.na(sales))
colSums(is.na(cp))

#como são 9 produtos , A escolha de modelar somente um produto será feita pelo produto que tem maior volume de vendas . 
vol_vendas <- sales %>%
  group_by(PROD_ID) %>%
  summarise(QTY_ORDER = sum(QTY_ORDER) , REVENUE = sum(REVENUE))

summary(vol_vendas)
vol_vendas
ggplot(vol_vendas, aes(x=PROD_ID,y= QTY_ORDER)) + geom_col()

#O produto 7 aparece com o maior volume de vendas sendo maior que a soma de todos os outros produtos vendidos.
#separando o Produto 7 

sp7 <-  sales %>% 
  filter(PROD_ID == "P7")

# Separação das vendas por dia e criaçao do preço unitario do dia 
sp7_diario <- sp7 %>%
  group_by(DATE_ORDER, PROD_ID) %>%
  summarise(QTY_ORDER = sum(QTY_ORDER) , REVENUE = sum(REVENUE))%>%
  mutate(PRICE_UNI = (round(REVENUE/QTY_ORDER ,2)))

sp7_diario1 <- as.data.frame(sp7_diario)
sp7_final <- subset(sp7_diario1,select=-c(PROD_ID,REVENUE))
# trabalhando a Base de concorrentes
cp7 <- cp %>% 
  filter(PROD_ID == "P7")

cp71 <- cp7 %>%
  group_by(DATE_EXTRACTION,COMPETITOR,PAY_TYPE) %>%
  summarise(COMPETITOR_PRICE = mean(COMPETITOR_PRICE))
##### remover a ultima linha do dia 	2015-10-14 por apresentar erro de medição 
cp711 <- cp71 %>% filter(DATE_EXTRACTION < "2015-10-14")

#Transformando as variaveis concorrentes e forma de pagamento em uma unica variavel competidor_paymethod

dtest4 <- unite(cp711, col = "COMP_PAY", COMPETITOR,PAY_TYPE, sep= "_")
head(dtest4)

#Fazendo spread nas variaveis 
dtest5 <- spread(dtest4, COMP_PAY, COMPETITOR_PRICE)
head(dtest5)

#fill NA para a mediana das observaçoes para não alterar muito a base 
df <- as.data.frame(dtest5)
final <- df %>% 
  mutate_at(vars(starts_with("C")), ~ifelse(is.na(.), median(., na.rm = TRUE), .))
head(final)

#Join nos 2 data frames 
base_prod7 <- left_join(sp7_final,final, by = c("DATE_ORDER" = "DATE_EXTRACTION"))
base_fina <- subset(base_prod7, select = -c(DATE_ORDER))
summary(base_fina)
#ainda tem algum NA , trocar o NA pela mediana novamente
base_final <-  base_fina %>% 
  mutate_at(vars(starts_with("C")), ~ifelse(is.na(.), median(., na.rm = TRUE), .))




#########################################################################################
#checando correlaçao
res <- cor(base_final)
round(res, 2)
heatmap(abs(cor(base_final)))

##################################################################################################
#Separar a base em Train e test
set.seed(1234)
sample <- sample.int(n = nrow(base_final), size = floor(.80*nrow(base_final)), replace = F)
train <- base_final[sample, ]
test  <- base_final[-sample, ]


############# Modelagens #####################################################
### Regressão linear / logistica / stepwise 

model1 <- lm(QTY_ORDER~., data=train)# modelo simples de regressão
summary(model1)

###### Regressão logistica 
model <- glm(QTY_ORDER~., data=train)
summary(model)
t12 <- glm(QTY_ORDER ~ PRICE_UNI + C4_1, data=train)
summary(t12)
pred_log <- predict(t12, newdata = test)
rmse_log <- sqrt(sum((pred_log - test$QTY_ORDER)^2)/length(test$QTY_ORDER))
c(RMSE = rmse_log, R2=summary(t12)$r.squared)

###### regressao linear com step 
model2 <- step(model1, direction ="backward" )
summary(model2)
vif(model2)
model3 <- step(model1, direction="both") ###### como a direçao do step não alterou o R2 pode escolher qual usar  
summary(model3)
pred_step <- predict(model2, newdata = test)
rmse_step <- sqrt(sum((pred_step - test$QTY_ORDER)^2)/length(test$QTY_ORDER))

c(RMSE = rmse_step, R2=summary(model2)$r.squared) 

par(mfrow=c(1,1))
plot(test$QTY_ORDER, pred2, xlim=c(-100,2000), ylim=c(-100,3000))
abline(lm(test$QTY_ORDER ~ pred_step), col="red")
####################### arvore de regressão ############
library(rpart)
tre <- rpart(QTY_ORDER ~., data=train, method = "anova")
summary(tre)
printcp(tre)
rsq.rpart(tre)
tre$
pred_tree <- predict(tre, newdata = test)
rmse_tree <- sqrt(sum((pred_tree - test$QTY_ORDER)^2)/length(test$QTY_ORDER))
 
######################## Random forrest ###################
library(randomForest)
tre1 <- randomForest(QTY_ORDER ~., data = train , importance = TRUE)
print(tre1)
importance(tre1)
pred_rnd <- predict(tre1, newdata = test)
rmse_rnd <- sqrt(sum((pred_rnd - test$QTY_ORDER)^2)/length(test$QTY_ORDER))

############
#Até aqui o melhor resultado de previsão foi de 40% no modelo random forrest. com o menor RMSE em 509
#para melhorar o score talvez utilizar lag nas variaveis ou alguma outra transformação nas variaveis 
##############################################################################
