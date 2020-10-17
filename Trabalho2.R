dados <- read.csv(file="email_marketing.csv", header=TRUE, sep=",")
head(dados)

library(DataExplorer)
library(gridExtra)
library(dplyr)
library(tidyr)
library(plyr)
library("e1071")
library(caret)
library(caTools)

introduce(dados)

plot1 <- plot_intro(dados)
plot2 <- plot_missing(dados)
grid.arrange(plot1, plot2, ncol=2)
nrow(dados)
str(dados)
summary(dados)
# passando pelos valores vazios "" nas variaveis com fatores para decidir sobre a remoçao da variavel.
summary(dados$ds_ocpc)
234606/nrow(dados)
summary(dados$tp_sexo) # aqui vemos que tem 5 categorias aonde teriamos que ter 2 . Aqui simplesmente vou remover os valores diferentes de M ou F mais a frente .
summary(dados$tp_pess) # novamente vou optar por remover os vazios mais a frente .
summary(dados$ds_uf) # essa variavel eu vou testar de outras formas , removendo ela completamente e se o modelo não responder bem , transformarei em regiao do pais para testar ou transformar cada estado em uma variavel binaria separada .  

#filtrar os dados vazios da variavel tot_click
dados2 <- dados %>% filter(!is.na(fg_clik))
## retirando as variaveis com muita falta de dados conforme grafico anterior e a variavel id do cliente e cep e verificando a variavel ocupação verificamos um grande numero de respostas em branco 77%.  

dados3 <- select(dados2, -c(vl_salr,tp_camp_clik,qt_parc,qtd_dias_ult_tran,vl_tran,tp_form_pgto,tp_canl,tot_per_id,sum_val,id_pess_unif,ds_ocpc,cd_cep5,ds_uf))


#transformar os fatores da variavel origem de campanha em numeros 
dados4 <- dados3 %>% mutate(ds_orig = match(ds_orig,c("B","CN","M","R")))
#selecionando somente os valores M e F e F e J das colunas pois apresentam classes diferentes das que deveriam.
dados5 <- dados4 %>% filter(tp_sexo == c("F", "M"))
dados6 <- dados5 %>% filter(tp_pess == c("F", "J"))
dados6$tp_sexo <- droplevels(dados6$tp_sexo )
dados6$tp_pess <- droplevels(dados6$tp_pess)
#transformando as classes em numeros F = 1 e M e J = 2 
dados7 <- dados6 %>% mutate(tp_sexo = match(tp_sexo,c("F", "M")),
                            tp_pess = match(tp_pess,c("F", "J"))
                            )
summary(dados7)

#removendo os NA`s e idades > 100 que possivelmente sejam erro de entrada `
dados8 <- dados7 %>% filter(!is.na(nr_idad))
dados8 <- dados8 %>% filter(nr_idad < 100)
summary(dados8)

# remover a variavel tp_canl_camp pois ela tem o mesmo valor para todos os resultados. 
dados8 <- select(dados8, -c(tp_canl_camp))

#finalizando a base de dados
base <- as.data.frame((dados8))
#transformando os valores faltando na mediana 
#O campo tp_camp_open pois tem um numero significativo de NA's , optei por excluir , a outra opção era transformar em 0 mas temos somente 4 campanhas e algum valor diferente
#deveria estar diferente de 0 pois ela categoriza a ultima campanha aberta pelo cliente, ou simplesmente filtar os NA`s e os valores 0 . Mas pra simplificar no inicio excluirei . `
base <-base %>% 
  mutate_at(vars(starts_with("v")), ~ifelse(is.na(.), median(., na.rm = TRUE), .))
base <-base %>% 
  mutate_at(vars(starts_with("q")), ~ifelse(is.na(.), median(., na.rm = TRUE), .))
summary(base)
base <- select(base, -c(tp_camp_open))
base <- as.data.frame(base)
count(base, "fg_clik")
#base$fg_clik <- as.factor(base$fg_clik)
str(base)
table(base$fg_clik)
##### aqui vemos que a base é bastante desbalanceada , irei balancear a base após separar as bases para teste e validação


summary(base)
str(base)
cor(base)
res <- cor(base)
round(res, 2)
heatmap(abs(cor(base)))
########## Modelagen #######

set.seed(1234)
sample <- sample.int(n = nrow(base), size = floor(.80*nrow(base)), replace = F)

train <- base[sample, ]
test  <- base[-sample, ]


str(train)
str(test)
# usarei ROSE para balancear a base , a test ficará em modificações

library(ROSE)
balance.train <- ROSE(fg_clik ~., data=train, seed=3)$data
table(balance.train$fg_clik)
str(bal)
#### Regressão linear 

model1 <- lm(fg_clik~., data=balance.train)# modelo simples de regressão
summary(model1)
step(model1, direction= "both")
model11 <- lm(formula = fg_clik ~ tp_sexo + tp_pess + nr_idad + tp_estd_civl + 
                tp_resd + vl_rend + tp_escl + qtd_dias_ult_camp + tp_camp + 
                ds_orig + tot_env + tot_rec + tot_open + tot_clik, data = balance.train)
summary(model11)
#### 0.39 de r2  

####### regressão logistica 
##### testando alguns modelos 
model2 <-  glm(fg_clik~. ,data=balance.train )
summary(model2)
model21 <- glm(formula = fg_clik ~ tp_sexo + tp_pess + nr_idad + tp_estd_civl + 
                 tp_resd + vl_rend + tp_escl + qtd_dias_ult_camp + tp_camp + 
                 ds_orig + tot_env + tot_rec + tot_open + tot_clik, data = balance.train)
summary(model21)
test1 <- predict(model21, type = "response")
head(test1)
model22 <- glm(formula = fg_clik ~., family = "binomial" ,data = balance.train)
summary(model22)
model222 <- glm(formula = fg_clik ~ tp_sexo + tp_pess + nr_idad + tp_estd_civl + 
                  tp_resd + vl_rend +  qtd_dias_ult_camp + tp_camp + 
                  ds_orig + tot_env + tot_rec + tot_open + tot_clik, family = "binomial" ,data = balance.train)
summary(model222)
test2 <- predict(model222,newdata = test, type = "response")
fit.test2 <- ifelse(test2 > 0.8,1,0) # transformando as predicts maiores de 80% em 1 
resu.t2 <- mean(fit.test2 != test$fg_clik)
print(paste('Accuracy',1-resu.t2)) 
table(test$fg_clik, fit.test2)
table(test$fg_clik)

#### tem algo errado aqui, acho que o modelo está prevendo os não cliques 

# decision tree
library(rpart)
library(rpart.plot)
tree1 <- rpart(fg_clik ~.,data = balance.train, model="anova")
summary(tree1)
rpart.plot(tree1)
importance(tree1)

### random forrest
library(randomForest)  
rf <- randomForest(fg_clik~., ntree=100, data = balance.train, mtry=4, importance = TRUE)
plot(rf)
print(rf)
varImp(rf)
pred.rf <- predict(rf,test,type = "response")
table(test$fg_clik, pred.rf > 0.5) 


########### to do ################
########### Melhorar o modelo de previsão logistica , testando tratar outras variaveis que foram excluidas.
########### Gerar um score para cada cliente.
###############################################
