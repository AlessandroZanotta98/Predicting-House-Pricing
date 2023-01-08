library(readr)
library(tidymodels)
library(ggplot2)
library(tidyverse)
library(rlang)
tidymodels_prefer()
library(modeldata)
library(ggpubr)
library(hexbin)
library(MASS)
library(colorRamps)
library(corrplot)
library(patchwork)
library(splines)
library(parsnip)
library(stacks)
#library(splitstackshape)

##funzioni per grafici ====
grafico <-function(dati,x,y,d,lm,qual){
  if (qual==FALSE & lm==FALSE)
    ggplot(dati,aes_string(x,y))+
    geom_point(alpha=0.1)
  if (qual==TRUE & lm==FALSE)
    ggplot(dati, aes_string(x, y)) + #importante sqft living
    geom_point(alpha = 0.1) +
    facet_wrap(d)
  if (qual==TRUE & lm==TRUE)
    ggplot(dati, aes_string(x, y)) + #importante sqft living
    geom_point(alpha = 0.1) +
    facet_wrap(d)+
    geom_smooth(method = "rlm")
}

plot_smoother <- function(deg_free) {
  ggplot(ames_train, aes(x = Latitude, y = Sale_Price)) + 
    geom_point(alpha = .2) + 
    scale_y_log10() +
    geom_smooth(
      method = lm,
      formula = y ~ ns(x, df = deg_free),
      col = "red",
      se = FALSE
    ) + theme_bw() +
    ggtitle(paste(deg_free, "Spline Terms"))
}  





#0.import dei dati####

PATH <- "https://raw.githubusercontent.com/aldosolari/DM/master/docs/HomePrices/"
train = read_csv2(paste0(PATH,"home_prices_train.csv"))
test = read_csv2(paste0(PATH,"home_prices_test.csv"))
fit <- linear_reg() %>% set_engine("lm") %>% fit(price ~ ., data = train)
yhat <- predict(fit, new_data = test)
write.table(file="2575_previsione.txt", yhat$.pred, row.names = FALSE, col.names = FALSE)
tidy(fit)


n <- nrow(train)
#id <- 1:n
str(train)
cor(train$sqft_above+train$sqft_basement,train$sqft_living)
train$sqft_basement=ifelse(train$sqft_basement==0,log(1),log(train$sqft_basement,base=10))
cor(log(train$sqft_above,base=10)+train$sqft_basement,log(train$sqft_living,base=10))
cor(log(train$sqft_above,base=10),log(train$sqft_living,base=10))
cor(train$sqft_basement,log(train$sqft_living,base=10))
cor(train$sqft_above,train$sqft_living)
cor(train$sqft_basement,train$sqft_living)

gghistogram(train,x="price",color="orange",fill="orange")
plot_map = ggplot(train, 
                  aes(x = longitude, y = lattitude, fill = price),colour="black") +
  geom_point(alpha = 0.3,pch=21) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Data Map") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradientn(colours = c( "green","white", "red"),
                       values = scales::rescale(c(4, 5, 6.5)))+                                
  labs(color = "Price", size = "Sqft_living")+
  theme_classic()

plot_map
#1.SPENDING OUR DATA divisione in training e test ####
set.seed(123)
train_split <- initial_split(train, prop = 0.80, strata = price) 
data_train <- training(train_split)
data_test  <- testing(train_split)



sort(unique(data_train$yr_built))
sort(unique(data_train$year_renovated))
sort(unique(data_train$date_sold))




#ANALISI ESPLORATIVA #####
#osservazioni, traducendo dall'inglese
#sqft_living: sqft_above+sqft_basement.

#motivo per cui le trasformo in base 10, in modo tale da 
#far si che siano più combinazioni lineari tra loro
data_verifica <- data_train %>% 
  select(sqft_living,sqft_above,sqft_basement)

table(log(data_verifica$sqft_living,base=10)==log(data_verifica$sqft_above,base=10)+log(data_verifica$sqft_basement,base=10))
table(data_verifica$sqft_above+data_verifica$sqft_basement==data_verifica$sqft_living)
#inutile tenere sqft_above, sqft basemenet

#indago case con 0 bagni
data_train %>%
  filter(bathrooms==0) %>%
  select(price) #9 case con 0 bagni le considero non abitabili in quanto 
                #non è presente il bagno (riferimento bibliografico)
table(data_train$bathrooms)

table(train$bathrooms)
table(test$bathrooms)


colnames(data_train)
range(data_train$sqft_living)

case_grandi <- data_train %>%
  filter(bathrooms>5) %>%
  select(sqft_living)
range(case_grandi$sqft_living)
win.graph()
data_train %>% 
  ggplot(aes(x=as.factor(bathrooms),y=price,fill=as.factor(bathrooms))) +
  geom_boxplot() 
win.graph()
data_train %>% 
  ggplot(aes(x=as.factor(round(bathrooms)),y=price,fill=as.factor(round(bathrooms)))) +
  geom_boxplot() 

#aumenta numero di bagni aumenta il prezzo
#aumenta la mediana e il range di variazione del prezzo, quindi è ragionevole
#pensare che più bagni hanno le case più costa, ma ci sta in quanto più bagni ha
#più la casa è grande, come si può notare dal boxplot qui riportato.
win.graph()
data_train %>% 
  ggplot(aes(x=as.factor(bathrooms),y=log10(sqft_living),fill=as.factor(bathrooms))) +
  geom_boxplot() 
data_train %>% 
  ggplot(aes(x=as.factor(bedrooms),y=price,fill=as.factor(bedrooms))) +
  geom_boxplot() 



#price in base a floors
win.graph()
data_train %>% 
  ggplot(aes(x=as.factor((floors)),y=price,fill=as.factor((floors)))) +
  geom_boxplot() 
#da qui creo others in floors
win.graph()
data_train %>% 
  ggplot(aes(x=as.factor(round(floors)),y=price,fill=as.factor(round(floors)))) +
  geom_boxplot() 
win.graph()
data_train %>% 
  ggplot(aes(x=as.factor((floors)),y=log10(sqft_living),fill=as.factor(floors))) +
  geom_boxplot() 

#graficogiustificare imputazione numeri letto-----
require(gridExtra)
library(gridExtra)
living_bed<- data_train %>% 
  ggplot(aes(x=as.factor((bedrooms)),y=log10(sqft_living),fill=as.factor(bedrooms))) +
  geom_boxplot()+
  guides(fill=guide_legend(title="Number of Bedrooms"))+
  xlab("Number of Bedrooms")+
  ylab("Sqft_living")

bed_price <- data_train %>% 
  ggplot(aes(x=as.factor(bedrooms),y=price,fill=as.factor(bedrooms))) +
  geom_boxplot()+
  guides(fill=guide_legend(title="Number of Bedrooms"))+
  xlab("Number of Bedrooms")+
  ylab("Price")

bed_price
living_bed
grid.arrange(living_bath,bath_price,ncol=2)

win.graph()
data_train %>% 
  ggplot(aes(x=as.factor(bathrooms),y=log10(sqft_living),fill=as.factor(bathrooms))) +
  geom_boxplot()+
  guides(fill=guide_legend(title="Number of Bathrooms"))+
  xlab("Number of Bathrooms")+
  ylab("Sqft living")



sort(unique(data_train$bathrooms))
#come mostrato (riferimento bibliografico) le in america per 0.5 bagni si 
#intende che hanno un bagno con il lavandino e la toilet, quindi nel dataset 
#sono presenti valori anomali, ovvero tutti quei numeri compresi tra la metà
# e l'intero più vicino, come li sistemo? Vado a confrontare una soglia 
# di sqft living con i bagni e decido se arrontondare per difetto o meno

rand_forest_randomForest_spec <-
  rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine('randomForest') %>%
  set_mode('regression')

#soglie sqft_living per arrotondamento
bagni <- sort(unique((data_train$bathrooms)))

data_bagni_sqft_living <- data_train %>%
  select(bathrooms,sqft_living) %>%
  filter(bathrooms>4.25)

range(data_bagni_sqft_living$sqft_living)

mean(data_train$sqft_living)
medie_sqftliving <- vector(length = 8)
length(bagni)
for (i in 1:(length(bagni)-1)){
  numeri <- data_bagni_sqft_living%>%
    filter(bagni[i]<bathrooms & bathrooms<bagni[i+1])
  medie_sqftliving[i] <- mean(numeri$sqft_living)
}
medie_sqftliving
#correlazioni
data_cor <- data_train %>%
  select(price,bedrooms,bathrooms,sqft_living,sqft_lot,
         floors,yr_built,year_renovated,lattitude,
         longitude,nn_sqft_living,nn_sqft_lot)

tmwr_cols <- colorRampPalette(c("#91CBD765", "#CA225E"))
win.graph()
data_cor %>% 
  cor() %>% 
  corrplot(method = "number")

#indago casa con 33 camere e 11 camere
data_train %>%
  filter(bedrooms==33 | bedrooms==11)
#una camera deve essere dagli 8 a 12 metri quadri, qunidi fai equazione 
#motivo per cui rimuoviamo queste due osservazioni

#fare grafici con dati train
win.graph()
ggplot(data_train, aes(y = (floors))) + 
  geom_bar(fill = 'blue') + 
  theme_classic() +
  ggtitle("Frequency of houses with different floors")+
  labs(x="House",y="floors")
#da mettere in analisi esplorativa, ricordati di mettere step_other
win.graph()
ggplot(data_train, aes(y = bathrooms)) + 
  geom_bar(fill = 'blue') + 
  theme_classic() +
  ggtitle("Frequency of houses with different bathrooms")+
  labs(x="House",y="bathrooms")

#graifici zip_code, zip code può essere interpretato come una dummy in quanto 
#mi permette di vedere se una casa appartiene ad una certa abitazione o meno
win.graph()
ggplot(data_train,aes(zip_code))+
  geom_bar()

grafico(data_train,x="sqft_living",y = "price",d = "bathrooms",lm=TRUE,qual=TRUE)




#presenza outlier

#=====outlier=====
summary_price <- summary(data_train$price)#indaghiamo dopo
Q1 <- summary_price[2]
Q3 <- summary_price[5]
diff_inter <- Q3-Q1
outlier <- data_train %>%
  filter(price>Q3+diff_inter | price<Q1-diff_inter)

str(data_train)
sort((unique(data_train$sqft_above)),decreasing = TRUE)

table(data_train$bathrooms)/nrow(data_train)
# MODELLO LINEARE---------
#RECIPE LM------

data_recipe_lm <- recipe(price~.,data=data_train) %>%
  step_mutate(tipologia=ifelse(bathrooms>0 & bathrooms<1,"open space",ifelse(bathrooms>4.25,"grande","normale"))) %>%
  step_mutate(bathrooms=round(bathrooms)) %>%
  step_mutate(floors=ifelse(floors>2.5,"other",floors)) %>%
  step_mutate(bedrooms=replace(bedrooms,bedrooms==0,1))%>%
  step_mutate(sqft_lot=log10(sqft_lot)) %>%
  step_mutate(sqft_living=log10(sqft_living)) %>%
  step_mutate(nn_sqft_living=log10(nn_sqft_living)) %>%
  step_mutate(sqft_abbove=log10(sqft_above))%>%
  step_ns(longitude,deg_free = 18) %>%
  step_ns(lattitude,deg_free = 22) %>%
  step_mutate(sqft_basement=ifelse(sqft_basement>0,log10(sqft_basement),log10(1))) %>%
  step_mutate(year_old=ifelse(year_renovated>1965,"nuova","vecchia")) %>%
  step_mutate(renovated=ifelse(year_renovated-yr_built>0,"yes","no")) %>%
  step_mutate(storica=ifelse(yr_built<1980,"yes","no")) %>%
  #step_mutate(open_space=ifelse(bathrooms>0,"no","yes")) %>%
  step_mutate(Living_basement=ifelse(sqft_basement>0,"yes","no")) %>%
  step_mutate(sqft_garden = ifelse(sqft_lot - sqft_living > 0, sqft_lot - sqft_living, 0)) %>%
  step_mutate(garden=ifelse(sqft_garden>0,"yes","no")) %>%
  step_mutate_at(c(waterfront,view,condition,year_old,renovated,storica,zip_code,
                   floors,Living_basement,bedrooms,tipologia,bathrooms,garden),fn=as.factor) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_interact(~sqft_lot:starts_with("storica")) %>%
  step_interact(~sqft_lot:starts_with("condition_poor")) %>%
  step_interact(~sqft_lot:starts_with("condition_fair")) %>%
  step_interact(~sqft_lot:starts_with("condition_very_good")) %>%
  step_interact(~sqft_living:starts_with("storica")) %>%
  step_interact(~sqft_living:starts_with("condition_poor")) %>%
  step_interact(~sqft_living:starts_with("condition_fair")) %>%
  step_interact(~sqft_living:starts_with("condition_very_good")) %>%
  step_interact(~sqft_living:starts_with("zip_code_")) %>%
  step_interact(~sqft_living:starts_with("Living_basement")) %>%
  step_interact(~sqft_lot:starts_with("tipologia")) %>%
  step_interact(~sqft_basement:starts_with("floors")) %>%
  step_rm(date_sold)

#serve per controllo#
data_trained <- prep(data_recipe_lm,data_train,verbose = TRUE)

#operazioni effettuate sul training
data_juiced <- juice(data_trained)
dim(data_juiced)
colnames(data_juiced)
#ricavo dataset nuovo, con il comando juice
data_baked <- bake(data_trained,new_data = data_test)
dim(data_baked)
colnames(data_baked)
#per fare le previsioni devo avere le stesse variabili sul test, la stessa
#matrice di dati che ho sul training
#tunato, non far più girare 

#TUNING SPLINES (GENERALIZED ERROR)--------

parameters(data_recipe_lm)
data_lm_param <- 
  data_recipe_lm %>% 
  parameters() %>% 
  update(
    `long df` = spline_degree(), 
    `lat df` = spline_degree()
  )
df_vals <- seq(2, 55, by = 4)
spline_grid <- expand.grid(`long df` = df_vals, `lat df` = df_vals)
lm_mod <- linear_reg() %>% set_engine("lm")
set.seed(123)

cv_splits <- vfold_cv(data_train, v = 5, repeats=2,strata = price)
data_lm_res <- tune_grid(lm_mod, data_recipe_lm, resamples = cv_splits, grid = spline_grid,
                         metrics = metric_set(mae),control = control_grid(verbose=TRUE))

data_lm_res %>% show_best(metric="mae") #data_lm_res al posto di ames_res
lm_best_params <- data_lm_res %>%
  tune::select_best("mae")
knitr::kable(lm_best_params)

#top con 18(42) long df e 22(42) lat df
autoplot(data_lm_res) + theme_bw()
ames_res




#GRAFICI-----------
str(data_juiced)
colnames(data_juiced)
ggplot(data_juiced, aes(x = sqft_living, y = price)) + #importante sqft living
  geom_point(alpha = 0.1) +
  facet_wrap(~ year_old_vecchia, nrow=4) +
  geom_smooth(method = "rlm") + 
  theme_bw()

#per questi grafici fai confronto prima e dopo log10
win.graph()
gghistogram(data_juiced,x="price",bins=30)

win.graph()
gghistogram(data_juiced,x="sqft_lot",bins=30,color="blue",fill="blue",
            title="sqft_lot after transformation")

require(gridExtra)
library(gridExtra)


sqft_before <- gghistogram(data_train,x="sqft_living",bins=30,color="blue",fill = "blue",
            title="sqft_living before transformation")


sqft_after <- gghistogram(data_juiced,x="sqft_living",bins=30,color = "blue",fill="blue",
            title = "sqft_living after transformation")


grid.arrange(sqft_before,sqft_after)

win.graph()
gghistogram(data_train,x="sqft_living",bins=30,color="blue",fill="blue",
            title="sqft_living before transformation")


win.graph()
gghistogram(data_juiced,x="sqft_basement",bins=30)

win.graph()
gghistogram(data_train,x="sqft_basement",bins=30)


win.graph()
gghistogram(data_juiced,x="nn_sqft_living",bins=30)


#sqft_basement no lo10 perchè alcune case non hanno basement
win.graph()
grafico(data_juiced,x="sqft_living",y="price",d = "waterfront_X1",qual=TRUE,lm=TRUE)
# no interazione

win.graph()
grafico(data_juiced,x="bedrooms",y="price",d = "waterfront_X1",qual=TRUE,lm=TRUE)
#interazione tra bedrooms e waterfront 
win.graph()
hexbinplot(price~bedrooms|waterfront_X1,data_juiced)

win.graph()
grafico(data_juiced,x="sqft_lot",y="price",d="tipologia_open.space",qual=TRUE,lm=TRUE)
#interazione bellissima
colnames(data_juiced)
win.graph()
grafico(data_juiced,x="sqft_lot",y="price",d="Living_basement_yes",qual=TRUE,lm=TRUE)
#interazione piccola
win.graph()
grafico(data_juiced,x="sqft_lot",y="price",d="condition_fair",qual=TRUE,lm=TRUE)
#interazione bellissima
win.graph()
grafico(data_juiced,x="sqft_lot",y="price",d="condition_poor",qual=TRUE,lm=TRUE)
colnames(data_juiced)
win.graph()
grafico(data_juiced,x="sqft_lot",y="price",d="condition_very_good",qual=TRUE,lm=TRUE)
#bellissima

win.graph()
grafico(data_juiced,x="sqft_basement",y="price",d="condition_very_good",qual=TRUE,lm=TRUE)

win.graph()
hexbinplot(price~sqft_basement|giardino,data_juiced)


# SPECIFICAZIONE MODELLO LINEARE --------

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_recipe(data_recipe_lm) %>%
  add_model(lm_model)
#PREVISIONI SUL TRAINING (EMPIRICAL ERROR)-------
#38/34 0.05509563
m_fit_trn <- lm_wflow %>%
  fit(data=data_train)
m_fit_trn$fit
lm.pred_trn <- predict(m_fit_trn, new_data = data_train %>% select(-price))
Metrics::mae(actual = data_train$price, predicted = lm.pred_trn$.pred)
mean(abs(lm.pred_trn$.pred-data_train$price))
#errore sul train 0.05561232

colnames(data_juiced)


#PREVISIONI SUL TEST MODELLO LINEARE--------------
m_fit1 <- lm_wflow %>%
  fit(data=data_train)
tidy(m_fit1)
lm.pred1 <- predict(m_fit1, new_data = data_test %>% select(-price))
Metrics::mae(actual = data_test$price, predicted = lm.pred1$.pred)
mean(abs(lm.pred1$.pred-data_test$price))
#0.05538191 (38-34) con aggiunta giardino 0.05537687

win.graph()
autoplot(data_lm_res,metric="mae")+theme_bw()

#PREVISIONI SUL TEST FINALE VERO-----
fit_finale <- lm_wflow %>%
  fit(data=train)
yhat_finale <- predict(fit_finale, new_data = test)
yhat_finale$.pred
ytrue <- previsione_corretta$V1

Metrics::mae(actual=ytrue,predicted = yhat_finale$.pred)
mean(abs(yhat_finale$.pred-ytrue))
#0.05622922

#XGBOOST------

#RECIPE XGBOOST---------
data_recipe_xgboost <- recipe(price~.,data=data_train) %>%
  #step_mutate(anno=format(as.Date(date_sold,format="%Y-%m-%d","%Y",tz="UTC"),"%Y")) %>%
  #step_mutate(mese=format(as.Date(date_sold,format="%Y-%m-%d","%m",tz="UTC"),"%m")) %>%
  #step_mutate(giorno=format(as.Date(date_sold,format="%Y-%m-%d","%d",tz="UTC"),"%d"))%>%
  step_mutate(date_sold=as.integer(format(as.Date(date_sold), "%Y%m%d")))%>%
  step_normalize(c(sqft_living, sqft_lot, sqft_above, nn_sqft_living, nn_sqft_lot)) %>%
  #step_mutate(bathrooms=round(bathrooms)) %>%
  step_mutate(bedrooms=replace(bedrooms,bedrooms==33,3)) %>%
  step_mutate(bedrooms=replace(bedrooms,bedrooms>=8,8)) %>%
  step_mutate(bedrooms=replace(bedrooms,bedrooms==0,1)) %>%
  step_mutate(floors=round(floors)) %>%
  step_mutate(perc_cas=(sqft_above/sqft_living)*100)%>%
  step_mutate(perc_cas_2=(sqft_basement/sqft_living)*100)%>%
  step_mutate(bathrooms=replace(bathrooms, bathrooms>=5.25, 6)) %>%
  #step_log(sqft_lot,sqft_living,nn_sqft_living,sqft_above,base = 10)%>%
  #step_mutate(sqft_lot=log10(sqft_lot)) %>%
  #step_mutate(sqft_living=log10(sqft_living)) %>%
  #step_mutate(nn_sqft_living=log10(nn_sqft_living)) %>%
  #step_mutate(sqft_above=log10(sqft_above)) %>%
  #step_mutate(sqft_basement=ifelse(sqft_basement>0,log10(sqft_basement),log10(1))) %>%
  step_mutate(year_old=year_renovated-yr_built) %>%
  #step_mutate(renovated=ifelse(year_renovated-yr_built>0,"yes","no")) %>%
  #step_mutate(storica=ifelse(yr_built<1980,"yes","no")) %>%
  #step_mutate(abitabile=ifelse(bathrooms>0,"yes","no")) %>%
  #step_mutate(Living_basement=ifelse(sqft_basement>0,"yes","no")) %>%
  #step_other(zip_code,threshold = 0.05) %>%
  #step_mutate_at(c(waterfront,view,condition,year_old,renovated,storica,zip_code,
                   #floors,Living_basement,abitabile,bedrooms,anno,mese,giorno),fn=as.factor) %>%
  step_mutate(sqft_garden = ifelse(sqft_lot - sqft_living > 0, sqft_lot - sqft_living, 0)) %>%
  step_other(zip_code,threshold = 0.05) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_rm(sqft_basement) %>%
  prep()

#fai prove aggiungendo e togliendo add_recipe

#preproc <- data_recipe_xgboost %>%
  #prep()

controllo <- juice(data_recipe_xgboost)
str(controllo)
colnames(controllo)
train_cv_folds <- 
  recipes::bake(
    data_recipe_xgboost, 
    new_data = training(train_split)
  ) %>%  
  rsample::vfold_cv(v = 10,repeats = 2,strata = price)
#v=10,repeats=2

xgboost_model<-
  boost_tree(mode="regression",tree_depth = 15,#12 
             trees = 500, #500
             learn_rate = 0.0309754, #0.0400719 
             min_n = 47,#37 
             loss_reduction =  2.2e-06) %>% #2.1e-06
  set_engine('xgboost') %>%
  set_mode('regression')

boost_tree_wflow <- 
  workflow() %>%
  add_model(xgboost_model) %>%
  add_recipe(data_recipe_xgboost)
  #add_formula(price~ .)
  #aggiunta, hai tolto add_formula(price tilde.)

#TUNING XGBOOST---------
xgboost_params <- 
  dials::parameters(
    min_n(range = c(30,50)),
    tree_depth(range = c(8,20)),
    learn_rate(range =c(0.03,0.05),trans = NULL),
    loss_reduction(range=c(2e-06,2.2e-06),trans=NULL))

set.seed(123)
xgboost_grid <- 
  dials::grid_max_entropy(
    xgboost_params,
    size=10,
    param_info=xgboost_params
  )

knitr::kable((xgboost_grid))
dim(xgboost_grid)
xgboost_tuned <- 
  tune::tune_grid(
    object = boost_tree_wflow,
    resamples = train_cv_folds,
    grid = xgboost_grid,
    metrics = metric_set(mae),
    control = control_grid(verbose = TRUE),
  )

xgboost_tuned %>%
  tune::show_best(metric="mae")
xgboost_best_params <- xgboost_tuned %>%
  tune::select_best("mae")
knitr::kable(xgboost_best_params)

xgboost_model_final <- xgboost_model %>% 
  finalize_model(xgboost_best_params)
xgboost_model_final
#min n 34|tree depth 4|  learn rate 0.0469438 |loss reduction 0
#mean mae 0.0532 (generalized error)


m_fit_trn_xgb <- boost_tree_wflow %>%
  fit(data=data_train)
m_fit_trn_xgb$fit
xgboost.pred_trn <- predict(m_fit_trn_xgb, new_data = data_train %>% select(-price))
Metrics::mae(actual = data_train$price, predicted = xgboost.pred_trn$.pred)
mean(abs(xgboost.pred_trn$.pred-data_train$price))
#empirical error 0.04522767, scritto giusto diventa 0.04638426
#0.02178754 con recipe
#0.03923675 senza date sold e trasformazioni e sqft_above
# 0.03872017 con giorno
#0.03587541 06/06/2022 23.40
# 0.03503077 con round(bathrooms)
# 0.03526925 con recipe fatto bene
# 0.03593591
m_fit_test_xgb <- boost_tree_wflow %>%
  fit(data=data_train)
xgboost.pred1 <- predict(m_fit_test_xgb, new_data = data_test %>% select(-price))
Metrics::mae(actual = data_test$price, predicted = xgboost.pred1$.pred)
mean(abs(xgboost.pred1$.pred-data_test$price))

# 0.05169138 su test
# 0.05498562 su test dopo xgboost tunato
# 0.0548271 su test dopo aver messo giorno
# 0.052988358 23.42 06/06/2022
# 0.05250058 su test dopo aver tolto le dummy create
# 0.05279597 ultimo corretto con recipe
# 0.05170675 ultim corretto con yr_renovated
# 0.05209951 dopo stanze messe a 0

mean(abs(xgboost.pred1$.-ytrue))
length(xgboost.pred1$.pred)

#PREVISIONI SUL TEST FINALE VERO-----
fit_finale_xg <- boost_tree_wflow %>%
  fit(data=train)
yhat_finale <- predict(fit_finale_xg, new_data = test)
yhat_finale$.pred
ytrue <- previsione_corretta$V1

Metrics::mae(actual=ytrue,predicted = yhat_finale$.pred)
mean(abs(yhat_finale$.pred-ytrue)) #0.05217662
#0.05628121
#0.05520909
#0.05293947
#0.05291216 06/06/2022 23.40
#0.05211617
#0.05198087
#0.05197646
#RANDOM FOREST-----

rand_forest_randomForest_spec <-
  rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine('randomForest') %>%
  set_mode('regression')






#ENSEMBLE-------



lm_weights <- seq(0,1,by=0.15)
xgb_weights <- seq(0,1,by=0.15)
xgb_weights
mae <- vector(mode = "numeric",length = length(lm_weights))
length(lm_weights)
for (i in 1:length(lm_weights)){
  previsti <- (xgboost.pred_trn$.pred*xgb_weights[i])+(lm.pred_trn$.pred*lm_weights[100001-i])
  mae[i] <- Metrics::mae(actual = data_train$price, predicted = previsti)
  
}
which.min(mae)

