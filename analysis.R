# в данном скрипте мы строим линейную модель зависимости стоимости метра квадратного от других параметров (собранных с помощью скрипта preProcessing.R).   (ЧЕРНОВОЙ ВАРИАНТ)


# Предварительно проведем небольшой РАЗВЕДОЧНЫЙ АНАЛИЗ
# факторный анализ
# район города
fit <- aov(pricem2~district, data = df[df$district!="" & df$district!="Пр.",])
tt2 <- TukeyHSD(fit)
print(tt2)

# корреляционный анализ
cor(x=df$nphoto, y=df$pricem2, use="complete.obs")
cor(x=df$nmonth, y=df$pricem2, use="complete.obs")

table(df$district)
library(ggplot2)
ggplot(data = df[df$district!="" & df$district!="Пр.",], aes(x=district, y = pricem2)) + geom_boxplot()

# количество комнат
fit <- aov(pricem2~nroom, data = df)
tt2 <- TukeyHSD(fit)
print(tt2)

2-1  -56.28621
3-1 -100.14127
4-1 -165.33849

ggplot(data = df, aes(x=nroom, y = pricem2)) + geom_boxplot()

# ДОПОЛНИТЕЛЬНАЯ ОБРАБОТКА ДАННЫХ

# Из текстовых полей можно выделить доп.информацию - маркеры
# мебель
# капремонт
# торг
# малосемейка
# срочно 
# долевое
# А через количество просмотров можно понять какие квартиры наиболее интересны



# Готовим дата-сет -------------------------------------------------------

# ОБЯЗАТЕЛЬНО выделить первичку и вторичку

predictors <- c("district", "tipdoma", "planirovka", "remont", "mrasst.f", 
                "nroom",  "nfloor.f", "area1",  "area2",  "nbalkon", 
                "vidbalkon",  "vozr",  "pricem2", "vozrcap", "area3")

# в df2 сохранена вторичка
df2 <- df[df$planirovka!="новостройка" & !is.na(df$planirovka), predictors]
df2$planirovka <- factor(df2$planirovka)



# удаление всех строк с 1 и более NA
df2 <- na.omit(df2)
sapply(df2, function(e) sum(is.na(e)))

# несколько правок - сделать кол-во балконов фактором - 0, 1з, 1н
df2$nbalkon <- as.character(df2$nbalkon)
df2$nbalkon[df2$nbalkon!="0" & df2$vidbalkon=="заст"] <- "1з"
df2$nbalkon[df2$nbalkon!="0" & df2$vidbalkon=="незаст"] <- "1н"
df2$nbalkon <- factor(df2$nbalkon, levels = c("0","1з", "1н"))
table(df2$nbalkon)

# вид балкона удаляем 
df2 <- df2[,-11]

# и возраст лучше сделать фактором
## для начала удалим -1
df2 <- df2[df2$vozr!=-1,]
hist(df2$vozr)

# что делать с капремонтом
ggplot(data = df2[df2$planirovka=="брежневка",], aes(x=vozrcap, y = pricem2)) + geom_boxplot()
ggplot(data = df2[df2$planirovka=="хрущевка",], aes(x=vozrcap, y = pricem2)) + geom_boxplot()
# капремонт удаляем, т.к. для хрущевок вообще никакой рзаницы, для брежневок и вовсе капремонт
# до 5 лет удешевляет стоимость - есть подозрение, что это по причине допущенного предположения
# о том, что если люди, пишут "дом после капремонта", то значит что капремонт был не позднее 5 лет назад
df2 <- df2[-13]

# area свести в одни через PCA - не помогло улучшить модель
# library(caret)
# preProc<-preProcess(df2[,c("area1","area2","area3", "vozr")], method="pca", thresh = 0.89)
# trainingPC <- predict(preProc, df2[,c("area1","area2","area3", "vozr")]) 
# preProc$numComp
# #trainingPC$PC1
# # удалим переменные "area1","area2","area3", "vozr" и добавим вместо них 2 найденные главные компоненты
# df3 <- df2[-c(8,9,11,13)]
# df3 <- data.frame(df3,pc1=trainingPC$PC1, pc2=trainingPC$PC2)

# удалить количество комнат, а также "area2","area3"
df2 <- df2[-c(6,9,13)]

# inTrain <- createDataPartition(y=df2$pricem2.f, p=0.9, list=F)
# training <- df2[inTrain,predictors]
# testing <- df2[-inTrain,predictors]


# Линейная модель ---------------------------------------------------------

fit_lm <- lm(pricem2~., data = df2)
summary(fit_lm)
hist(fit_lm$residuals)
shapiro.test(fit_lm$residuals)
shapiro.test(rnorm(100, mean = 5, sd = 3))
mean(fit_lm$residuals)
sd(fit_lm$residuals)
qqnorm(fit_lm$residuals)

fit_lm3 <- lm(pricem2~., data = df3[-6])
summary(fit_lm3)

# Adjusted R-squared:  0.4234


fit_lm2 <- lm(pricem2^0.5~., data = df2[-c(6,9)])
summary(fit_lm2)

fit_lm_full <- lm(pricem2~., data = df2) 
fit_lm_null <- lm(pricem2~1, data = df2) 
ideal_model <- step(object = fit_lm_full, scope = list(lower = fit_lm_null, upper = fit_lm_full), direction = 'backward')
summary(ideal_model)

# удалим половину признаков в линейной модели
fit_lm2 <- lm(pricem2~., data = df2[c(1:3,4,5,7,8,10,12)])
summary(fit_lm2)

# перебор всех параметров модели
library(leaps)
leaps <-regsubsets(pricem2~., data=df2, nbest=4)
plot(leaps, scale='adjr2')

# визуализация для категориальных переменных
library(vcd)
# mosaic(formula=district~nroom+nbalkon, data=df2)

# расчет коэффициента (взаимной) сопряжености Пирсона - для оценки степени зависимости качественных переменных
x=matrix(c(99,99,99,0,0,1,0,1,0), nrow=3)
y=matrix(c(99,0,0,99,0,0,0,99,0,0,99,0,0,0,99,0,0,99), ncol=6)
y=matrix(c(99,0,0,1,0,0,0,99,0,0,1,0,0,0,99,0,0,1), ncol=6)
assocstats(y)$cont

summary(assocstats(table(df2[,c(1,3)])))
assocplot(table(df2[,c(1,3)]))

library(DescTools)
# вот этот с correct = T возвращает точно единицу, но по дефолту стоит без корректировки
# данный коэффициент оценивает степень зависимости 2-х категориальных переменных
ContCoef(x=c(1,2,3), y = c(4,5,6), correct = T)

2/sqrt(13)

data("Arthritis")
tab <- xtabs(~Improved + Treatment, data = Arthritis)
summary(assocstats(tab))


# chisq.test(x)
chisq.test(x=df2$district, y=df2$planirovka)


## таблица сопряженности
ftable(x = df2[,c(3,5)], row.vars = 1) #col.vars=)

# зависимость непрерывных переменных
pairs2(df2[,c(8,9,11,14)])

fit2 <- train(pricem2.f~., data = training, method="rpart")#, type="Regression")
fit3= randomForest(pricem2.f~., data=training, ntree=200)
pred_fit3 <- predict(fit3, newdata = testing)
confusionMatrix(pred_fit3,testing$pricem2.f)

tt <- predict(fit2, newdata = testing)
tt[1]
confusionMatrix(tt,testing$pricem2.f)

library(rattle)
# install.packages("rpart.plot")
fancyRpartPlot(fit2$finalModel)

# важность переменных
varImp(fit2)	

summary(fit2)
names(df)

table(df$nbalkon)

dput(x=fit2, file = "fit2")
dput(x=training, file = "training")
dget("fit2")
runApp(appDir="d:/Grag/R/R-studio/Coursera/8_Data products/ShinyExamples/property/")

install_github('mytools', 'Grag2015')
library(mytools)
x <- 1
predict.flatvalue(x)
save(fit2,file="fit2.RData")
save(fit3,file="fit3.RData")
x <- data.frame("dd", "dd2")
x$dd <- 1
names(x) <- 
    tt <- training[1,]
save(tt, file="dftemp.RData") 
load(file="dftemp.RData")
rm(tt)

tt <- data.frame(district="Зав", tipdoma="кирпичный", planirovka="новостройка", remont="хороший ремонт",
                 mrasst.f="(0,100]", nroom="2",  nfloor=3, nfloormax=9, area1=35,  area2=20,  nbalkon=1, 
                 vidbalkon="заст",  vozr=10, pricem2.f="(600,800)")

fit_lda <- train(pricem2.f~., data=training, method="lda")
fit_lda <- lda(pricem2.f~., data =training)
predict(fit_lda, testing[1,])[1]

tt <- predict(fit_lda, newdata = training)
confusionMatrix(tt,training$pricem2.f)
save(fit_lda,file="fit_lda.RData")
tt

# на серверы шайни не захотел рабоатть алгоритм, подозреваю, что это из-за кириллических
# факторов
mutch <- function(t){
    t1 <- c("Зав", "Лен", "Мос", "Окт", "Пар", "Пер", "Сов", "Фр", "Цен", "кирпичный", "монолитный", "панельный", "новостройка", "стандартный проект", "улучшеный проект", "хороший ремонт", "без отделки", "(0,100]", "(100,500]", "(500,1e+03]", "(1e+03,2e+03]")
    t2 <- c("Zavodskoy","Leninsky","Moskowsky","Oktyabrsky","Partizansky","Pervomaisky","Sovetsky","Frunzensky","Centralny","brick","solid","panel","new building","standard","advanced","Yes","No","(0,100]","(100,500]","(500,1000]","(1000,2000]")
    t2[which(t1==t, arr.ind = T)]
}
