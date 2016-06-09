# СКРИПТ ОБРАБОТКИ ДАННЫХ (ЧЕРНОВОЙ ВАРИАНТ)

# план работ
# 0. малосемейки добавить в стандартные по адресу
# 1. капремонт добавить из текстовых полей
# 2. капремонт сделать фактором менее 5, 5-10 и 10-20, 20+ (или просто да/нет)
# 3. 1-й, последний, не крайний
# 4. построение регрес-х моделей

# есть такое интересное замечание - стабилизация цен. 
# часто (насколько можно проверить - 33%!!!) продавцы меняют цены после размещения объявлления, 
# видимо реагируют на число звонков, интерес к объявлению. Надо рассчитать сколько
# процентов правит цену и через сколько дней в среднем происходят правки.
# В связи с этим естть предложение парсить базу 2 раза - во второй раз парсим через N дней
# и оставляем в качестве финальной базы пересечение баз 1 и 2. в которых останутся только
# объявления прожившие N дней и вероятно стабилизированные

# тут можно спарсить список малосемеек
# http://www.rinet.by/jilaya-nedvijimost#024071

library(xlsx)
library(stringi)
library(ggplot2)

metro <- read.csv("metro.csv", sep=";", numerals = "no.loss")
df <- read.xlsx2("LM2kvabaseall.xlsx", encoding = "UTF-8",sheetName = "baseall")

sapply(df, function(e) sum(e==""))

df$descr <- as.character(df$descr)
df$descr2 <- as.character(df$descr2)
df$primech <- as.character(df$primech)

# summary(df)
# str(df)
# table(df$sobstv)

# для каждой переменной мы должны проделать следующие шаги
# определить тип переменной (числова, фактор, упоряд.фактор)
# при необходимости преобразовать переменную к нужному типу
# понять какие значения может принимать переменная и удалить недопустимые значения либо заменить на NA
# также можно наверное удалить значения с небольшим числом наблюдений или перенести их 
# в ближайший класс (см. предиктор ремонт)
# далее, факторные переменные лучше упорядочить

# в целом для недвижимости можно выделить 2 типа факторов - факторы места и факторы помещения.
# и хотя эти факторы зависимы (например, в дорогом районе не будет малосемеек), но в модели
# должны присутствовать факторы обоих типов

# уберем лишние параметры 
tt <- names(df) %in% c("URL", "Date", "Time1", "Time2", "To", "pricechange", "encoding")
df <- df[,!tt]

# преобразование типов
df$dolg <- as.numeric(as.character(df$dolg))
df$shir <- as.numeric(as.character(df$shir))
df$nday <- as.integer(as.character(df$nday))
df$nweek <- as.integer(as.character(df$nweek))
df$nmonth <- as.integer(as.character(df$nmonth))
df$nphoto <- as.integer(as.character(df$nphoto))

df$price <- as.integer(as.character(df$price))
df$pricem2 <- as.numeric(as.character(df$pricem2))


summary(df$pricem2)
# удаляем строки, в которых стоимость метра NA
df <- df[!is.na(df$pricem2), ]

hist(df$pricem2, breaks = 30)

set.seed(555)
tt <- rbinom(size = 1, n = length(df$pricem2), prob = 0.6)==1
sum(tt)
shapiro.test(df$pricem2[tt])

# Удаляем выбросы по цене м2
mn <- quantile(df$pricem2, probs = c(0.25, 0.75))[1]-abs(1.5*IQR(df$pricem2))
mx <- quantile(df$pricem2, probs = c(0.25, 0.75))[2]+abs(1.5*IQR(df$pricem2))
df <- df[df$pricem2>=mn & df$pricem2<=mx,]

# рассчитываем расстояние до метро
rast <- function(shir1, dolg1, shir2, dolg2){
    kd <- 65394.58
    ks <- 115780.8
    sqrt(ks^2*(shir1-shir2)^2+kd^2*(dolg1-dolg2)^2)
}

mrast <- function(shir1, dolg1){
    mrast <- 5000
    tt <- 0
    for (i in 1:nrow(metro)) {
        tt <- rast(shir1, dolg1, metro[i,2], metro[i,3])
        if(!is.na(tt)){
            if (tt < mrast) {
                mrast <- tt
            }
        }
    }
    mrast
}

for (i in 1:nrow(df)) {
    df$mrasst[i] <- mrast(df$shir[i], df$dolg[i])
}
summary(df$mrasst)
mrasst.f <- cut(x=df$mrasst,breaks = c(0, 500, 1000, 2000, 3000, 20000))
df$mrasst.f <- mrasst.f

# исключаем объекты-комнаты 
table(df$room)
noflats <- grepl("к",df$room)|(as.integer(sub("/\\d", replacement = "", x = df$room))<as.integer(sub("\\d/", replacement = "", x = df$room))) 
df <- df[!noflats & !is.na(noflats),]

# добавляем количество комнат как фактор
df$nroom <- sub("/\\d", replacement = "", x = df$room)
table(df$nroom)
df$nroom <- factor(df$nroom, ordered = T)

# парсим этаж и высоту здания
# этаж
df$nfloor <- sub("(в|н|т)?/.*", replacement = "", x = df$floor)
df$nfloor <- as.integer(df$nfloor) # важно преобразовать в число, чтобы нечисловые значения, были преобразованы в NA

# table(df$nfloor)
# sum(is.na(df$nfloor))

df$nfloormax <- sub(".*/(\\d+).*", replacement = "\\1", x = df$floor)
df$nfloormax <- as.integer(df$nfloormax) # важно преобразовать в число, чтобы нечисловые значения, были преобразованы в NA

# этаж надо рассматривать как фактор 1-й, 2-3, 4+, last
df$nfloor.f <- as.character(df$nfloor)

df$nfloor.f[is.na(df$nfloor)] <- "4+" # NA (50 шт) заменил на компромиссный вариант
df$nfloor.f[df$nfloor==1]="1"
df$nfloor.f[df$nfloor<=3 & df$nfloor>=2]="2-3"
df$nfloor.f[df$nfloor>=4]="4+"
df$nfloor.f[df$nfloor==df$nfloormax]="last"
table(df$nfloor.f)   
df[df$nfloor.f=="0","nfloor.f"] <- "1"
df$nfloor.f <- factor(df$nfloor.f, levels = c("1", "2-3", "4+", "last"))

# table(df$nfloormax)
# sum(is.na(df$nfloormax)) # смотрим, чтобы не было много NA

# исключить дома - ищем для начала по этажности 1/1
ishouse <- (df$nfloormax==1)&(!is.na(df$nfloormax)) # удалил 24 дома
df <- df[!ishouse, ]

# процесс сбора был организован в 2 этапа, и на 2-м этапе часть объектов (3-4%) были удалены из базы
# пользователями, поэтому есть записи, не содержащие информации 2-го этапа, мы эти записи удалим
isnofull <- df$metro=="DELETED"&(!is.na(df$metro))
sum(isnofull)
sum(is.na(isnofull))
df <- df[!isnofull,]
df[is.na(df$ID),]


# Район -------------------------------------------------------------------

# район важный фактор. у нас есть район города (11 уровней) и район+микрорайон (86 уровней).
# наверное пока остановимся на фактор район (без м.р.)
# проверяем наличие странных значений и заменяем их на NA
table(df$district)
for (i in 1:nrow(df)) {
    if (!is.na(df$district[i])) {
        if(df$district[i]=="" | df$district[i]=="Пр."){
            df$district[i] <- NA
        }
    }
}

sum(is.na(df$district))
df$district <- factor(df$district, levels = c("Зав", "Лен", "Мос", "Окт", "Пар", "Пер", "Сов", "Фр", "Цен"))

# адрес - пока не будем использовать
table(df$address)

# площадь добавим
df$area1 <- sub("^(.*?)/(.*?)/(.*)$", replacement = "\\1", x = df$area)
df$area2 <- sub("^(.*?)/(.*?)/(.*)$", replacement = "\\2", x = df$area)
df$area3 <- sub("^(.*?)/(.*?)/(.*)$", replacement = "\\3", x = df$area)

df$area1 <- sub(",", replacement = ".", x = df$area1)
df$area2 <- sub(",", replacement = ".", x = df$area2)
df$area3 <- sub(",", replacement = ".", x = df$area3)

df$area1 <- as.numeric(df$area1) # важно преобразовать в число, чтобы нечисловые значения, были преобразованы в NA
df$area2 <- as.numeric(df$area2)
df$area3 <- as.numeric(df$area3)

sum(is.na(df$area1))
sum(is.na(df$area2))
sum(is.na(df$area3))


# Возраст и капремонт -----------------------------------------------------
# из года постройки надо выделить возраст  

old <- sub("^(\\d{4})([ ]{1,2})?(\\d{4})?$", replacement = "\\1", x = df$year)
old <- as.integer(old)
oldcap <- sub("^(\\d{4})([ ]{1,2})?(\\d{4})?$", replacement = "\\3", x = df$year)
oldcap <- as.integer(oldcap)
# т.к. при заполнении продавцы часто ставили вместо года постройки 
# год капремонта, то выбирать год постройки будем как минимальное значение из этой пары
year1 <- integer(length = nrow(df))
year2 <- integer(length = nrow(df))
for (i in 1:length(old)) {
    year1[i] <- min(old[i],oldcap[i], na.rm = T) # год постройки 
    year2[i] <- max(old[i],oldcap[i])  # год капремонта
}
year1[year1==Inf] <- NA
# видел значения равные 0, надо их заменить на NA и вообще все "старше" 1900 заменить на NA
year1[year1<1900]<- NA

year2[year2<1900]<- NA

vozr <- 2016-year1
# summary(vozr) 
vozrcap <- 2016-year2
df$vozr <- vozr

# summary(vozrcap)  - 
# саммари позволяет найти странные значения, например для капремонта, я 
# нашел что люди пишут год будущего капремонта 2017, 2018 и т.д., учитывая падающие бюджеты и 
# урезанные списки работ по капремонту такие "будущие" капремонты можно не учитывать
# т.е. учитывая, что сейчас начало 2016 все капремонты от 2016 и выше обнуляем.
year2[year2>=2016] <- NA
vozrcap <- 2016-year2
df$vozrcap <- vozrcap
table(df$vozrcap)

## выделим капремонт из текстовых
kaprem <- grepl("(капремонт|кап(.){1-3}|капитальн(ый|ого)( )?ремонт(а)?)", paste(df$descr, df$descr2, df$primech))
sum(kaprem & is.na(df$vozrcap))
head(df[kaprem & is.na(df$vozrcap),c("descr","descr2","primech")])
hist(df$vozrcap, breaks = 30)

# глядя на распредение лет после капремонта, решено сделать фактор - до 5, 5-10, 10+, нет
df$vozrcap <- as.character(df$vozrcap)
df$vozrcap[df$vozrcap<=5 & !is.na(df$vozrcap)] <- "до 5"
df$vozrcap[df$vozrcap>5 & df$vozrcap<=10 & !is.na(df$vozrcap)] <- "5-10"
df$vozrcap[df$vozrcap>10 & !is.na(df$vozrcap)] <- "10+"

df[kaprem & is.na(df$vozrcap), "vozrcap"] <- "до 5" # считаем что если продавец пишет, "дом после капремонта" значит с того момента прошло не более 5 лет
df[is.na(df$vozrcap),"vozrcap"] <- "нет"

df$vozrcap <- factor(df$vozrcap, levels = c("до 5", "5-10", "10+", "нет"), ordered = T)

# балконы
# для балкона важно в первую очередь его наличие (0,1,2) и застеклен/незастеклен - это легко получить
# из имеющейся информации. У нас в данных есть 475 пустых значений. т.е. владельцы просто не заполнили
# информацию о балконе, можем ли мы считать, что у них нет балкона? наверное так было бы неверно, ведь
# они просто могли забыть заполнить эту информацию. нужно для проверки глянуть текстовые поля c описанием
# на наличие слов-маркеров "балкон", "лоджия" и др.

table(df$balcony)
table(df$balkon)
nbalkon <- integer(length = nrow(df))
nbalkon <- rep.int(-1,length(nbalkon))
nbalkon[grepl(x = df$balcony, pattern = "-")] <- 0
nbalkon[grepl(x = df$balcony, pattern = "2")] <- 2
nbalkon[grepl(x = df$balcony, pattern = "3")] <- 3
nbalkon[df$balcony==""] <- NA
nbalkon[nbalkon==-1] <- 1
df$nbalkon <- nbalkon

vidbalkon <- character(length = nrow(df))
vidbalkon[grepl(x = df$balcony, pattern = "з")] <- "заст"
vidbalkon[vidbalkon!="заст"] <- "незаст"
df$vidbalkon <- factor(vidbalkon)

# тип дома
table(df$tipdoma)
# попытка вытащить тип-дома из тектовых полей не удалась, лишь 2 объекта уточнились
tt <- grepl(x = paste(df[df$tipdoma=="",]$descr,df[df$tipdoma=="",]$descr2, df[df$tipdoma=="",]$primech), pattern = "(кирпич|монолит|панельный)")
sum(tt)


# пустые значения заменяем на NA
tt <- df$tipdoma
tt[df$tipdoma==""]<-NA
df$tipdoma <- tt  
    
# значение "кар" заменим на "каркасно-блочный"
df[df$tipdoma=="кар" & !is.na(df$tipdoma),"tipdoma"] <- "каркасно-блочный"

df$tipdoma <- factor(df$tipdoma, levels =  c("блок-комнаты", "каркасно-блочный", "кирпичный", "мк", "монолитный", "панельный", "силикатные блоки"))


# Планировка --------------------------------------------------------------

# восстанавливаем планировку по адресу
## малосемейки
malosem <- df[df$planirovka=="малосемейка"&!is.na(df$planirovka), "address"]
length(malosem)
addmalosem <- (df$address %in% df$address[malosem])&(is.na(df$planirovka))
sum(addmalosem)
df$planirovka[addmalosem] <- "малосемейка"
sum(is.na(df$planirovka))

## брежневки
brezhn <- df[df$planirovka=="брежневка"&!is.na(df$planirovka), "address"]
addbrezh <- (df$address %in% df$address[brezhn])&(is.na(df$planirovka))
df$planirovka[addbrezh] <- "брежневка"

## сталинки
stal <- df[df$planirovka=="сталинка"&!is.na(df$planirovka)&grepl(df$address,pattern = "\\d+"), "address"]
addstal <- (df$address %in% df$address[stal])&(is.na(df$planirovka))
df$planirovka[addstal] <- "сталинка"

## соврем.
sovr <- df[df$planirovka=="соврем."&!is.na(df$planirovka)&grepl(df$address,pattern = "\\d+"), "address"]
addsovr <- (df$address %in% df$address[sovr])&(is.na(df$planirovka))
df$planirovka[addsovr] <- "соврем."

## стандартный проект
stan <- df[df$planirovka=="стандартный проект"&!is.na(df$planirovka)&grepl(df$address,pattern = "\\d+"), "address"]
addstan <- (df$address %in% df$address[stan])&(is.na(df$planirovka))
df$planirovka[addstan] <- "стандартный проект"

## улучшеный проект
ulu <- df[df$planirovka=="улучшеный проект"&!is.na(df$planirovka)&grepl(df$address,pattern = "\\d+"), "address"]
addulu <- (df$address %in% df$address[ulu])&(is.na(df$planirovka))
df$planirovka[addulu] <- "улучшеный проект"

## хрущевка
tt <- df[df$planirovka=="хрущевка"&!is.na(df$planirovka)&grepl(df$address,pattern = "\\d+"), "address"]
addtt <- (df$address %in% df$address[tt])&(is.na(df$planirovka))
df$planirovka[addtt] <- "хрущевка"

## чешский проект
tt <- df[df$planirovka=="чешский проект"&!is.na(df$planirovka)&grepl(df$address,pattern = "\\d+"), "address"]
addtt <- (df$address %in% df$address[tt])&(is.na(df$planirovka))
df$planirovka[addtt] <- "чешский проект"

# мы обнаружили, что люди один и тот же дом пишут под разной планировкой, малосемейка или
# стандартный проект

c(1,2) %in% c(1,2,3)

windows(width=20, height=20)
gplots::plotmeans(pricem2~planirovka, data = df)

# пустые значения заменяем на NA
tt <- df$planirovka
tt[df$planirovka==""]<-NA
df$planirovka <- tt 

# значение "бреж" заменим на "брежневка"
df[df$planirovka=="бреж" & !is.na(df$planirovka),"planirovka"] <- "брежневка"
df[df$planirovka=="стал" & !is.na(df$planirovka),"planirovka"] <- "сталинка"
df[df$planirovka=="чеш." & !is.na(df$planirovka),"planirovka"] <- "чешский проект"
df[df$planirovka=="хрущ" & !is.na(df$planirovka),"planirovka"] <- "хрущевка"

df$planirovka <- factor(df$planirovka, levels = c("брежневка", "малосемейка", "новостройка", 
                                                  "соврем.", "сталинка", "стандартный проект", 
                                                  "улучшеный проект", "хрущевка", "чешский проект"))

# отдельно поищем "малосемейки" в текстовых полях
tt <- grepl(x = paste(df$descr,df$descr2, df$primech, df$dopoln), pattern = "(малосемейка|малосемейке|маласемейка)")
# тут проблема в том, что мы не учитываем записи "не малосемейка", и т.о. вносим ошибки
# поэтому следующий код не выполняем
# df$planirovka[tt&is.na(df$planirovka)] <- "малосемейка"
# df[tt&is.na(df$planirovka),]
# sum(tt&is.na(df$planirovka))

table(df$planirovka)
levels(df$planirovka)

# список адресов с малосемейками
# tt <- unique(df[df$planirovka=="малосемейка" & !is.na(df$planirovka),"address"])
# tt <- tt[order(tt)]
# writeLines(text =as.character(tt), con = "малосемейки2.txt")
# расширенный список малосемеек
tt2 <- readLines(con = "малосемейки.txt", encoding = "UTF-8")

# обновляю планировку для NA по значению адреса
addmalosem2 <- (df$address %in% tt2)&(is.na(df$planirovka))
sum(addmalosem2)
df$planirovka[addmalosem2] <- "малосемейка"


# РЕМОНТ ------------------------------------------------------------------

# ремонт
table(df$remont)
df[df$remont=="плохое состояние", c("descr", "descr2", "primech", "ID")]
# со значением "плохое состояние" всего 17 объектов. видимо люди, склонны завышать уровень ремонта
# может закинуть их в уд.ремонт?
fit <- aov(pricem2~remont, data = df)
tt2 <- TukeyHSD(fit)
print(tt2)
plot(tt2)
ggplot(data = df, aes(x=remont, y=pricem2)) + geom_boxplot()

sum(is.na(df$remont))


# дисперсионный анализ с помощью функции aov показал, что
# 1. в связи с малым кол-вом оъектов значение "плохое состояние" не дает стат.устойчивого результата
# однако в дальшейшем в текстовых полях мы нашли еще 19 квартир (с пустым полем ремонт), для которых
# указано, что они требуют ремонта
library(gplots)
windows(width=20, height=20)
gplots::plotmeans(pricem2~remont, data = df)

# 2. средние в группах удовлетворительный ремонт и нормальный ремонт статистически значимо не различаются
# видимо, продавцы  (как и я) не видят разницы в этих в этих понятиях 
# поэтому объединим группы удов. и норм. ремонт в одну группу "удовлетворительный ремонт"


df$remont <- as.character(df$remont)

df[df$remont=="плохое состояние", "remont"] <- "без ремонта" 
df[df$remont=="нормальный ремонт", "remont"] <- "удовлетворительный ремонт"

tt <- df$remont==""
df$remont[tt] <- NA

df$remont <- factor(df$remont, levels = c("без отделки", "строительная отделка", "без ремонта", 
                                          "удовлетворительный ремонт", "хороший ремонт",
                                          "отличный ремонт", "евроремонт" ), ordered = T)

# попробуем вытащить ремонт из текстовых полей 
sum(is.na(df$descr[tt]))
tt <- is.na(df$remont)
    tt2 <- grepl("([а-яА-Я]+( )+)?ремонт( )?([а-яА-Я]+)?", paste(df$descr[tt], df$descr2[tt], df$primech[tt]))
    tt[tt==T] <- tt2 
    tt3 <- stri_extract_first(regex = "([а-яА-Я]+)?(.{0,3})?([а-яА-Я]+)?(.{0,3})?ремонт(.{0,3})?([а-яА-Я]+)?([а-яА-Я]+)?(.{0,3})?", str =  paste(df$descr[tt], df$descr2[tt], df$primech[tt]))

# выделим "хороший ремонт"

horrem <- grepl("((Х|х)ороши(й|м)|(с|С)овременны(й|м)|(с|С)вежий|(Н|н)овый
                |(Д|д)обротны(й|м)|(Д|д)орог(ой|им))( )+(ремонт(ом)?)", paste(df$descr[tt], df$descr2[tt], df$primech[tt]))
sum(horrem)

# выделим отличный ремонт
otlrem <- grepl("((О|о)тличны(й|м)|(П|п)ревосходны(й|м)|(д|Д)изайнерски(й|м))( )+(ремонт(ом)?)", paste(df$descr[tt], df$descr2[tt], df$primech[tt]))
sum(otlrem)

# выделим удовлетворительный ремонт
udrem <- grepl("((У|у)довлетв(о|а)рительны(й|м)|(К|к)осметически(й|м)|(К|к)ачественны(й|м)
               |(О|о)бычны(й|м)|(Ч|ч)астичны(й|м)|(А|а)ккуратны(й|м))( )+(ремонт(ом)?)", paste(df$descr[tt], df$descr2[tt], df$primech[tt]))
sum(udrem)

# выделим требуется ремонт (без ремонта)
trebrem <- grepl("((Т|т)ребует(ся)?|(Б|б)ез)( )+(ремонт(а)?)", paste(df$descr[tt], df$descr2[tt], df$primech[tt]))
sum(trebrem)

df[tt,"remont"][horrem] <- "хороший ремонт"
df[tt,"remont"][otlrem] <- "отличный ремонт"
df[tt,"remont"][udrem] <- "удовлетворительный ремонт"
df[tt,"remont"][trebrem] <- "без ремонта"

table(df$remont)
sum(is.na(df$remont))





