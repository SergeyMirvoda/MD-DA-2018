#Дисперсионный анализ. Пример

#Загрузим данные (требуется установить Рабочую папку с помощью setwd) или указать полный путь
data = read.csv("data/diet.csv",row.names=1)
summary(data)
#Ознакомимся со структурой и переименуем колонки, как нам удобно
#https://www.sheffield.ac.uk/polopoly_fs/1.547015!/file/Diet_data_description.docx
#https://www.sheffield.ac.uk/mash/data
colnames(data) <- c("gender", "age", "height", "initial.weight", 
                    "diet.type", "final.weight")
data$diet.type <- factor(c("A", "B", "C")[data$diet.type])
#Добавим новую колонку - Похудение
data$weight.loss = data$initial.weight - data$final.weight
#Проанализиуем есть ли различия по типам диет
boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

#проверим сбалансированные ли данные
table(data$diet.type)

#График групповых средних
library(gplots) #библиотека устанавлевается с помощью install.packages
plotmeans(weight.loss ~ diet.type, data=data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)


#Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm
#тест на межгрупповые различия
fit <- aov(weight.loss ~ diet.type, data=data)
summary(fit)

#попарные различия между средними значениями для всех групп
TukeyHSD(fit)

#Tukey honest significant differences test)
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(diet.type="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")

#Задание
#Добавить проверку на выборы и избавиться от них
#повторно проверсти все тесты и сравнить результаты с выбросами и без
#Открыть документ https://www.sheffield.ac.uk/polopoly_fs/1.547015!/file/Diet_data_description.docx
#и попытаться выполнить задания из него



