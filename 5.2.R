#Пакет для работы с алгоримами машинного обучения
install.packages("caret")
install.packages("ellipse")
install.packages("e1071")
install.packages("randomForest")



library(caret)

##1.
#Загрузим данные
#Загрузим встроенный датасет ?iris
data(iris)
# и созраним ссылку на него в переменную
dataset <- iris

##2.
#Изучим устройство данных
summary(dataset)
# измерения
dim(dataset)
# список типов каждого атрибута
sapply(dataset, class)
# "взглянем" На данные
head(dataset)
# просмотрим уровни классификатора
levels(dataset$Species)

##2! Отрежем часть данных для последующей валидации 
# поллучим 80% данных
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# выберем  20% для валидации
validation <- dataset[-validation_index,]
# 80% у нас будут исходные данные
dataset <- dataset[validation_index,]

## 3. Исследуем и визуализируем даные
#Распределение видов Ирисов в данныом наборе
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

#Разобьём данные на переменные и отклик (х и y)
x <- dataset[,1:4]
y <- dataset[,5]

#Выборки будем визуализировать диаграммой размаха, как обычно
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}
plot(y)

#Посмотрим взаимодействие внутри данных
featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")

##4. Анализ
# Будем запускать все алгоримы и проерять кроссвалидацией(cv)
#через 10 блоков
control <- trainControl(method="cv", number=10)
# Контролируема метрика
metric <- "Accuracy"
#линейные алгоритмы 
#LDA http://www.machinelearning.ru/wiki/index.php?title=LDA
set.seed(13)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
#нелинейные алгоритмы
# CART https://basegroup.ru/community/articles/math-cart-part1
set.seed(13)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN https://wiki.loginom.ru/articles/k-nearest-neighbor.html
# https://basegroup.ru/community/articles/knn
set.seed(13)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
#Сложные алгоритмы
# SVM http://www.machinelearning.ru/wiki/index.php?title=%D0%9C%D0%B5%D1%82%D0%BE%D0%B4_%D0%BE%D0%BF%D0%BE%D1%80%D0%BD%D1%8B%D1%85_%D0%B2%D0%B5%D0%BA%D1%82%D0%BE%D1%80%D0%BE%D0%B2
set.seed(13)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
#RandomForest
# https://dyakonov.org/2016/11/14/%D1%81%D0%BB%D1%83%D1%87%D0%B0%D0%B9%D0%BD%D1%8B%D0%B9-%D0%BB%D0%B5%D1%81-random-forest/
# https://medium.com/@williamkoehrsen/random-forest-simple-explanation-377895a60d2d
set.seed(13)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

#Получим оценки конролируемой метрики для каждого алгоритма
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
#визуализируем
dotplot(results)
#лучше всех LDA
print(fit.lda)
#Её и будем использовать
#для этого у нас подготовлен валидационный набор
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)

