library(MASS)
data(birthwt)
summary(birthwt)

help(birthwt)

colnames(birthwt)

colnames(birthwt) <- c("birthwt.below.2500", "mother.age", 
                       "mother.weight", "race",
                       "mother.smokes", "previous.prem.labor", 
                       "hypertension", "uterine.irr",
                       "doctor.visits", "birthwt.grams")

  birthwt$race <- factor(c("white", "black", "other")[birthwt$race])
birthwt$mother.smokes <- factor(c("No", "Yes")[birthwt$mother.smokes + 1])
birthwt$uterine.irr <- factor(c("No", "Yes")[birthwt$uterine.irr + 1])
birthwt$hypertension <- factor(c("No", "Yes")[birthwt$hypertension + 1])

summary(birthwt)

plot (birthwt$race)
title (main = "Расовый состав рожениц в Springfield MA, 1986")

plot (birthwt$mother.age)
title (main = "Возраст рожениц в Springfield MA, 1986")

plot (sort(birthwt$mother.age))
title (main = "(Отсортировано) Возраст рожениц в Springfield MA, 1986")
  
plot (birthwt$mother.age, birthwt$birthwt.grams)
title (main = "Вес новорождённого в разрезе возраста роженицы")

plot (birthwt$mother.smokes, birthwt$birthwt.grams, 
      main="Вес новорождённого в разрезе курительной зависимости", 
      ylab = "Вес (г)", xlab="Зависимость")

t.test (birthwt$birthwt.grams[birthwt$mother.smokes == "Yes"], 
        birthwt$birthwt.grams[birthwt$mother.smokes == "No"])

linear.model.1 <- lm (birthwt.grams ~ mother.smokes, data=birthwt)
linear.model.1
summary(linear.model.1)

linear.model.2 <- lm (birthwt.grams ~ mother.age, data=birthwt)
linear.model.2
summary(linear.model.1)

plot(linear.model.2)


birthwt.noout <- birthwt[birthwt$mother.age <= 40,]
linear.model.3 <- lm (birthwt.grams ~ mother.age, data=birthwt.noout)
linear.model.3
summary(linear.model.3)
#plot(birthwt$mother.age, birthwt$birthwt.grams)
#abline(linear.model.3)

linear.model.3a <- lm (birthwt.grams ~ + mother.smokes + mother.age, data=birthwt.noout)
summary(linear.model.3a)

plot(linear.model.3a)

linear.model.4 <- lm (birthwt.grams ~ ., data=birthwt.noout)
linear.model.4

linear.model.4a <- lm (birthwt.grams ~ . - birthwt.below.2500, data=birthwt.noout)
summary(linear.model.4a)
plot(linear.model.4a)

glm.0 <- glm (birthwt.below.2500 ~ . - birthwt.grams, data=birthwt.noout)
plot(glm.0)

glm.1 <- glm (birthwt.below.2500 ~ . - birthwt.grams, data=birthwt.noout, family=binomial(link=logit))
summary(glm.1)
plot(glm.1)

odds <- seq(1, nrow(birthwt.noout), by=2)
birthwt.in <- birthwt.noout[odds,]
birthwt.out <- birthwt.noout[-odds,]

linear.model.half <- lm (birthwt.grams ~ . - birthwt.below.2500, data=birthwt.in)
summary (linear.model.half)

birthwt.predict <- predict (linear.model.half)
cor (birthwt.in$birthwt.grams, birthwt.predict)
plot (birthwt.in$birthwt.grams, birthwt.predict)

birthwt.predict.out <- predict (linear.model.half, birthwt.out)
cor (birthwt.out$birthwt.grams, birthwt.predict.out)
plot (birthwt.out$birthwt.grams, birthwt.predict.out)
