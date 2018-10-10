library(gapminder)
#install.packages('tidyverse')
library(dplyr)
#Функция фильтр
filter(gapminder, lifeExp < 29)
filter(gapminder, country == "Afghanistan", year > 1981)
filter(gapminder, continent %in% c("Asia", "Africa"))
#Тоже самое для векторов
gapminder[gapminder$lifeExp < 29, ]
subset(gapminder, country == "Rwanda")



head(gapminder)
gapminder %>% head(3)


head(select(gapminder, year, lifeExp),4)
#Ниже то же самое, но с пайпом
gapminder %>%
  select(year, lifeExp) %>%
  head(4)

gapminder %>%
  filter(country == "Cambodia") %>%
  select(year, lifeExp)
#Ниже то же самое
gapminder[gapminder$country == "Cambodia", c("year", "lifeExp")]

#Для демонстрации следующих функций загрузим другой датасет
msleep <- read.csv("https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv")
head(msleep)
#Упорядочить по одной колонке
msleep %>% arrange(order) %>% head
#По нескольким
msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  head
#Отфильтруем и отсортируем по убыванию
msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  filter(sleep_total >= 16)

#Добавление колонок
msleep %>%
  select(name, sleep_rem, sleep_total) %>% 
  mutate(rem_proportion = sleep_rem / sleep_total) %>%
  head
#Получение итогов
msleep %>% 
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total),
            total = n())

msleep %>% 
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())
