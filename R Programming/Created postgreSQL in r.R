library(RPostgreSQL)
library(tidyverse)

# [1] Connect database

con <-dbConnect(PostgreSQL(),
          host = "arjuna.db.elephantsql.com",
          port = 5432, ## default
          user = "cgwlfhrz",
          password = "********",
          dbname = "cgwlfhrz")

dbListTables(con)

rest_menu <-data.frame(
  id =1:10,
  menu_name = c("Cream Pumpkin Soup","Greek Salad",
                "Chilli Pasta","Carbonara",
                "'Mac and Cheese","Lamb Chops",
                "Bacon Mashed Potato","Gorgonzola Lamb Chops",
                "Tomato Caprese Soup","Grillted Steak Salad"),
  price=c(290,220,290,250,220,
          490,320,590,350,320))

dbWriteTable(con,"main_menu",rest_menu)

## recheck table in con database
dbListTables(con)

## check column name in menu table in con database
dbListFields(con,"main_menu")

# [2] get data

dbGetQuery(con,"select * from main_menu")

## create another table
customers <- data.frame(id=1:10,
                        name=c("michael jordan","lebron james",
                               "luka doncic","jayson tatum",
                               "jimmy butler","dwight howard",
                               "jeremy lin","klutch sports group",
                               "excel sports management",
                               "aces"),
                        ages = c(60,38,24,25,33,
                                 37,34,NA,NA,NA))

dbWriteTable(con, "customers", customers)

dbListTables(con)

## get query from second table

dbGetQuery(con,"select * from customers where ages >= 30")

## remove table

dbRemoveTable(con,"main_menu")
dbRemoveTable(con,"customers")

dbListTables(con)

# close connection?
dbDisconnect(con)
