# Website 1 Data
website1 <- c(10357, 10537, 10767, 10561, 10544, 10581, 10602, 10665, 10335, 
              10419, 10737, 10410, 10485, 10601, 10458, 10472, 10435, 10375, 
              10436, 10510, 10345, 10559, 10520, 10425, 10351, 10465, 10491, 
              10671, 10366, 10440, 10618, 10606, 10406, 10538, 10449, 10462)

# Website 2 Data
website2 <- c(11067, 11029, 10888, 10789, 10914, 10663, 10787, 11140, 11042, 
              11074, 10868, 10853, 10900, 11088, 10991, 10928, 10959, 11126, 
              11033, 11114, 11150, 11155, 11027, 10900, 11015, 11123, 10953, 
              11181, 10855, 10731, 10971, 10770, 11070, 11122, 11018, 10903)


#Website 1 question 1
sd(website1)
mean(website1)

#website 2 question 1
sd(website2)
mean(website2)


#question 2
t.test(website1, website2, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE)


#question 3 
#website 1
1-pnorm(12000,10499.97,110.2399/sqrt(36))
1-pnorm(12000, mean(website1), sd(website1)/ sqrt(36))

#website 2
1-pnorm(12000,10977.69, 132.618/sqrt(36))
1-pnorm(12000, mean(website2), sd(website2)/sqrt(36))



#question 4
var(website1)
var(website2)
test1<- var(website1)/var(website2)
test1
2* pf(test1, 36-1,36-1, lower.tail= FALSE)




psych::describe(website1)
