rm(list = ls()) #clear the workspace

library(tidyverse)
library(stargazer)
library(magrittr)
library(haven)

#Housing price regression through DID-----------------------------------------------------

KIELMC <- read_csv("KIELMC.csv")

select(KIELMC, rprice, nearinc, y81, y81nrinc)

select(KIELMC, rprice, nearinc, y81, y81nrinc) %>% 
  as.data.frame() %>% 
  stargazer(type = "text")

KIELMC %>% 
  group_by(nearinc, y81) %>% 
  summarize_at(.vars = vars(rprice),
               .funs = list(mean = mean, obs = length))

#method1----------------------------------------------------------
model1 <- lm(rprioce ~ nearinc, KIELMC, subset = (year == 1981))
summary(model1)
(b1 <- coef(model1)["nearinc"])

model2 <- lm(rprioce ~ nearinc, KIELMC, subset = (year == 1978))
summary(model2)
(b2 <- coef(model2)["nearinc"])

b1 - b2

#method2----------------------------------------------------------
model3 <- lm(rprioce ~ y81, KIELMC, subset = (nearinc == 1))
summary(model3)
(b3 <- coef(model3)["y81"])

model4 <- update(model3, subset = (nearinc == 0))
summary(model4)
(b4 <- coef(model4)["y81"])

b3 - b4

#method3------------------------------------------------------------
model5 <- lm(rprice ~ nearinc + y81 + y81nrinc, KIELMC)
summary(model5)
coef(model5)["y81nrinc"]
