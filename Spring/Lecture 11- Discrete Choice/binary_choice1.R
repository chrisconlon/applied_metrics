
## This is the Lewbel Dong Yang (2012) Example
draw_sample = function(){
  tibble(
  r = c(-1.8, -0.9, -0.92,-2.1, -1.92, 10),
  treated = c(0,0,0,1,1,1),
  true_te = c(0,0,0,0,1,0),
  error = rnorm(6),
  D = c(0,1,1,0,1,1)
  )  %>% mutate(y = (r + true_te * treated + error)>0)
}

summary(lm(D~ treated+r, data=df))

# Now with 1000x more data
for (n in 1:1000){data[[n]]=draw_sample()}
df<-bind_rows(data)
summary(lm(y~ treated+r, data=df))



