library(tidyverse)
pm <- read_csv("Z:/paper/working/jan_16.csv")


palet <- read.csv("Z:/paper/working/palet.csv")

palet <- na.omit(palet)


library(tidyverse)

weigth <- palet %>% select(c(1,2,3,4)) %>% 
  mutate(weight_palet = length * breath * gram *Packet.per.pallete/1406/2.2046/5) %>% 
  select(5) %>% unique() %>% na.omit



##For combination of pallets, use the knapsack probel algorithms, 
# link : https://www.r-bloggers.com/the-knapsack-problem/

appetizer.solution <- local (
  function (target) {
      app <- weigth[c(1:nrow(weigth)),]
        r <- 5L
    repeat {
      c <- gtools::combinations(length(app), r=r, v=app, repeats.allowed=TRUE)
      s <- rowSums(c)
      if ( all(s > target) ) {
        print("No solution found")
        break
      }
      x <- which( abs(s-target) < 2 )
      if ( length(x) > 0L ) {
        cat("Solution found: ", c[x,], "\n")
        break
      }
      r <- r + 1L
    }
  })
