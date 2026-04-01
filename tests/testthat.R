library(testthat)
library(jaspGraphs)
library(ggplot2)

test_check("jaspGraphs")
testthat::test_local()
snapshot_review()




ggplot(data.frame(x = rnorm(100), y = rnorm(100)), aes(x=x, y=y)) +
  geom_point()+
  jaspGraphs::themeJaspRaw() +
  jaspGraphs::geom_rangeframe(sides = "tl") +
  jaspGraphs::scale_x_continuous(position = "top")



# check if the pretty stuff is done manually in the full-on figures



