


B.fixed <- data.frame(coef = c("a", "b", "c"), mean = c(1, 0.5, 2))
B.huc <- data.frame(huc = as.factor(rep(1:10, 3)),  coef = rep(c("d", "e", "f"), each = 10), mean = c(rnorm(10, 2), rnorm(10, 10), rnorm(10, 1, 0.2)))
B.site <- data.frame(site = as.factor(rep(1:100, 3)),  coef = rep(c("d", "e", "f"), each = 100), mean = c(rnorm(100, 1), rnorm(100, 1, 0.5), rnorm(100, 0.5, 0.1)))
B.year <- data.frame(year = as.factor(rep(2001:2010, 4)),  coef = rep(c("g", "h", "i", "j"), each = 10), mean = c(rnorm(10, 0.5), rnorm(10, 0.2, 0.2), rnorm(10, 1, 0.2), rnorm(10, 2, 0.5)))

data <- data.frame(
  row = 1:1000,
  site = as.factor(rep(1:100, length.out = 1000)),
  huc = as.factor(rep(1:10, length.out = 1000)),
  year = as.factor(rep(2001:2010, each = 100)),
  a = rnorm(1000, 0),
  b = rnorm(1000, 0),
  c = rnorm(1000, 0),
  d = rnorm(1000, 0),
  e = rnorm(1000, 0),
  f = rnorm(1000, 0),
  g = rnorm(1000, 0),
  h = rnorm(1000, 0),
  i = rnorm(1000, 0),
  j = rnorm(1000, 0)
)





