context("hello")

test_that("hello function",{
  expect_match(hello(""),"hello!")
  expect_match(hello(123),"hello 123!")
  expect_match(hello("yo momma"),"hello yo momma!")
})

context("slr")
set.seed(240193)
x1 <- 1:100
y1 <- 0.5*(x1 + rnorm(100))
x2 <- 1:100
y2 <- -0.5*(x2 + rnorm(100)) + 50
dt1 <- data.table(x=c(x1,x2),y=c(y1,y2),z=rep(1:2,each=100))
dt2 <- data.table(z = c(1,2),
                  b0 = c(-0.02225037,49.90457869),
                  b1 = c(0.5001625,-0.4986353),
                  r2 = c(0.9987674,0.9988036),
                  n = c(100,100))

test_that("slr",{
  expect_equal(dim(slr(dt1,"x","y","z")),c(2,5))
  expect_true(all.equal(slr(dt1,"x","y","z"),dt2,tolerance = 0.0001))
})

set.seed(240193)
n=10000
dt1 <- data.table(x=runif(1,-3,3),
                  y = runif(10000,-3,3),
                  z = 1)

dt2 <- data.table(x = rnorm(10000),
                  y = rnorm(10000),
                  z = 2)

dt3 <- data.table(x=seq(-3,3,length.out = 10000))
dt3[,":="(y=0.5*x+rnorm(10000,sd=0.5),
          z=3),]

dt4 <- data.table(x = c(rnorm(5000,-1,.75),rnorm(5000,1,.75)),
                  y = c(rnorm(5000,1,.75),rnorm(5000,-1,.75)),
                  z = 4)

dtTest <- rbindlist(list(dt1,dt2,dt3,dt4))


dt5 <- data.table(x = seq(0,5,length.out = 10000),
                  y = seq(0,5,length.out = 10000))

dt5[,":="(x = x + rnorm(10000,sd=0.5),
          y = y + rnorm(10000,sd=0.5),
          z = 5)]

slr(rbindlist(list(dt1,dt2,dt3,dt4)),"x","y","z")
slr(rbindlist(list(dt1,dt2,dt3,dt4)),"x","y","z")



hist2d(dtTest,facet = 1,z="z",widthBin = 0.1)
hist2d(dtTest,facet = 1,z="z",widthBin = 0.1,hasLine=T)


hist2d(data.table(x=runif(10000),
                  y=runif(10000)))

hist2d(data.table(x=runif(10000)+0.1,
                  y=runif(10000)+0.1))

hist2d(data.table(x=rnorm(10000),
                  y=rnorm(10000)),widthBin = .1)

hist2d(rbindlist(list(dt4,dt5)), widthBin = .1) + coord_equal()

x <- 1:1000
y <- x + rnorm(1000,sd=10)
hist2d(data.table(x,y))

# test_that("plot",{
#   vdiffr::expect_doppelganger("Base graphics histogram", hist(mtcars$disp))
# })


