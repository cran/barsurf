### R code from vignette source 'barsurf.Rnw'

###################################################
### code chunk number 1: barsurf.Rnw:37-42
###################################################
options(continue="  ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 1.35, 1.6), cex=0.7, cex.main=1)))

set.seed (1)


###################################################
### code chunk number 2: barsurf.Rnw:97-99
###################################################
library (barsurf)
library (misc3d)


###################################################
### code chunk number 3: barsurf.Rnw:106-107
###################################################
set.bs.options (rendering.style="pdf", theme="blue")


###################################################
### code chunk number 4: barsurf.Rnw:123-125
###################################################
n <- 10
p <- 0.5


###################################################
### code chunk number 5: barsurf.Rnw:128-132
###################################################
x <- y <- 0:10
f <- function (x, y, n, p)
    dbinom (x, n, p) * dbinom (y, n, p)
fv <- outer (x, y, f, n, p)


###################################################
### code chunk number 6: barsurf.Rnw:137-138
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_dfield (x, y, fv)


###################################################
### code chunk number 7: barsurf.Rnw:141-142
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_bar (,,fv)


###################################################
### code chunk number 8: barsurf.Rnw:158-160
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_cfield (rotated.sinc, c (-15.5, 15.5), fb=0, n=40,
    raster=TRUE, hcv=TRUE)


###################################################
### code chunk number 9: barsurf.Rnw:163-164
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_surface (rotated.sinc, c (-15.5, 15.5), n=40)


###################################################
### code chunk number 10: barsurf.Rnw:190-192
###################################################
f <- function (w1, w2, w3 = 1 - w1 - w2)
    (w1 - 1 / 3)^2 + (w2 - 1 / 3)^2 + (w3 - 1 / 3)^2


###################################################
### code chunk number 11: barsurf.Rnw:196-197
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_tricontour (f, xlab="w1", ylab="w2")


###################################################
### code chunk number 12: barsurf.Rnw:220-223
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_isosurface (bispherical.dist,
    c (-3, 3),, c (-2, 2), nsurfaces=3,
    ref.arrows=FALSE, pconstants = c (1, 1, 0) )


###################################################
### code chunk number 13: barsurf.Rnw:242-243
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_cfield3 (bispherical.dist, c (-3, 3),, c (-2, 2), emph="l")


###################################################
### code chunk number 14: barsurf.Rnw:260-261
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_vec (circular.field, c (-1.6, 1.6), c (-1.1, 1.1) )


###################################################
### code chunk number 15: barsurf.Rnw:278-280
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_vec3 (plughole.field, c (-1.5, 1.5),, c (-0.35, 1.3),
    ref.arrows=FALSE)


###################################################
### code chunk number 16: barsurf.Rnw:321-322
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (blue.litmus () )


###################################################
### code chunk number 17: barsurf.Rnw:324-325
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (green.litmus () )


###################################################
### code chunk number 18: barsurf.Rnw:327-328
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (blue.litmus.hcv () )


###################################################
### code chunk number 19: barsurf.Rnw:330-331
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (green.litmus.hcv () )


###################################################
### code chunk number 20: barsurf.Rnw:333-334
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (rainbow.litmus () )


###################################################
### code chunk number 21: barsurf.Rnw:336-337
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (heat.litmus () )


###################################################
### code chunk number 22: barsurf.Rnw:343-344
###################################################
u <- rnorm (30, 4, 1)^3


###################################################
### code chunk number 23: barsurf.Rnw:348-349
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (blue.litmus.fit.hcv (u) )


###################################################
### code chunk number 24: barsurf.Rnw:373-376
###################################################
.colvs <- cbind (c (c (100, 150, 200, 250) ), 35, 75)
rownames (.colvs) <- c ("first color", "(2nd)", "(3rd)", "last color")
colnames (.colvs) <- c ("H", "C", "L")


###################################################
### code chunk number 25: barsurf.Rnw:379-380
###################################################
.colvs


###################################################
### code chunk number 26: barsurf.Rnw:388-390
###################################################
my.litmus <- function (a=0, b=1)
    litmus (a, b, .colvs, color.space="HCL")


###################################################
### code chunk number 27: barsurf.Rnw:394-395
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (my.litmus () )


###################################################
### code chunk number 28: barsurf.Rnw:401-403
###################################################
my.litmus.fit <- function (x, ...)
    litmus.fit (x, .colvs, color.space="HCL", ...)


###################################################
### code chunk number 29: barsurf.Rnw:407-408
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (my.litmus.fit (u) )


###################################################
### code chunk number 30: barsurf.Rnw:433-436
###################################################
x <- y <- seq (-15.5, 15.5, length.out=60)
fv <- outer (x, y, rotated.sinc)
colf <- blue.litmus.fit.hcv (fv)


###################################################
### code chunk number 31: barsurf.Rnw:440-449
###################################################
getOption("SweaveHooks")[["fig"]]()
#larger heatmap
plotf_cfield (rotated.sinc, range (x), n=40,
    contours=FALSE, raster=TRUE, colf=colf)
#smaller heatmap
plotf_cfield (rotated.sinc, c (-4, 4), n=40,
    add=TRUE, contours=FALSE, raster=TRUE, colf=colf)
#contour lines
plot_cfield (x, y, fv,
    add=TRUE, fb=0, heatmap=FALSE)


###################################################
### code chunk number 32: barsurf.Rnw:460-461
###################################################
f <- function (x, y) x + x^3 + y + y^3


###################################################
### code chunk number 33: barsurf.Rnw:465-467
###################################################
tempff <- function (x)
    hot.and.cold.fit (x, t = c (-0.2, 0.2) )


###################################################
### code chunk number 34: barsurf.Rnw:472-476
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_surface (f, c (-1, 1),
    grid.color="grey65",
    gradient.shading=FALSE,
    colff=tempff)


###################################################
### code chunk number 35: barsurf.Rnw:483-486
###################################################
tempff2 <- function (x)
    hot.and.cold.fit (x, t = c (-0.2, 0.2),
    hot.hue=100, cold.hue=60)


