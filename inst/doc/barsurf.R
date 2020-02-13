### R code from vignette source 'barsurf.Rnw'

###################################################
### code chunk number 1: barsurf.Rnw:37-40
###################################################
options(continue="  ")
options(SweaveHooks=list(fig=function()
par(mar=c(4.1, 4.1, 1.35, 1.6), cex=0.7, cex.main=1)))


###################################################
### code chunk number 2: barsurf.Rnw:85-87
###################################################
library (barsurf)
library (misc3d)


###################################################
### code chunk number 3: barsurf.Rnw:95-96
###################################################
set.bs.options (rendering.style="e", theme="blue")


###################################################
### code chunk number 4: barsurf.Rnw:116-118
###################################################
n = 10
p = 0.5


###################################################
### code chunk number 5: barsurf.Rnw:121-125
###################################################
x = y = 0:10
f = function (x, y, n, p)
    dbinom (x, n, p) * dbinom (y, n, p)
fv = outer (x, y, f, n, p)


###################################################
### code chunk number 6: barsurf.Rnw:130-131
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_dfield (x, y, fv)


###################################################
### code chunk number 7: barsurf.Rnw:134-135
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_bar (,,fv)


###################################################
### code chunk number 8: barsurf.Rnw:144-146
###################################################
args (plot_dfield)
args (plotf_dfield)


###################################################
### code chunk number 9: barsurf.Rnw:167-168
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_cfield (rotated.sinc, c (-15.5, 15.5), fb=0, n=60, hcv=TRUE)


###################################################
### code chunk number 10: barsurf.Rnw:171-172
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_surface (rotated.sinc, c (-15.5, 15.5), n=40)


###################################################
### code chunk number 11: barsurf.Rnw:198-199
###################################################
M = matrix (sample (1:50), 5, 10)


###################################################
### code chunk number 12: barsurf.Rnw:203-204
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_matrix (,,M)


###################################################
### code chunk number 13: barsurf.Rnw:234-237
###################################################
getOption("SweaveHooks")[["fig"]]()
nested_isosurfaces (bispherical.dist,
    c (-3, 3),, c (-2, 2), c (0.5, 1, 1.75),
    arrows=FALSE)


###################################################
### code chunk number 14: barsurf.Rnw:256-258
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_cfield_3d (bispherical.dist, c (-3, 3),, c (-2, 2),
    fb = c (0.5, 1, 1.75, 2.5), emph="l")


###################################################
### code chunk number 15: barsurf.Rnw:279-281
###################################################
f = function (w1, w2, w3)
    (w1 - 1 / 3)^2 + (w2 - 1 / 3)^2 + (w3 - 1 / 3)^2


###################################################
### code chunk number 16: barsurf.Rnw:289-290
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_tricontour (f, xlab="w1", ylab="w2")


###################################################
### code chunk number 17: barsurf.Rnw:292-293
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_trisurface (f, xlab="w1", ylab="w2")


###################################################
### code chunk number 18: barsurf.Rnw:314-316
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_vecfield (concentric.field, c (-1.6, 1.6), c (-1.1, 1.1),
    m = c (80, 60) )


###################################################
### code chunk number 19: barsurf.Rnw:327-329
###################################################
set.bs.theme ("heat")
set.bs.theme ("blue")


###################################################
### code chunk number 20: barsurf.Rnw:345-347
###################################################
colf = barface.heat ()
colf (c (TRUE, FALSE) )


###################################################
### code chunk number 21: barsurf.Rnw:350-352
###################################################
colf = litmus.heat ()
colf (c (0.25, 0.75) )


###################################################
### code chunk number 22: barsurf.Rnw:373-374
###################################################
colf = barface (c (0, 0, 1) )


###################################################
### code chunk number 23: barsurf.Rnw:378-379
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf)


###################################################
### code chunk number 24: barsurf.Rnw:385-386
###################################################
colf = barface (c (90, 42.5, 60), c (90, 35, 67.5), "HCL")


###################################################
### code chunk number 25: barsurf.Rnw:390-391
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf)


###################################################
### code chunk number 26: barsurf.Rnw:398-399
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (barface.blue () )


###################################################
### code chunk number 27: barsurf.Rnw:401-402
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (barface.green () )


###################################################
### code chunk number 28: barsurf.Rnw:418-421
###################################################
colvs = cbind (c (c (100, 150, 200, 250) ), 35, 75)
rownames (colvs) = c ("first color", "(2nd)", "(3rd)", "last color")
colnames (colvs) = c ("H", "C", "L")


###################################################
### code chunk number 29: barsurf.Rnw:424-425
###################################################
colvs


###################################################
### code chunk number 30: barsurf.Rnw:433-435
###################################################
colf = litmus (,,colvs, "HCL")
colf


###################################################
### code chunk number 31: barsurf.Rnw:439-440
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf)


###################################################
### code chunk number 32: barsurf.Rnw:447-448
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (litmus.heat () )


###################################################
### code chunk number 33: barsurf.Rnw:450-451
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (litmus.blue () )


###################################################
### code chunk number 34: barsurf.Rnw:453-454
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (litmus.green () )


###################################################
### code chunk number 35: barsurf.Rnw:456-457
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (litmus.rainbow () )


###################################################
### code chunk number 36: barsurf.Rnw:470-471
###################################################
colf = mlitmus (litmus.green (-1, 0, reverse=TRUE), litmus.blue (0, 1) )


###################################################
### code chunk number 37: barsurf.Rnw:475-476
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf)


###################################################
### code chunk number 38: barsurf.Rnw:483-484
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (hot.and.cold () )


###################################################
### code chunk number 39: barsurf.Rnw:492-495
###################################################
colf1 = glass.rainbow (0, 1.25, 1, start=-200, end=200)
colf2 = glass.rainbow (1.25, 1.75, c (1, 0.7, 0.4, 0.1), start=200, end=220)
colf3 = glass.rainbow (1.75, 5, 0.1, start=220)


###################################################
### code chunk number 40: barsurf.Rnw:501-502
###################################################
colf = mlitmus (colf1, colf2, colf3)


###################################################
### code chunk number 41: barsurf.Rnw:510-511
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf)


###################################################
### code chunk number 42: barsurf.Rnw:519-520
###################################################
z = c (-2, -1.6, -1.2, -0.5, -0.1667, 0.1667, 0.5, 1.2, 1.6, 2)


###################################################
### code chunk number 43: barsurf.Rnw:524-526
###################################################
getOption("SweaveHooks")[["fig"]]()
plotf_cfield_3d (bispherical.dist, c (-3, 3), z=z,
    fb = c (0.5, 1, 1.75, 2.5), color.function=colf)


###################################################
### code chunk number 44: barsurf.Rnw:539-543
###################################################
custom.litmus.fit = function (x)
{   colvs = cbind (c (c (100, 150, 200, 250) ), 35, 75)
    litmus.fit (x, colvs, "HCL")
}


###################################################
### code chunk number 45: barsurf.Rnw:547-548
###################################################
x = runif (10)


###################################################
### code chunk number 46: barsurf.Rnw:552-554
###################################################
range (x)
custom.litmus.fit (x)


###################################################
### code chunk number 47: barsurf.Rnw:558-559
###################################################
litmus.blue.fit (x)


###################################################
### code chunk number 48: barsurf.Rnw:566-569
###################################################
x = y = seq (0, 1,, 30)
fv = outer (x, y)
fv = fv^3


###################################################
### code chunk number 49: barsurf.Rnw:574-576
###################################################
colf.simple = litmus.blue.fit (fv, equalize=0)
colf.equalized = litmus.blue.fit (fv, equalize=1)


###################################################
### code chunk number 50: barsurf.Rnw:582-583
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf.simple)


###################################################
### code chunk number 51: barsurf.Rnw:590-591
###################################################
fb = c (0.0014, 0.0881)


###################################################
### code chunk number 52: barsurf.Rnw:595-596
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_cfield (,,fv, fb=fb, color.function=colf.simple)


###################################################
### code chunk number 53: barsurf.Rnw:603-604
###################################################
getOption("SweaveHooks")[["fig"]]()
plot (colf.equalized)


###################################################
### code chunk number 54: barsurf.Rnw:611-612
###################################################
getOption("SweaveHooks")[["fig"]]()
plot_cfield (,,fv, fb=fb, color.function=colf.equalized)


###################################################
### code chunk number 55: barsurf.Rnw:641-644
###################################################
x = y = seq (-15.5, 15.5, length.out=60)
fv = outer (x, y, rotated.sinc)
colf = litmus.blue.fit.hcv (fv)


###################################################
### code chunk number 56: barsurf.Rnw:648-657
###################################################
getOption("SweaveHooks")[["fig"]]()
#larger heatmap
plot_cfield (x, y, fv,
    contours=FALSE, color.function=colf)
#smaller heatmap
plotf_cfield (rotated.sinc, c (-4, 4),
    add=TRUE, contours=FALSE, color.function=colf, n=60)
#contour lines
plot_cfield (x, y, fv,
    add=TRUE, fb=0, heatmap=FALSE)


