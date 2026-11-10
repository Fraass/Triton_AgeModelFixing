# Temp file



### TEMP

plot(samples.distinct$age[samples.at.hole],
     samples.distinct$sample.depth[samples.at.hole],
     ylim=c(max(samples.distinct$sample.depth[samples.at.hole],na.rm=T),
            0),
     #xlim=c(min(samples.distinct$revised.age[samples.at.hole]),
     #        max(samples.distinct$revised.age[samples.at.hole])),
     
     ylab="Depth (m)",
     xlab="Age (Ma)",
     main=paste("Hole",hole),
     cex=1.5
)
# Original ages
points(samples.distinct$orig.age[samples.at.hole],
       samples.distinct$sample.depth[samples.at.hole],
       col='green4',
       pch=16,
       cex=1.25
)
lines(samples.distinct$orig.age[samples.at.hole][order.of.samples],
      samples.distinct$sample.depth[samples.at.hole][order.of.samples],
      col='green4')
# Zone-based ages
points(samples.distinct$zon.age[samples.at.hole],
       samples.distinct$sample.depth[samples.at.hole],
       col='mediumorchid4',
       pch=18,
       cex=.75
)
segments(samples.distinct$zon.age[samples.at.hole][order.of.samples]-samples.distinct$age.err[samples.at.hole][order.of.samples],
         samples.distinct$sample.depth[samples.at.hole][order.of.samples],
         samples.distinct$zon.age[samples.at.hole][order.of.samples]+samples.distinct$age.err[samples.at.hole][order.of.samples],
         samples.distinct$sample.depth[samples.at.hole][order.of.samples],
         col='mediumorchid4')
lines(samples.distinct$zon.age[samples.at.hole][order.of.samples],
      samples.distinct$sample.depth[samples.at.hole][order.of.samples],
      col='mediumorchid4')


points(samples.distinct$int.mag.age[samples.at.hole],
       samples.distinct$sample.depth[samples.at.hole],
       col='indianred4',
       pch=17,
       cex=1
)
lines(samples.distinct$int.mag.age[samples.at.hole][order.of.samples],
      samples.distinct$sample.depth[samples.at.hole][order.of.samples],
      col='indianred4')


points(samples.distinct$mod.age[samples.at.hole],
       samples.distinct$sample.depth[samples.at.hole],
       col='deepskyblue4',
       pch=18,
       cex=.75
)
lines(samples.distinct$mod.age[samples.at.hole][order.of.samples],
      samples.distinct$sample.depth[samples.at.hole][order.of.samples],
      col='deepskyblue4')


lines(samples.distinct$revised.age[samples.at.hole][order.of.samples],
      samples.distinct$sample.depth[samples.at.hole][order.of.samples],
      lwd=4)


i
hole



