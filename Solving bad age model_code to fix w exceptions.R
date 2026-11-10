# Temp File 2

## SITES THAT NEED TO BE LEFT OUT OF PROCESS BELOW 
exceptions<-c("198_1210A",
              "114-700B",
              "208-1266B",
              "119-738C.c",
              "93-605",
              "154-927C",
              "28-265",
              "172-1062B",
              "172-1063A",
              "7-62A",
              "14-141",
              "166-1006A",
              "159-958A",
              "120-747A", # !!NEEDS TO BE LOOKED AT BEFORE USE!!
              "198-1211C", # NEEDS TO BE LOOKED AT. ALL 1 DEPTH?
              "306-1313A",
              "306-1313D",
              "306-1313B",
              "183-1138A",
              "339.1391.339.planktic.foram.U1391C", #Needs to be examined
              "177-1091A",
              "151.912.912A" # uh... is this a single sample?
              )

# Make revised.age and plot

#for(i in 1:length(problem.holes)){
for(i in 1:length(all.holes)){
  
  
  # Good hole to work as an example is i<-11
  
  
  #hole<-problem.holes[i]
  hole<-all.holes[i]
  which(samples.distinct$holeID == hole)->samples.at.hole
  
  # making sure site is in correct order
  order(samples.distinct$sample.depth[samples.at.hole])->order.of.samples
  #plot(samples$age[samples.at.hole][order.of.samples],
  #     samples$sample.depth[samples.at.hole][order.of.samples]
  #     )
  
  diff(samples.distinct$age[samples.at.hole][order.of.samples])->first.diffs
  which(first.diffs == 0)->zero.change.in.depth
  length(zero.change.in.depth)->number.of.zero.changes
  
  # Error switch, uncomment if wanting to see all age models
  
  if(number.of.zero.changes >= 2 & !any(hole == exceptions)){
    
    
    # find each 'chunk' of the age model
    unique(samples.distinct$age[samples.at.hole][order.of.samples][zero.change.in.depth])->repeated.ages
    
    ## Creating revised.age
    for(o in 1:length(repeated.ages)){
      which(samples.distinct$age[samples.at.hole][order.of.samples] == repeated.ages[o])->chunk.samples
      max(samples.distinct$sample.depth[samples.at.hole][order.of.samples][chunk.samples])->d1 
      min(samples.distinct$sample.depth[samples.at.hole][order.of.samples][chunk.samples])->d2 
      
      samples.distinct$age[samples.at.hole][order.of.samples][chunk.samples][1]-
        abs(samples.distinct$age.err[samples.at.hole][order.of.samples][chunk.samples][1])->a2  #[1] is because these are always the same
      
      samples.distinct$age[samples.at.hole][order.of.samples][chunk.samples][1]+
        abs(samples.distinct$age.err[samples.at.hole][order.of.samples][chunk.samples][1])->a1 
      
      # First sample is just the minimum age
      samples.distinct$revised.age[samples.at.hole][order.of.samples][min(chunk.samples)]<-a2
      
      # Last sample is just the max age
      samples.distinct$revised.age[samples.at.hole][order.of.samples][max(chunk.samples)]<-a1
      
      
      # if previous sample before 'chunk' has an age within the error bounds
      if(min(chunk.samples) != 1){ # Checking to make sure this is not the last sample at site
        if(a2 < samples.distinct$age[samples.at.hole][order.of.samples][max(chunk.samples)-1]){
          c(min(chunk.samples)-1,chunk.samples)->chunk.samples #making the code below work by adding a sample into the series
          samples.distinct$age[samples.at.hole][order.of.samples][min(chunk.samples)]->samples.distinct$revised.age[samples.at.hole][order.of.samples][min(chunk.samples)] #keeping original age
          a2<-samples.distinct$revised.age[samples.at.hole][order.of.samples][min(chunk.samples)] # new last age in sequence
          d2<-min(samples.distinct$sample.depth[samples.at.hole][order.of.samples][chunk.samples]) # new last depth in sequence
        }
      }
      
      
      
      # if next sample after 'chunk' has an age within the error bounds
      if(max(chunk.samples) < max(order.of.samples)){ # Checking to make sure this is not the last sample at site
        if(a1 > samples.distinct$age[samples.at.hole][order.of.samples][max(chunk.samples)+1]){
          c(chunk.samples,max(chunk.samples)+1)->chunk.samples #making the code below work by adding a sample into the series
          samples.distinct$age[samples.at.hole][order.of.samples][max(chunk.samples)
          ]->samples.distinct$revised.age[samples.at.hole][order.of.samples][max(chunk.samples)] #keeping original age
          a1<-samples.distinct$revised.age[samples.at.hole][order.of.samples][max(chunk.samples)] # new last age in sequence
          d1<-max(samples.distinct$sample.depth[samples.at.hole][order.of.samples][chunk.samples]) # new last depth in sequence
        }
      }
      
      # identifying sedimentation rate
      {d2-d1}/{a2-a1}->sr
      
      for(p in 2:{
        length(chunk.samples)-1
      }){
        depth.of.sample<-samples.distinct$sample.depth[samples.at.hole][order.of.samples][chunk.samples[p]]
        {d2-d1}/{a2-a1}->sr
        a1-1/sr*(d1-depth.of.sample)->a3
        a3->samples.distinct$revised.age[samples.at.hole][order.of.samples][chunk.samples[p]]
      } 
      
      
    }
    
    
    print(
      paste(
        paste(  
          paste("Hole",hole),
          paste("has",number.of.zero.changes)
        ),
        "depth changes without change in age.")
    )
    
    
    
    pdf(file=paste(hole,".pdf",sep=""))
    plot(samples.distinct$age[samples.at.hole],
         samples.distinct$sample.depth[samples.at.hole],
         ylim=c(max(samples.distinct$sample.depth[samples.at.hole],na.rm=T),
                0),
         xlim=c(min(samples.distinct$revised.age[samples.at.hole],na.rm=T),
                 max(samples.distinct$revised.age[samples.at.hole],na.rm=T)),
         
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
    
    suppressWarnings(rug(c(2.58,5.333,23.03,33.9,56,65),side=1)) #geo timescale, but it throws warnings whenever there's a tick outside of the plotting area
    dev.off()
    
    
    
    
  } # Uncomment, end of 'error switch' to see only sites with errors, or all
  
  
  
  
}