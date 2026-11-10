# FOR SOLVING PROBLEM FROM AUTOBIOSTRAT

which("Ciperoella ciperoensis" == triton.wide.plus.samples$species)->cip
unique(triton.wide.plus.samples$site[cip])->cip.sites
# Sites to examine 709

for(i in 1:length(cip.sites)){
  Site<-cip.sites[i]
  plot(triton.wide.plus.samples$age[which(triton.wide.plus.samples$site == Site)],
       triton.wide.plus.samples$sample.depth[which(triton.wide.plus.samples$site == Site)],
       ylim=c(max(triton.wide.plus.samples$sample.depth[which(triton.wide.plus.samples$site == Site)],na.rm=T),0),
       ylab="Depth",
       xlab="Age (Ma)",
       main=paste("Site",Site)
  )
  
  
  points(triton.wide.plus.samples$orig.age[which(triton.wide.plus.samples$site == Site)],
         triton.wide.plus.samples$sample.depth[which(triton.wide.plus.samples$site == Site)],
         col='red',
         pch=16
  )
  
}


hist(triton.wide.plus.samples$age[which(triton.wide.plus.samples$site == Site)]-triton.wide.plus.samples$orig.age[which(triton.wide.plus.samples$site == Site)])
hist(diff(triton.wide.plus.samples$age[which(triton.wide.plus.samples$site == Site)]))


# Switch to holeID
which("Ciperoella ciperoensis" == triton.wide.plus.samples$species)->cip
unique(triton.wide.plus.samples$holeID[cip])->cip.holes
# Sites to examine 709
problem.holes->cip.holes #KLUGE#
for(i in 1:length(cip.holes)){
  hole<-cip.holes[i]
  plot(triton.wide.plus.samples$age[which(triton.wide.plus.samples$holeID == hole)],
       triton.wide.plus.samples$sample.depth[which(triton.wide.plus.samples$holeID == hole)],
       ylim=c(max(triton.wide.plus.samples$sample.depth[which(triton.wide.plus.samples$holeID == hole)],na.rm=T),
              0),
       #xlim=c(0,
      #        max(triton.wide.plus.samples$age[which("Ciperoella ciperoensis" == triton.wide.plus.samples$species)],na.rm=T)),
       ylab="Depth",
       xlab="Age (Ma)",
       main=paste("Hole",hole)
  )
  
  
  points(triton.wide.plus.samples$orig.age[which(triton.wide.plus.samples$holeID == hole)],
         triton.wide.plus.samples$sample.depth[which(triton.wide.plus.samples$holeID == hole)],
         col='red',
         pch=16,
         cex=.75
  )
  abline(v=c(2.58,5.333,23.03,33.9,56,65))
}


# Problem Holes
c(
  "359.1468.forams.359.U1468A",
  "115-714A.a",
  "115-709C",
  "101-628A",
  "115-706A.a",
  "115-709A",
  "313_M0027A",
  "114-703A.a",
  "80-548A",
  "342.1405.324.planktic.foram.U1405A",
  "115-710A.b",
  "9.77.77B", #maybe
  "184_1148A",
  "320.321.1336.320.Planktonic.Foraminifera.U1336B",
  "92-597", 
  "144-871A.a",
  "363.1490.planktonic.foram.363.U1490A",
  "149-898A.a",
  "184-1148A",
  "144-872C.a",
  "115-709B.a",
  "149-900A.b",
  "80-549A",
  "149-897C",
  "342.1404.324.planktic.foram.U1404A",
  "149-900A.a",
  "115-714A.b")->problem.holes
sort(problem.holes)

# Getting age models per site



#### CHUNK FROM Basic Ecological Metrics.Rmd
# Working on building a Wide, not long, data table
    # This started from https://jennabraun.github.io/posts/2019/, but with adaptation since Triton already has lots of cleaning done on it.
    #str(triton.pres)
    
    # Need: species, abundance
    #age, pal.lat, pal.long, lat.long,holeID,sampleID
    
    #Data has already been cleaned, and there's already a unique ID.
    #I'm going to make a table that's the sampleID, age,  pal.lat, pal.long, latitude, longitude ,holeID
    #and a second one which is sampleID, species ID and abundance
    samples<- triton.pres %>% select(sampleID, 
                                     age,
                                     age.err,
                                     age.calc,
                                     orig.age,
                                     zone,
                                     zon.age,
                                     age.st,
                                     age.en,
                                     rng.age,
                                     int.age,
                                     err.int.age,
                                     mag.zone,
                                     mag.age.st,
                                     mag.age.en,
                                     mag.age,
                                     int.mag.age,
                                     err.int.mag.age,
                                     mod.age,
                                     #orig.st,
                                     #orig.en,
                                     #Zone.st,
                                     #Zone.en,
                                     #Zone.st.ref,
                                     #Zone.en.ref,
                                     #orig.ts,
                                     pal.lat, 
                                     pal.long, 
                                     latitude, 
                                     longitude,
                                     sample.depth, 
                                     holeID,
                                     oceanic,
                                     oceanic.augment,
                                     oceanic.augment.pal.lat)
    
    triton.wide<- triton.pres %>% select(sampleID,species,abundance)
    
    # spread data into a wide format
    triton.wide<-triton.wide %>% pivot_wider(names_from = species,values_from = abundance)
    
    # need to replace NAs with zero
    triton.wide[is.na(triton.wide)]<-0
    
    #samples and triton.wide are differently sized at this point
    all.equal(samples$sampleID,triton.wide$sampleID)
    # [1] "Lengths (266841, 28705) differ (string compare on first 28705)" "27719 string mismatches"     
    # 27,719!
    triton.wide <- triton.wide[match(samples$sampleID, triton.wide$sampleID),]
#### END CHUNK


distinct(samples)->samples.distinct

##
unique(samples.distinct$holeID)->all.holes

# Making space for the revised age model column
cbind(samples.distinct,revised.age=rep(NA,times=length(samples.distinct[,1])))->samples.distinct
samples.distinct$revised.age<-samples.distinct$age

# Moving to the revised age model folder
setwd('./Bad Age models/')


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
  
  # Error switch, un comment if wanting to see all age models
  
  #if(number.of.zero.changes >= 2){
  
  
    print(
    paste(
      paste(  
        paste("Hole",hole),
        paste("has",number.of.zero.changes)
        ),
    "depth changes without change in age.")
    )
    
    #pdf(file=paste(hole,".pdf",sep=""))
    plot(samples.distinct$age[samples.at.hole],
         samples.distinct$sample.depth[samples.at.hole],
         ylim=c(max(samples.distinct$sample.depth[samples.at.hole],na.rm=T),
                0),
         xlim=c(min(
           samples.distinct$zon.age[samples.at.hole][order.of.samples]-abs(samples.distinct$age.err[samples.at.hole][order.of.samples])
           ,na.rm=T),
           max(
             samples.distinct$zon.age[samples.at.hole][order.of.samples]+abs(samples.distinct$age.err[samples.at.hole][order.of.samples])
             ,na.rm=T)),
         #xlim=c(0,
         #        max(triton.wide.plus.samples$age[which("Ciperoella ciperoensis" == triton.wide.plus.samples$species)],na.rm=T)),
         ylab="Depth",
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
    
    ## NOTE ##
    
    ## While coding this, I found some `age.err`
    ## values that had a positive and some negative values
    ## need to use max/min to do interpolation.
    
    ##.    ##
    
    
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
    rug(c(2.58,5.333,23.03,33.9,56,65),side=1)
    #dev.off()
  
    
    
    
    #} # Uncomment, end of 'error switch' to see only sites with errors, or all
  
  
  
  #points(samples$orig.age[which(triton.wide.plus.samples$holeID == hole)],
  #       samples$sample.depth[which(triton.wide.plus.samples$holeID == hole)],
  #       col='red',
  #       pch=16,
  #       cex=.75
  #)
  
}
setwd("/Users/andrewfraass/Library/CloudStorage/OneDrive-UniversityofVictoria/Research/ARL_Ecological Metric/")


### New chunk that applies ages ###
## As I develop, might move into the loop above ##

## This will apply the maximum ages for the error and interpolate between. 

## Next iteration will take the age of the sample above and use that as the minimum age, 
## if that sample is within the error. 
## Similar with the bottom sample in the repeated chunk.

hole<-all.holes[i]
which(samples.distinct$holeID == hole)->samples.at.hole

# making sure site is in correct order
order(samples.distinct$sample.depth[samples.at.hole])->order.of.samples
# Samples that have no change in age but changes in depth
diff(samples.distinct$age[samples.at.hole][order.of.samples])->first.diffs
which(first.diffs == 0)->zero.change.in.depth

# find each 'chunk' of the age model
unique(samples.distinct$age[samples.at.hole][order.of.samples][zero.change.in.depth])->repeated.ages

 ## Visualize ##
    # abline(v=unique(samples.distinct$age[samples.at.hole][order.of.samples][zero.change.in.depth]))

## Yes, triple looping is obviously inefficient, but i have complete control over it ##
## It only needs to be run once to apply corrections, so... it's fine ##

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
## Visualize ##
lines(samples.distinct$revised.age[samples.at.hole][order.of.samples],
      samples.distinct$sample.depth[samples.at.hole][order.of.samples],
      lwd=4)





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
              "159-958A")

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
  
  suppressWarnings(rug(c(2.58,5.333,23.03,33.9,56,65),side=1)) #geo timescale, but it throws warnings whenever there's a tick outside of the plotting area
  dev.off()
  
  
  
  
  } # Uncomment, end of 'error switch' to see only sites with errors, or all
  
  

  
}


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







