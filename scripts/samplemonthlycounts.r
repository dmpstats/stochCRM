  ###JAN###
  ifelse(!is.na(species.count$JanSD), countJan<-sampleCount(species.count$Jan, species.count$JanSD), countJan<-species.count$Jan)
  sampledJan[i]<-countJan
  ###FEB###
  ifelse(!is.na(species.count$FebSD), countFeb<-sampleCount(species.count$Feb, species.count$FebSD), countFeb<-species.count$Feb)
  sampledFeb[i]<-countFeb
  ###MAR###
  ifelse(!is.na(species.count$MarSD), countMar<-sampleCount(species.count$Mar, species.count$MarSD), countMar<-species.count$Mar)
  sampledMar[i]<-countMar
  ###APR###
  ifelse(!is.na(species.count$AprSD), countApr<-sampleCount(species.count$Apr, species.count$AprSD), countApr<-species.count$Apr)
  sampledApr[i]<-countApr
  ###MAY###
  ifelse(!is.na(species.count$MaySD), countMay<-sampleCount(species.count$May, species.count$MaySD), countMay<-species.count$May)
  sampledMay[i]<-countMay
  ###JUN###
  ifelse(!is.na(species.count$JunSD), countJun<-sampleCount(species.count$Jun, species.count$JunSD), countJun<-species.count$Jun)
  sampledJun[i]<-countJun
  ###JUL###
  ifelse(!is.na(species.count$JulSD), countJul<-sampleCount(species.count$Jul, species.count$JulSD), countJul<-species.count$Jul)
  sampledJul[i]<-countJul
  ###AUG###
  ifelse(!is.na(species.count$AugSD), countAug<-sampleCount(species.count$Aug, species.count$AugSD), countAug<-species.count$Aug)
  sampledAug[i]<-countAug
  ###SEP###
  ifelse(!is.na(species.count$SepSD), countSep<-sampleCount(species.count$Sep, species.count$SepSD), countSep<-species.count$Sep)
  sampledSep[i]<-countSep
  ###OCT###
  ifelse(!is.na(species.count$OctSD), countOct<-sampleCount(species.count$Oct, species.count$OctSD), countOct<-species.count$Oct)
  sampledOct[i]<-countOct
  ###NOV###
  ifelse(!is.na(species.count$NovSD), countNov<-sampleCount(species.count$Nov, species.count$NovSD), countNov<-species.count$Nov)
  sampledNov[i]<-countNov
  ###DEC###
  ifelse(!is.na(species.count$DecSD), countDec<-sampleCount(species.count$Dec, species.count$DecSD), countDec<-species.count$Dec)
  sampledDec[i]<-countDec

  ###COMPILE IN DATA FRAME###
  sampledCountIter<-c(sampledJan[i], sampledFeb[i], sampledMar[i], sampledApr[i], sampledMay[i], sampledJun[i], sampledJul[i], sampledAug[i], sampledSep[i], sampledOct[i], sampledNov[i], sampledDec[i])
  sampledSpeciesCount[i,]=sampledCountIter
