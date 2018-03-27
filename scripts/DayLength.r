
# Day length function - unmodified from Masden ----------------------------


#### Taken from Forsythe et al.(1995) A model comparison for daylength as a function of latitude and day of year.  Ecological Modelling. 80: 87 - 95


							
DaylengthThruYr = data.frame (seq(1,365,1))
names(DaylengthThruYr) = c("YrDay")

DaylengthThruYr$P = asin(0.39795 * cos (0.2163108 + 2 * atan (0.9671396 * tan(0.0086 * (DaylengthThruYr$YrDay - 186)))))

DaylengthThruYr$DayLength = 24 - (24/pi) * acos((sin(0.8333*pi/180) + sin(Latitude * pi / 180) * sin (DaylengthThruYr$P)) / (cos(Latitude *pi /180 ) * cos(DaylengthThruYr$P)))

hours = data.frame(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
names(hours) = c("Month")

hours$Total[1] = 31 * 24
hours$Total[2] = 28 * 24
hours$Total[3] = 31 * 24
hours$Total[4] = 30 * 24
hours$Total[5] = 31 * 24
hours$Total[6] = 30 * 24
hours$Total[7] = 31 * 24
hours$Total[8] = 31 * 24
hours$Total[9] = 30 * 24
hours$Total[10] = 31 * 24
hours$Total[11] = 30 * 24
hours$Total[12] = 31 * 24

hours$Day[1] = sum(DaylengthThruYr$DayLength[c(1:31)])
hours$Day[2] = sum(DaylengthThruYr$DayLength[c(32:59)])
hours$Day[3] = sum(DaylengthThruYr$DayLength[c(60:90)])
hours$Day[4] = sum(DaylengthThruYr$DayLength[c(91:120)])
hours$Day[5] = sum(DaylengthThruYr$DayLength[c(121:151)])
hours$Day[6] = sum(DaylengthThruYr$DayLength[c(152:181)])
hours$Day[7] = sum(DaylengthThruYr$DayLength[c(182:212)])
hours$Day[8] = sum(DaylengthThruYr$DayLength[c(213:243)])
hours$Day[9] = sum(DaylengthThruYr$DayLength[c(244:273)])
hours$Day[10] = sum(DaylengthThruYr$DayLength[c(274:304)])
hours$Day[11] = sum(DaylengthThruYr$DayLength[c(305:334)])
hours$Day[12] = sum(DaylengthThruYr$DayLength[c(335:365)])

hours$Night = hours$Total - hours$Day


