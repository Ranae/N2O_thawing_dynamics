flux<-read_csv("data/mesocosm_fluxes.csv")
head(flux)
colnames(flux)<-c("hour", "date", "core", "N", "ins")
flux<-filter(flux, N != "NA", hour > 59)

thermal10aP<-subset(thermal, thermal$year == "2010" & thermal$ground == "Above" & thermal$trt == "P")

thermal10aP$plotF <- as.factor(thermal10aP$plot)
thermalG<-groupedData(mass ~ gdd | plotF, data=thermal10aP)

fit.beta.10a <- nlsList(mass ~ SSbgf(gdd, w.max, t.e, t.m), data = thermalG)
plot(intervals(fit.beta.10a), layout = c(3,1))
fit.nlme.10a<-nlme(fit.beta.10a, random=pdDiag(w.max ~1))

fluxG<-groupedData(N ~ hour | ins, data=flux)

fit.beta.flux<- nlsList(N ~ SSbgf(hour, w.max, t.e, t.m), data = fluxG)
