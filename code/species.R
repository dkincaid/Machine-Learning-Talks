library(caret)
species.full = read.table("../data/speciesprocessed.csv",header=T,sep=",")
species.full = species.full[species.full$species=="Canine" | species.full$species=="Feline",]
species.full = species.full[species.full$age < 20000,]
species.full = species.full[species.full$age > 0,]
species.full = species.full[species.full$units =="pounds",]
species.full = species.full[species.full$weight < 1000,]
species.full = na.omit(species.full)
species.features = subset(species.full,select=c("name","sex","age","weight","visits","totsibs","actsibs"))
species.features$name = species.features$name[drop=T]

namefreq = as.data.frame(with(species.features, table(name)))
excludename = as.character(namefreq[namefreq$Freq < 100,"name"])
badnames = as.integer(rownames(species.features[species.features$name %in% excludename,]))
levels(species.features$name) = c(levels(species.features$name),"Other")
species.features[badnames,]$name = "Other"
species.features$name = species.features$name[drop=T]

modelmatrix = model.matrix(~ name - 1, data=species.features)
modelmatrix = model.matrix(~ sex - 1, data=species.features)
species.features = subset(species.features,select=c("age","weight","visits","totsibs","actsibs"))
species.features = cbind(species.features, modelmatrix)

species.targets = subset(species.full, select="species")
species.targets = species.targets$species[drop=TRUE]

set1index = createDataPartition(species.targets, p=.2, list=FALSE, times=1)
species.targets.test = species.targets[set1index]
species.features.test = species.features[set1index,]
species.targets.train = species.targets[-set1index]
species.features.train = species.features[-set1index,]

knnmodel = train(species.features.train,species.targets.train,"knn")

speciesPredictions = extractPrediction(list(knnmodel),testX=species.features.test,testY=species.targets.test)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)

nbmodel = train(species.features.train,species.targets.train,"nb")

speciesPredictions = extractPrediction(list(nbmodel),testX=species.features.test,testY=species.targets.test)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)


rfmodel = train(species.features.train,species.targets.train,"rf")

speciesPredictions = extractPrediction(list(rfmodel),testX=species.features.test,testY=species.targets.test)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)

treebagmodel = train(species.features.train,species.targets.train,"treebag")

speciesPredictions = extractPrediction(list(treebagmodel),testX=species.features.test,testY=species.targets.test)
speciesPredictions = speciesPredictions[speciesPredictions$dataType == "Test",]
confusionMatrix(speciesPredictions$pred, speciesPredictions$obs)
