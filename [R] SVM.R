library(RTextTools)

dtMatrix = create_matrix(berita$text, toLower = T,removeNumbers = T, 
                         removePunctuation = T, removeStopwords = T, 
                         stemWords = T)
dtMatrix

container = create_container(dtMatrix, beritatag, trainSize=1:40, virgin=FALSE)
model.svm = train_model(container, "SVM", kernel="linear", cost=1)

pred.svm = predict(model.svm)
accuracy(beritatag, pred.svm)

predMatrix = create_matrix(tanya$text, toLower = T,removeNumbers = T, 
                           removePunctuation = T, removeStopwords = T, 
                           stemWords = T, originalMatrix = dtMatrix)
predMatrix

predSize = nrow(predMatrix)

predictionContainer = create_container(predMatrix, labels=rep(0,predSize), 
                                       testSize=1:predSize, virgin=FALSE)

hasil.svm = classify_model(predictionContainer, model.svm)
hasil.svm
