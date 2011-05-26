library("klaR")

classify.naiveBayes = function(trainingSet, classes){
    # polaczenie transponowanej macierzy slow z klasami i stworzenie data.frame
    tsData <- data.frame(cbind(t(trainingSet), classes));
    
    # utworzenie klasyfikatora, usekernel uzyte dla przypadkow gdy wariancja klas jest zerowa w ramach rozpatrywanego atrybutu
    nb <- NaiveBayes(tsData, factor(tsData$classes), usekernel = TRUE);
    return(nb);
}


nb <- classify.naiveBayes(tmatrix, classes);
#predict(nb, t(tmatrix));

