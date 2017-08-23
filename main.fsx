#load "DataIO.fsx"
#load "NetworkHamming.fsx"
open DataIO
open NetworkHamming
let main = 
    let pathTraningSet = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"data/Kinnari/traning/")
    let pathTestSet = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"data/Kinnari/test/0.3/")
    let mutable traningSet = loadData pathTraningSet
    let mutable wMatrix = traningNN traningSet
    let mutable testSet = loadData pathTestSet
    // printfn "wHamming nrow=%d, ncol=%d" (Array2D.length1 wMatrix.Hamming.[0])(Array2D.length2 wMatrix.Hamming.[0])
    // printfn "wHopfield ncol=%d, nrow=%d" (Array2D.length2 wMatrix.Hopfield)(Array2D.length1 wMatrix.Hopfield)
    let exitNN = [runTestingNN(&testSet,wMatrix);
                  runTestingNN(&traningSet,wMatrix)]
    save exitNN