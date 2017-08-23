#load "MatrixOperation.fs"
open MatrixOperation
let traningNN (set : float [,] list) =
    let m = List.length set
    let weightHamming = List.map (fun i-> i^*0.5) set
    let c = 1.0/(2.0*(float m))
    let weightHopfield = !--((eye m),((ieye m)^*c))
    let weightMatrix = { Hamming = weightHamming; Hopfield = weightHopfield }
    weightMatrix
let runTestingNN (tSet:float [,] list byref,
                  w:WeightMatrix)=
    let evalWeightLayerHopfield x = 
        let tx = t(x) 
        // printfn "tx nrow=%d, ncol=%d" (Array2D.length1 tx)(Array2D.length2 tx)
        let a = List.map (fun i -> i^^*tx) w.Hamming
        let convertAtoArray2D = Array2D.init 1 (List.length a) (fun i j -> a.[j].[0,0])
        // printfn "w.Hamming element=%A" convertAtoArray2D
        convertAtoArray2D
    let evalExitLayerHopfield (l:float [,], wHopfield:float[,]) = 
        let MAX_ITER = 50
        let mutable lold = l
        let mutable l_new:float [,]=l
        let rec filterHopfield iter =
            let removeNegativeValue j = (j+abs(j))/2.0
            l_new <- lold^^*wHopfield
            l_new <- Array2D.map removeNegativeValue l_new
            let isEquialMatrix =
                Seq.exists (fun i ->
                    l_new.[0,i]=lold.[0,i]) 
                    {0..(Array2D.length2 l_new)-1}
            if not isEquialMatrix && iter<MAX_ITER then 
                lold <- l_new
                filterHopfield (iter+1)
        filterHopfield 0
        l_new
    let mutable indexs:int list = []
    for img in tSet do
        let wHopfieldLayer = evalWeightLayerHopfield img
        let exitNN = evalExitLayerHopfield(wHopfieldLayer, w.Hopfield)
        // printfn "exitNN=%A" exitNN
        let t = maxValueToIndexCol exitNN
        indexs <- t :: indexs
    // printfn "-----"
    List.rev indexs