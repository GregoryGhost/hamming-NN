module MatrixOperation
exception InvalidColAndRowMatrixException of int * int
type WeightMatrix = {
    Hamming:float [,] list
    Hopfield:float [,]
}
let eye (k:int) =
    Array2D.init k k (fun i j -> if i=j then 1.0 else 0.0)
let (^*) matrix (k:float)=
    Array2D.map (fun i -> i*k) matrix
let ieye (k:int)=
    Array2D.init k k (fun i j -> if i=j then 0.0 else 1.0)
let toVector (x : float [,])=
    let nrow = Array2D.length1 x
    let ncol = Array2D.length2 x
    let vector = Array2D.init 1 x.Length (fun i j ->
                                            let vi = (float j)/(float ncol) |> floor |> int
                                            let vj = j-vi*ncol
                                            x.[vi,vj])
    vector
let t(m) = 
    let nrow = Array2D.length1 m
    let ncol = Array2D.length2 m
    let res = Array2D.init ncol nrow (fun i j ->
                                         m.[j,i])
    res
let (!--) (m1:float [,], m2:float [,]) =
    let l1m1 = Array2D.length1 m1
    let l2m1 = Array2D.length2 m1
    let l1m2 = Array2D.length1 m2
    let l2m2 = Array2D.length2 m2
    let check = (l1m1=l1m2)&&(l2m1=l2m2)
    if not check then 
        failwith "diffrent size Array2D"
    else
        let srow = l1m1
        let scol = l2m1
        let result = Array2D.init srow scol (fun i j ->
                                               m1.[i,j]-m2.[i,j])
        result
let maxValueToIndexCol (x : float [,]):int=
    let mutable maxX = 0.0
    let mutable index = 0
    Seq.iter (fun i ->
        let cutv = x.[i,0..(Array2D.length2 x)-1]
        Seq.iter (fun j ->
            if cutv.[j] >= maxX then 
                maxX <- cutv.[j]
                index <- j
            ) {0..(Array.length cutv)-1}
    ) {0..(Array2D.length1 x)-1}
    index
let (^^*) m1 m2 =
    let scol=Array2D.length2 m1
    let srow=Array2D.length1 m2
    if scol<>srow then raise <| InvalidColAndRowMatrixException(scol, srow)
    let res:float [,] = Array2D.zeroCreate (Array2D.length1 m1) (Array2D.length2 m2)
    let genRes =
        for i in 0..(Array2D.length1 m1)-1 do
            for k in 0..(Array2D.length2 m2)-1 do
                for j in 0..scol-1 do
                    res.[i,k] <- res.[i,k] + m1.[i,j] * m2.[j,k]

    res                              