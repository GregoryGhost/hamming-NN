#load @"../MatrixOperation.fs"
open MatrixOperation
#r @"../lib/Fuchu.dll"

module Tests = begin
    open Fuchu
    let k = 2.
    let vect = [|1.; 2.; 3.|]
    let m = array2D [vect
                     vect
                     vect]
    let mRect = array2D [vect
                         vect]
    let newMRect = array2D [[1.; 1.]
                            [2.; 2.]
                            [3.; 3.]]
    let mCol = array2D [[1.]
                        [2.]
                        [3.]]
    let mRow = array2D [vect]
    let vOnk = Array.map (fun i-> i*k) vect
    let testingEye n expectedOutput = 
        Assert.Equal("", expectedOutput, eye(n))
    let testingIeye n expectedOutput =
        Assert.Equal("", expectedOutput, ieye(n))
    let testingMulOnK (m : float[,], k : float, expectedOutput : float[,]) =
        Assert.Equal("", expectedOutput, m^*k)
    let testingToVector (m : float[,], expectedOutput : float[,]) =
        Assert.Equal("", expectedOutput, toVector m)
    let testingTransponMatrix (m : float[,], expectedOutput : float[,]) =
        Assert.Equal("", expectedOutput, t(m))
    let testingMatrixDifference (m1 : float[,], m2 : float[,], expectedOutput : float[,]) =
        Assert.Equal("", expectedOutput, !--(m1, m2));
    let testingMaxValueToIndex (m : float[,], expectedOutput : int) =
        Assert.Equal("", expectedOutput, maxValueToIndexCol m)
    let testingMulMatrixs (m : float[,], m1 : float[,], expectedOutput) =
        let mulMatrixs = 
            try
                m^^*m1
            with
            | :? InvalidColAndRowMatrixException -> array2D[]
        Assert.Equal("", expectedOutput, mulMatrixs)
    let suite = 
        TestList [
            testList "eye" [
                testCase "eye 0" <| (fun _ ->
                    let expectedOutput0:float [,]  = array2D []
                    testingEye 0 expectedOutput0
                );
                testCase "eye 2" <| (fun _ ->
                    testingEye 2 (array2D [[1.; 0.]
                                           [0.; 1.]])
                );
                testCase "eye 3" <| (fun _ -> 
                    testingEye 3 (array2D [[1.; 0.; 0.]
                                           [0.; 1.; 0.]
                                           [0.; 0.; 1.]])
                );
            ];
            testList "MulMatrixOnK" [
                testCase "MatrixEmpty" <| (fun _ ->
                    let me = array2D [[]]
                    testingMulOnK(me, k, me) 
                );
                testCase "Vector" <| (fun _ ->
                    testingMulOnK(mRow, k, array2D [vOnk])
                );
                testCase "MatrixSqr" <| (fun _ -> 
                    testingMulOnK(m, k, (array2D [vOnk
                                                  vOnk
                                                  vOnk]))
                );
                testCase "MatrixRect" <| (fun _ ->
                    testingMulOnK(mRect, k, (array2D [vOnk
                                                      vOnk]))
                );
            ];
            testList "ieye" [
                testCase "ieye 0" <| (fun _ ->
                    let expectedOutput0:float [,]  = array2D []
                    testingIeye 0 expectedOutput0
                );
                testCase "ieye 2" <| (fun _ ->
                    testingIeye 2 (array2D [[0.; 1.]
                                            [1.; 0.]])
                );
                testCase "ieye 3" <| (fun _ -> 
                    testingIeye 3 (array2D [[0.; 1.; 1.]
                                            [1.; 0.; 1.]
                                            [1.; 1.; 0.]])
                );
            ];
            testList "toVector" [
                testCase "MatrColToVector" <| (fun _ ->
                    testingToVector(mCol, mRow)
                );
                testCase "MatrRowToVector" <| (fun _ ->
                    testingToVector(mRow, mRow)
                );
                testCase "Matr2x2ToVector" <| (fun _ ->
                    let m2x2 = eye 2
                    testingToVector(m2x2, (array2D [[1.; 0.; 0.; 1.]]))
                );
            ];
            testList "Transpose" [
                testCase "MatrixCol" <| (fun _ ->
                    testingTransponMatrix(mCol, mRow)
                );
                testCase "MatrixRow" <| (fun _ ->
                    testingTransponMatrix(mRow, mCol)
                );
                testCase "M2x3" <| (fun _ ->
                    testingTransponMatrix(mRect, newMRect)
                );
                testCase "M3x2" <| (fun _ ->
                    testingTransponMatrix(newMRect, mRect)
                );
            ];
            testList "MatrixDifference" [
                testCase "VectorRow" <| (fun _ ->
                    let vector = mRow
                    testingMatrixDifference(vector, vector, array2D[[0.; 0.; 0.]])
                );
                testCase "VectorCol" <| (fun _ ->
                    let vector = mCol
                    testingMatrixDifference(vector, vector, array2D[[0.]
                                                                    [0.]
                                                                    [0.]])
                );
                testCase "Matrix2x2" <| (fun _ ->
                    let m2x2 = eye 2
                    testingMatrixDifference(m2x2, m2x2, array2D[[0.; 0.]
                                                                [0.; 0.]])
                );
                testCase "M2x2 !-- vector" <| (fun _ ->
                    let vector = mCol
                    let m2x2 = eye 2
                    let test = 
                        try
                            !--(m2x2, vector) |> ignore
                            0
                        with
                        | :? System.Exception -> 1
                        | _ -> 0
                    match test with
                    |1 -> ()
                    |0 -> failtest "should have failed"
                );
            ];
            testList "maxValueToIndexCol" [
                testCase "InVectorRow" <| (fun _ ->
                    testingMaxValueToIndex(mRow, 2)
                );
                testCase "InMatrixRect" <| (fun _ ->
                    testingMaxValueToIndex(mRect, 2)
                );
                testCase "InVectorCol" <| (fun _ ->
                    testingMaxValueToIndex(mCol, 0)
                );
            ];
            testList "MulMatrixOnMatrix" [
                testCase "vRow*vCol"<| (fun _ ->
                    let expValue = array2D[[14.]]
                    testingMulMatrixs(mRow, mCol, expValue)
                );
                testCase "vCol*vRow"<| (fun _ ->
                    let expValue = array2D[[1.; 2.; 3.]
                                           [2.; 4.; 6.]
                                           [3.; 6.; 9.]]
                    testingMulMatrixs(mCol, mRow, expValue)
                );
                testCase "vCol*vCol"<| (fun _ ->
                    let exceptionValue = array2D[]
                    testingMulMatrixs(mCol, mCol, exceptionValue)
                );
                testCase "mRect*mCol"<| (fun _ ->
                    let expValue = array2D[[14.]
                                           [14.]]
                    testingMulMatrixs(mRect, mCol, expValue)
                );
                testCase "mCol*mRect"<| (fun _ ->
                    let exceptionValue = array2D[]
                    testingMulMatrixs(mCol, mRect, exceptionValue)
                );
            ];
        ]
end