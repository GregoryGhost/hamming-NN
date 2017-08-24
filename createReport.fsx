let createReport (exitNN : int list list, pathTraningSet, pathTestSets)=
    let pathTraningSet = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"data/Kinnari/traning/")
    let resfile = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"REPORT.html")
    let outf = 
        if System.IO.File.Exists(resfile) then
            System.IO.File.Delete(resfile)
        System.IO.File.CreateText resfile
    let initReport =
        outf.WriteLine "<!doctype html> \
                        <html><head><meta charset=\"utf-8\"/> \
                        <style> img { border: 1px solid #00f; } \
                        hr { border:1px solid #000;} \\
                        table {width:700px;} \
                        td:first-child { color:#f00; text-align:right; } \
                        td { text-align:center; } </style> </head><body>"
        outf.WriteLine "<h1>Классификатора на основе НС Хемминга:</h1>
                        <h1>Отчет о распознавании образов</h1>" 
    let titleSet str =
        outf.WriteLine ("<h2>"+str+"</h2>")
    let addToReport pathSet =
        outf.WriteLine "<table><tr><td></td>"
        Seq.iter (fun i -> outf.WriteLine ("<td><img src='"+i+"'/></td>")) pathSet
        outf.WriteLine "</tr></table><hr/><p/>"
    let addTraningSetToReport pathSet =
        titleSet "Учебный набор"
        addToReport pathSet
    let addResultSetsToReport (testSet, resultSet, traningSet:string list) =
        titleSet "Тестовый набор:"
        addToReport testSet
        outf.WriteLine "<table><tr><td></td>"
        List.iter (fun i -> outf.WriteLine ("<td><img src='"+traningSet.[i]+"'/></td>")) resultSet
        outf.WriteLine "</tr></table><hr/><p/>"
    let loadImgName pathLoad = seq{
        if System.IO.Directory.Exists(pathLoad) then
            for img in System.IO.Directory.EnumerateFiles(pathLoad, "*.bmp") do
                yield img
        else failwith "dir is not found: %s" pathLoad
    }
    let traningSet = loadImgName pathTraningSet 
    traningSet |> addTraningSetToReport
    let tSet = traningSet |> Seq.toList
    {0..List.length pathTestSets-1}
    |> Seq.iter (fun i -> addResultSetsToReport(loadImgName pathTestSets.[i], exitNN.[i], tSet))
    outf.WriteLine "</body></html>"
    outf.Close()
    //TODO:paket auto install Fuchu dll from source code from github repository