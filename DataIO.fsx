#load "MatrixOperation.fs"
open System.IO
open System.Drawing
let resfile = Path.Combine(__SOURCE_DIRECTORY__,"result.txt")
let save data= 
    let outf = 
        if File.Exists(resfile) then
            File.Delete(resfile)
        File.CreateText resfile
    let write  (x: int list, outs: StreamWriter)= 
        for i in x do 
           i.ToString() |> outs.WriteLine
        outf.WriteLine "<--->"
    List.iter (fun i -> write(i, outf)) data
    outf.Close()
let loadData (path_load : string)= 
    let convertImg (img : Bitmap) =
        let uncolor (c:Color)=
            let x = (float)(0.3*(float c.R) + 0.59*(float c.G) + 0.11*(float c.B))
            x
        let convertMinus11 (y : float)= abs (y/255.0) - 1.0
        let z = Array2D.init img.Width img.Height (fun i j -> img.GetPixel(i,j) |> uncolor |> convertMinus11)
        MatrixOperation.toVector z
    let loadImg = seq{
            if Directory.Exists(path_load) then
                for img in Directory.EnumerateFiles(path_load, "*.bmp") do
                    let imgIn = new Bitmap(img)
                    yield imgIn
            else failwith "dir is not found: %s" path_load
    }
    Seq.map convertImg loadImg |> Seq.toList