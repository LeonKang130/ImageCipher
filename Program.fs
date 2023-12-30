open System

open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

module RC4 =
    let GetSBox (key: byte array) =
        let s = Array.init 256 byte
        let mutable j = 0
        for i in 0 .. 255 do
            j <- (j + int (s[i] + key[i % key.Length])) % 255
            let tmp = s[i]
            s[i] <- s[j]
            s[j] <- tmp
        s
    let Encrypt (key: byte array) (data: byte array) =
        let s = GetSBox key
        let mutable i = 0
        let mutable j = 0
        for k in 0 .. data.Length - 1 do
            i <- (i + 1) % 256
            j <- (j + int s[i]) % 256
            let tmp = s[i]
            s[i] <- s[j]
            s[j] <- tmp
            let t = s[i] + s[j]
            data[k] <- data[k] ^^^ s[int t]
    let Decrypt = Encrypt

[<EntryPoint>]
let main _ =
    use image = Image.Load<Rgba32>("cat.jpg")
    let width, height = image.Width, image.Height
    let imageData: byte array = Array.init (width * height) (fun i ->
        let row, col = i / width, i % width
        image[col, row].ToVector4()
        |> (fun v -> 0.2126f * v.X + 0.7152f * v.Y + 0.0722f * v.Z)
        |> min 1.0f
        |> max 0.0f
        |> (fun l -> l * 255f)
        |> byte
    )
    //let imageData = [| for _ in 0 .. width * height - 1 -> 0uy |]
    let key = "Hello world"
    let keyData =
        key.ToCharArray()
        |> Array.map byte
    RC4.Encrypt keyData imageData
    // RC4.Decrypt keyData imageData
    use outputImage = Image.LoadPixelData<L8>(ReadOnlySpan imageData, width, height)
    outputImage.Save("output.jpg")
    0