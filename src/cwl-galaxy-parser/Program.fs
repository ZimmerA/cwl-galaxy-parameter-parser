open Argu
open System
open System.IO

type InputParameterMapTypes =
    | FilePath of string array
    | IntValue of int
    | BoolValue of bool
    | TextValue of string
    | FloatValue of float

type CliArguments =
    | File of inputName: string * path: string
    | Integer of inputName: string * value: int
    | Boolean of inputName: string * value: bool
    | Text of inputName: string * value: string
    | Float of inputName: string * value: float

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | File _ ->
                "Specify a file input (inputname : file). If this flag is used multiple times with the same input name, a collection will be created."
            | Integer _ -> "Specify an integer input (inputname : value)."
            | Boolean _ -> "Specify a boolean input (inputname : value)."
            | Text _ -> "Specify a text input (inputname : value)."
            | Float _ -> "Specify a float input (inputname : value)."

let generateInputParameterMap allResults =
        allResults
        |> List.fold
            (fun parameterMap argument ->
                match argument with
                | File (inputName, filePath) ->
                    parameterMap
                    |> Map.change
                        inputName
                        (fun valueOption ->
                            match valueOption with
                            | Some value ->
                                match value with
                                | FilePath f -> Some(FilePath(f |> Array.append [| filePath |]))
                                | _ -> failwith "An argument with the same name already exists and is not a file"
                            | None -> Some(FilePath [| filePath |]))
                | Integer (inputName, value) ->
                    parameterMap
                    |> Map.change
                        inputName
                        (fun valueOption ->
                            match valueOption with
                            | Some _ -> failwith "An argument with the same name already exists"
                            | None -> Some(IntValue value))
                | Boolean (inputName, value) ->
                    parameterMap
                    |> Map.change
                        inputName
                        (fun valueOption ->
                            match valueOption with
                            | Some _ -> failwith "An argument with the same name already exists"
                            | None -> Some(BoolValue value))
                | Text (inputName, value) ->
                    parameterMap
                    |> Map.change
                        inputName
                        (fun valueOption ->
                            match valueOption with
                            | Some _ -> failwith "An argument with the same name already exists"
                            | None -> Some(TextValue value))
                | Float (inputName, value) ->
                    parameterMap
                    |> Map.change
                        inputName
                        (fun valueOption ->
                            match valueOption with
                            | Some _ -> failwith "An argument with the same name already exists"
                            | None -> Some(FloatValue value)))
            Map.empty<string, InputParameterMapTypes>

let generateOutFile (inputParameterMap: Map<string, InputParameterMapTypes>) : string =
    let outFile: string =
        ("", inputParameterMap)
        ||> Map.fold
                (fun acc key elem ->
                    match elem with
                    | FilePath elem ->
                        match elem.Length with
                        | i when i = 1 ->
                            try
                                File.Copy(elem.[0], Path.Combine("./", Path.GetFileName(elem.[0])))
                            with
                            | _ -> ()

                            acc
                            + key
                            + ":\n"
                            + "  class: File\n"
                            + "  path: inputDataFolder/"
                            + Path.GetFileName(elem.[0])
                            + "\n"
                        | i when i > 1 ->
                            acc
                            + key
                            + ":\n"
                            + "  class: Collection\n"
                            + "  collection_type: list\n"
                            + "  elements:\n"
                            + (("", elem)
                               ||> Array.fold
                                       (fun acc v ->
                                           try
                                               File.Copy(v, Path.Combine("./", Path.GetFileName(v)))
                                           with
                                           | _ -> ()

                                           acc
                                           + "  - class: File\n"
                                           + "   path: inputDataFolder/"
                                           + Path.GetFileName(v)
                                           + "\n"))
                        | _ -> acc
                    | IntValue elem -> acc + key + ": " + string (elem) + "\n"
                    | BoolValue elem -> acc + key + ": " + elem.ToString() + "\n"
                    | TextValue elem -> acc + key + ": " + string (elem) + "\n"
                    | FloatValue elem -> acc + key + ": " + elem.ToString() + "\n")

    outFile


[<EntryPoint>]
let main argv =
    let errorHandler =
        ProcessExiter(
            colorizer =
                function
                | ErrorCode.HelpText -> None
                | _ -> Some ConsoleColor.Red
        )

    let parser =
        ArgumentParser.Create<CliArguments>(programName = "cwl-galaxy-parser", errorHandler = errorHandler)
    let parsedResults = parser.ParseCommandLine argv

    let allResults = parsedResults.GetAllResults()
    let inputParameterMap = generateInputParameterMap allResults
    let outFileContent = generateOutFile inputParameterMap

    System.IO.File.WriteAllText(@"galaxyInput.yml", outFileContent)
    0 // return an integer exit code
