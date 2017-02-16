#r "../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"

open Newtonsoft.Json

type Circle = 
    { Radius : int
      Numbers : List<int> }

let example = 
    { Radius = 4
      Numbers = [ 1; 4; 3 ] }

JsonConvert.SerializeObject(example)