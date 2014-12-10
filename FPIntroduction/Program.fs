// Simple add function
let add a b = a + b


// partial application
let add1 x = add 1 x


// currying 
let add2 = add 2 


// make another method
let mul a b = a * b


// compose 
let add1mul2 x = mul 2 (add 1 x)


// data transformation
let add2mul3 x = x |> add 2 |> mul 3


let s x = mul 3  (add 2 x) 
// let (|>) x f = f x


// pipeline building
let add4mul5 =  add 2 >> mul 3

// let (>>) x f v = f (x v)

let addmul a b =  add a >> mul b


// seq (IEnumerable)
let seq1to4 = seq {1..4}


// Add n to each element in a sequence
let addSeq a = Seq.map (add a)


// Big pipeline 
let addMulSumSeq a b = Seq.map (add a) >> Seq.map (mul b) >> Seq.reduce (+)

// tuples
let vector1 = 1.0,2.0
let vector2 = 5.0,6.0


// define alias
type Vector = float * float

let scale f ((x,y):Vector) = (x*f,y*f)

let scalarMultiply (x1:float,y1:float) (x2,y2) = (x1*x2,y1*y2)


// matching 
let format singular plural b = 
    match b with 
    | 0 -> sprintf "no %s" plural
    | 1 -> sprintf "one %s" singular
    | 2 -> sprintf "two %s" plural
    | x -> sprintf "%d %s" x plural

let formatIHave singular plural = format singular plural >> sprintf "I have %s" 

let formatIHaveApples = formatIHave "apple" "apples"


// guards
let formatPositive singular plural b = 
    match b with 
    | 0 -> sprintf "no %s" plural
    | 1 -> sprintf "one %s" singular
    | 2 -> sprintf "two %s" plural
    | x when x < 0 -> failwith "negative is not allowed!" 
    | x -> sprintf "%d %s" x plural

let formatIHavePositive singular plural = formatPositive singular plural >> sprintf "I have %s" 

let formatIHavePositiveApples = formatIHavePositive "apple" "apples"


type Counts = Zero | One | Two | Many


type Result = 
    | Value of int
    | Error of string

let div x y = 
    if y <> 0 then
        Value (x / y)
    else
        Error "division by 0!"

type Shape =
    | Rectangle of float * float
    | Circle of float

let surfaceOfShape s = 
    match s with
    | Circle r -> System.Math.PI * r * r
    | Rectangle (w,h) -> w * h

type Shape with
    member s.Surface = surfaceOfShape s

type AssetId = AssetId of int

type AssetId with
    member x.toInt = let (AssetId i) = x in i


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 
    
    
// return an integer exit code
// closures 
//let accumulator op n = 
//    let acc = ref (n)
//    fun i -> acc := op !acc i; !acc   
