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

// let (|>) x f = f x


// pipeline building
let add4mul5 x =  add 2 >> mul 3

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

// closures 
let accumulator op n = 
    let acc = ref (n)
    fun i -> acc := op !acc i; !acc   

// guards
let formatPositive singular plural b = 
    match b with 
    | 0 -> sprintf "no %s" plural
    | 1 -> sprintf "one %s" singular
    | 2 -> sprintf "two %s" plural
    | _ as x when x < 0 -> failwith "negative is not allowed!" 
    | x -> sprintf "%d %s" x plural

let formatIHavePositive singular plural = formatPositive singular plural >> sprintf "I have %s" 

let formatIHavePositiveApples = formatIHavePositive "apple" "apples"



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
