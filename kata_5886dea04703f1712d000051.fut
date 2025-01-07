def max_two (m1: i64) (m2: i64): i64 =
    if m1 > m2 then m1 else m2

def max_arr (arr: []i64): i64 = reduce (max_two) 0 arr 

def divisors (n: i64): i64 = 
    let (divisors,_) = loop (div_arr, i) = ([], 1) for i < i64.f64 <| f64.sqrt (f64.i64 n) do
        if n % i == 0 then 
            if i != n / i then (concat [i, n/i] div_arr, i + 1)
            else (concat [i] div_arr, i + 1)
        else (div_arr, i + 1)
    in length divisors

def weakness (n: i64): i64 = 
    let n_arr = 1..<n
    let divisors_n = divisors n
    in reduce (+) 0 <|
        map (\x -> if divisors x > divisors_n then 1 else 0) n_arr

def weakest_in_range (n_max: i64): (i64, i64) = 
    let n_arr = 1..<n_max + 1
    let weaknesses_arr = map (\x -> weakness x) n_arr
    let max_weakness = max_arr weaknesses_arr
    let num_at_max = reduce (+) 0 <| 
        map (\x -> if x == max_weakness then 1 else 0) weaknesses_arr
    in (max_weakness, num_at_max)

def main (n: i64): (i64,i64) = weakest_in_range n

