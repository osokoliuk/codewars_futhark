def neq lte x y = if x `lte` y then !(y `lte` x) else true

def pack lte xs =
  zip3 (indices xs) xs (rotate (-1) xs)
  |> filter (\(i,x,y) -> i == 0 || neq lte x y) |> map (.1)


def sigma1 (n: f64): f64 =
    let (divisors, _) = loop (divisors_arr, i) = ([],1) for i < i64.f64 (f64.sqrt (n + 1.0)) do
        if i64.f64 n % i == 0 && i != i64.f64 n then (concat divisors_arr [f64.i64 i, (n / f64.i64 i)], i + 1)
        else (divisors_arr, i + 1)
    let sigma_1_no_dupes = pack (f64.<=) divisors
    let sigma1_reduced = reduce (+) 0 sigma_1_no_dupes
    in sigma1_reduced

def main (n: f64): f64 = sigma1 n
