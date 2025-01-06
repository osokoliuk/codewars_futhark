def binom (n: i64) (k: i64) : i64 =
  if n >= k
  then reduce (*) 1 (n - k + 1..<n + 1) / reduce (*) 1 (1..<k + 1)
  else 0

def v1 (n: i64) (p: i64) : i64 =
  let n_arr = 0..<n + 1
  in reduce (+) 0 <| map (\x -> (-1) ** x * p * 4 ** (n - x) * binom (2 * n - x) x) n_arr

def u1 (n: i64) (p: i64) : i64 =
  let n_arr = 0..<n + 1
  in reduce (+) 0 <| map (\x -> (-1) ** x * p * 4 ** (n - x) * binom (2 * n - x + 1) x) n_arr

def veff (n: i64) (p: i64) : i64 = (2 * n + 1) * p
def ueff (n: i64) (p: i64) : i64 = (n + 1) * p

def main (n: i64) (p: i64) : (i64, i64) = (veff n p, ueff n p)
