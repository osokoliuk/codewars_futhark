module complex_module = {
    type real = f64
    type complex = (f64, f64)
    type found 'a = #found a | #not_found

    def zero: complex = (0.0,0.0)

    def create_complex (a: f64) (b: f64): complex = (a, b)
    
    def add (z1: complex) (z2: complex): complex = 
        let (a1, b1) = z1
        let (a2, b2) = z2
        in (a1 + a2, b1 + b2)

    def substract (z1: complex) (z2: complex): complex = 
        let (a1, b1) = z1
        let (a2, b2) = z2
        in (a1 - a2, b1 - b2)

    def multiply (z1: complex) (z2: complex): complex = 
        let (a1, b1) = z1
        let (a2, b2) = z2
        in (a1 * a2 - b1*b2, a1 * b2 + a2 * b1)
    
    def norm (z: complex): real = 
        let (a, b) = z
        in a**2 + b**2
    
    def modulus (z: complex): real = 
        let norm_z = norm z
        in norm_z**(1/2)
    
    def pow (z: complex) (n: i64): complex = 
        let (a, b) = z
        let r = modulus z
        let theta = f64.atan (b/a)
        in multiply (r**(f64.i64 n), 0) (f64.cos (f64.i64 n*theta), f64.sin(f64.i64 n*theta))
    
    def sequence (n: i64) (z: complex): complex = 
        let pow_arr = 1..<n+1
        let pow_series: complex = reduce (add) zero <| map (\x -> pow z x) pow_arr
        let multiplier: complex = substract (1 , 0) z
        in multiply multiplier pow_series 
    
    def find_min_seq (z: complex) (eps_tol: f64): i64 =
        let switch: bool = false 
        let n0: i64 = 1
        let n1: i64 = 100
        let (_, n_min) = loop (found, i) = (switch, -1)
            while found == false do
                let n_arr = n0..<n1
                let diff_arr = map (\x -> modulus x) <| map (\x -> substract x z) <| map (\x -> sequence x z) n_arr 
                let diff_arr_ns = zip diff_arr n_arr
                let diff_arr_ns' = filter (\(x,_) -> x < eps_tol) diff_arr_ns
                let (_, ns') = unzip diff_arr_ns'
                let n_iter =  if length ns' >= 1 then 
                    (true, ns'[0])
                else
                    let n0 = n1
                    let n1 = n1 + 100
                    in (false, -1)
                in n_iter
        in n_min

    def f (z: complex) (eps_tol: f64): i64 = 
        let mod_z: real = modulus z 
        in if mod_z < 1.0 then
            find_min_seq z eps_tol
        else -1
}


def main (a: f64) (b: f64) (eps_tol: f64): i64 = 
    let z = complex_module.create_complex a b
    in complex_module.f z eps_tol
