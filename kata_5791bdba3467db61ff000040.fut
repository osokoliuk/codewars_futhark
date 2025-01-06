def matmul [m][n][p] (A: [m][n]f64) (B: [n][p]f64): [n][p]f64 = 
    let k_arr = 0..<n
    let i_arr = 0..<m
    let j_arr = 0..<p
    in transpose <| map(\j -> map (\i -> reduce (+) 0 
        <| map (\k -> A[i][k] * B[k][j]) k_arr) i_arr) j_arr :> [n][p]f64

def matpow [m] (A: [m][m]f64) (pow: i64): [m][m]f64 = 
    let identity_matrix: [m][m]f64 = map(\y -> map(\x -> if x == y then 1 else 0) (0..<m)) (0..<m)
    in reduce (matmul) identity_matrix <| replicate pow A

def main (A: [][]f64) (pow: i64): [][]f64 = 
    matpow A pow
