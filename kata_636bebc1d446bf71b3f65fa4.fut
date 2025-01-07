def check_for_element_eq (x: i64) (as: []i64): i64 = 
    let bool_sum = reduce (+) 0 <|
        map (\a -> if a == x then 1 else 0) as
    in  bool_sum

def check_for_element_leq (x: i64) (as: []i64): i64 = 
    let bool_sum = reduce (+) 0 <|
        map (\a -> if a < x then 1 else 0) as
    in bool_sum

def sequence_A (n: i64) : i64 =
    let B_seq = 2..<1001
    let (A_last, _, _, _) =
        loop (A_seq, comparison_arr, i, j) = (1, [], 1, 0)
        for i < n - 1 do
            let comparison_appended = concat comparison_arr [A_seq]
            let (B_second, j_appended, flush) =
                if check_for_element_eq B_seq[j] comparison_appended != 0
                    then if check_for_element_leq B_seq[j] comparison_appended != 0 
                            then (B_seq[j+1], j+2, true)
                         else (B_seq[j+1], j+2, false)
                else (B_seq[j], j+1, false)

        let A_second = A_seq + B_second
        in if flush then (A_second, [], i + 1, j_appended)
           else (A_second, comparison_appended, i + 1, j_appended)
    in A_last

def main (n: i64) : i64 = sequence_A n
