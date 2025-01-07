def ST (n: i64): i64 = 
    let S1 = 1 + 2**n + 3**n
    let S2 = 1 + 2**n + 4**n
    let S3 = S1 + 4**n + 5**n + 6**n
    in S3 - S2 - S1

def find_mult10_SF (m: i64): i64 =
    let ((_,_), mult10_at_m, _, _) = loop ((ST_second, ST_first), last_mult10, i, count) = ((ST 1, ST 2), 0, 1, 0) 
        while count < m do
            let SF = (ST_second - 5 * ST_first - 4) / 4
            in if SF %% 10 == 0
                then ((ST (i + 2), ST_second), SF, i + 1, count + 1)
               else ((ST (i + 2), ST_second), last_mult10, i + 1, count)
    in mult10_at_m

def main (n:i64): i64 = find_mult10_SF n
