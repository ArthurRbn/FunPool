module Op where

op_sa :: ([a], [b]) -> ([a], [b])
op_sa ([], b) = ([], b)
op_sa (a, b)
    | length a == 1 = (a, b)
    | otherwise = ([head (tail a)] ++ [head a] ++ (drop 2 a), b)

op_sb :: ([a], [b]) -> ([a], [b])
op_sb (a, []) = (a, [])
op_sb (a, b)
    | length b == 1 = (a, b)
    | otherwise = (a, [head (tail b)] ++ [head b] ++ (drop 2 b))

op_sc :: ([a], [b]) -> ([a], [b])
op_sc l = op_sb (op_sa l)

op_pa :: ([Int], [Int]) -> ([Int], [Int])
op_pa (a, []) = (a, [])
op_pa (a, b) = (([head b] ++ a), (drop 1 b))

op_pb :: ([Int], [Int]) -> ([Int], [Int])
op_pb ([], b) = ([], b)
op_pb (a, b) = ((drop 1 a), ([head a] ++ b))

op_ra :: ([a], [b]) -> ([a], [b])
op_ra ([], b) = ([], b)
op_ra (a, b) = ((tail a) ++ [head a], b)

op_rb :: ([a], [b]) -> ([a], [b])
op_rb (a, []) = (a, [])
op_rb (a, b) = (a, (tail b) ++ [head b])

op_rr :: ([a], [b]) -> ([a], [b])
op_rr l = op_ra (op_rb l)

op_rra :: ([a], [b]) -> ([a], [b])
op_rra ([], b) = ([], b)
op_rra (a, b) = ([last a] ++ (take (length a - 1) a), b)

op_rrb :: ([a], [b]) -> ([a], [b])
op_rrb (a, []) = (a, [])
op_rrb (a, b) = (a, [last b] ++ (take (length b - 1) b))

op_rrr :: ([a], [b]) -> ([a], [b])
op_rrr l = op_rra (op_rrb l)