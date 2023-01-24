max_size_str equ 100
start_location ds.l 1
end_location ds.l 1
digit dc.b 0,0 
instruction_str dcb.b max_size_str,0 ; inialize with 0
data_str dcb.b max_size_str,0 ; inialize with 0