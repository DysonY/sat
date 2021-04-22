(* Read input in CNF format *)

let read_file filename =
  let chan = open_in filename in
  let s = really_input_string chan (in_channel_length chan) in
  close_in chan;
  print_string s

let _ = read_file Sys.argv.(1)
