type 'a ft_ref = { mutable value : 'a }

let return value = { value }
let get { value } = value
let set self newValue = ignore (self.value <- newValue)
let bind { value } cb = return (cb value)

let () =
  let foo = return 1 in
  print_int (get foo);
  print_newline ();
  set foo 42;
  print_int (get foo);
  print_newline ();
  let bar = bind foo (fun x -> string_of_int (x + 27)) in
  print_endline (get bar)
