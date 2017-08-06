let internal_use_code = "
type _baseClass
"

let variadic_callback_code = "
(* variadic callback code *)
type ('a, 'b) variadic_arity8 = 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 *  'b 
constraint 'a = 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8
type ('a, 'b) variadic_arity7 = 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 *  'b *  'b
constraint 'a = 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7
type ('a, 'b) variadic_arity6 = 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 *  'b *  'b *  'b
constraint 'a = 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 
type ('a, 'b) variadic_arity5 = 'a1 * 'a2 * 'a3 * 'a4 * 'a5 *  'b *  'b *  'b *  'b
constraint 'a = 'a1 * 'a2 * 'a3 * 'a4 * 'a5 
type ('a, 'b) variadic_arity4 = 'a1 * 'a2 * 'a3 * 'a4 *  'b *  'b *  'b *  'b *  'b
constraint 'a = 'a1 * 'a2 * 'a3 * 'a4
type ('a, 'b) variadic_arity3 = 'a1 * 'a2 * 'a3 *  'b *  'b *  'b *  'b *  'b *  'b
constraint 'a = 'a1 * 'a2 * 'a3
type ('a, 'b) variadic_arity2 = 'a1 * 'a2 *  'b *  'b *  'b *  'b *  'b *  'b *  'b
constraint 'a = 'a1 * 'a2 
type ('a, 'b) variadic_arity1 = 'a1 *  'b *  'b *  'b *  'b *  'b *  'b *  'b *  'b
constraint 'a = 'a1

type 'a variadic_arity = 
  [
    | `Arity_9 of 'a1 * 'a2 * 'a3 * 'a4  * 'a5  * 'a6  * 'a7  * 'a8  * 'a9   
    | `Arity_8 of 'a1 * 'a2 * 'a3 * 'a4  * 'a5  * 'a6  * 'a7  * 'a8  
    | `Arity_7 of 'a1 * 'a2 * 'a3 * 'a4  * 'a5  * 'a6  * 'a7 
    | `Arity_6 of 'a1 * 'a2 * 'a3 * 'a4  * 'a5  * 'a6   
    | `Arity_5 of 'a1 * 'a2 * 'a3 * 'a4  * 'a5 
    | `Arity_4 of 'a1 * 'a2 * 'a3 * 'a4 
    | `Arity_3 of 'a1 * 'a2 * 'a3  
    | `Arity_2 of 'a1 * 'a2 
    | `Arity_1 of 'a1 
  ]
  constraint 'a =  'a1 * 'a2 * 'a3 * 'a4  * 'a5  * 'a6  * 'a7  * 'a8  * 'a9  

type ('a, 'c, 'b) variadic_callback = ('b, 'c)  Js_internal.fn constraint 'b = [<'a variadic_arity]

type ('a, 'b, 'c, 'd) variadic_callback1 = (('a, 'b) variadic_arity1, 'c, 'd) variadic_callback
type ('a, 'b, 'c, 'd) variadic_callback2 = (('a, 'b) variadic_arity2, 'c, 'd) variadic_callback
type ('a, 'b, 'c, 'd) variadic_callback3 = (('a, 'b) variadic_arity3, 'c, 'd) variadic_callback
type ('a, 'b, 'c, 'd) variadic_callback4 = (('a, 'b) variadic_arity4, 'c, 'd) variadic_callback
type ('a, 'b, 'c, 'd) variadic_callback5 = (('a, 'b) variadic_arity5, 'c, 'd) variadic_callback
type ('a, 'b, 'c, 'd) variadic_callback6 = (('a, 'b) variadic_arity6, 'c, 'd) variadic_callback
type ('a, 'b, 'c, 'd) variadic_callback7 = (('a, 'b) variadic_arity7, 'c, 'd) variadic_callback
type ('a, 'b, 'c, 'd) variadic_callback8 = (('a, 'b) variadic_arity8, 'c, 'd) variadic_callback
"

let get () = 
  internal_use_code ^ 
  Variadic.predefined_code ^ 
  Override.Predefined.code ^
  Optional.Predefined.code ^
  variadic_callback_code

