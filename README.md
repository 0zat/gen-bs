# gen-bs
generate [bucklescript](https://github.com/BuckleScript/bucklescript) code from Javascript type specifications(now support [Web IDL](https://heycam.github.io/webidl)).  
In the future, d.ts files of [TypeScript](https://github.com/Microsoft/TypeScript) will be supported.

# Install
```
opam install webidl batteries
git clone https://github.com/0zat/gen-bs.git
cd gen-bs
ocamlbuild -use-ocamlfind gen_bs.byte
```
# Usage
```
./gen_bs.byte -dir <dir of Web IDL files>
```
* `-dir` option gathers Web IDL files from a directory, combines them and output bucklescript on the standard output.
* other command line options will come in the future.
* now the results are printed on the standard output only. Please use redirect to a file

## Usage example
```
gen_bs.byte -dir test/servo_idl > servo_dom.ml
```

# Generated code usage
* Please see [sample](https://github.com/0zat/gen-bs/blob/master/test/test.ml) which uses servo_dom.ml generated from [servo](https://github.com/servo/servo)
  * using getElementById, Performance API, SetTimeout
* Enum type is converted to polymorphic variant type  
* Union type is converted to polymorphic variant type
* Web IDL which mapped to Javascript object is converted to an abstract type and module
  * **\_like type is used represent inheritance and polymorphism
    * same as http://bucklescript.github.io/bucklescript/api/Dom.html
* Variadic argument is converted to Array type
* argguments are labbeled
  * a method which has optional argument has unit argument at the last of arguments
* enum, union, variadic is implemented by defining the same name function and convert argument and return value.
* overloaded methods are converted to 1 function
  * if converting argument to union is acceptable, then do it
  * else choose 1 method whose argument is longest
* example

|Web IDL|BuckleScript|
|:-----------:|:------------:|
|`enum {"a", "b", "c"}`| \[\`a \|\`b \|\`c\] |
|`(long or DOMString)`| \[\`Int of int \| \`String of string\] |
|`interface sample{  }` | `type sample ... module Sample = ...`|
|`long...`|`int array`|
|`long method_a(optional long arg)`|`let method_a ?arg () =`|
# Note
* This tool is experimental
* although BucklScript has DOM type definitions, this tool does not associate generated types with those types.
  * because some types in Web IDL are absent from BucklScript, inheritance relations could be fail.

# License
* Web IDL files under `test/servo_idl` are https://github.com/servo/servo/blob/master/LICENSE
* Web IDL files under `test/blink_idl` are https://chromium.googlesource.com/chromium/blink/+/master/LICENSE
* The Others are MIT
