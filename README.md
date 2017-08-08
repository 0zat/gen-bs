# gen-bs
generate [bucklescript](https://github.com/BuckleScript/bucklescript) code from Javascript type specifications(now support [Web IDL](https://heycam.github.io/webidl)).  
In the future, d.ts files of [TypeScript](https://github.com/Microsoft/TypeScript) will be supported.

# usacase
* You can generate DOM Library which has latest functions implemented in browsers by Web IDL
* In the future, you can use TypeScript Libraries on bucklescript by converting d.ts files. (maybe)

# Install
```
opam install gen-bs
```
# Usage
* after version 0.1.0 , some options are available. 
```
gen-bs <Web IDL file>
```
* input a Web IDL file and output on standard output by default
## Available options
|option|meaning|example|
|:-----------:|:------------:|:------------:|
|`-d` / `--dir`| input from Web IDL files in a directory | `gen-bs -d dir-name` |
|`-o` / `--output`| output to a file | `gen-bs -o output-file.ml input.webidl` |
|`-t` / `--type`| input json file which external defined types are written in. json format is eplained later | `gen-bs -t types.json input.webidl` |
|`-v` / `--version`| show version | `gen-bs -v` |
|`-T` / `--trace`| print backtrace when an error occurs  | `gen-bs -T input.webidl` |
## external type difinition
* in order to generate cast function for external defined types, you can use the following format json file.
```
[
  [<type name in generated bucklescript>, <external defined type name>],
  ["_Window", "Dom.window"],
  ...
]
```
  * input this file by `-t` option
  * by this json, cast function (regarding with the bellow example, `Window.to_Dom_window` function) will be generated

# Generated code usage
* Please see [firework example](https://github.com/0zat/gen-bs/tree/master/example/firework) which uses servo_dom.ml generated from [servo](https://github.com/servo/servo)
  * you can see the result page at https://0zat.github.io/gen-bs/firework
## Conversion rule
* Enum type is converted to polymorphic variant type  
* Union type is converted to polymorphic variant type
* Web IDL which mapped to Javascript object is converted to an abstract type and module
  * **\_like type is used represent inheritance and polymorphism
    * same as http://bucklescript.github.io/bucklescript/api/Dom.html
* Variadic argument is converted to Array type
* argguments are labbeled
* enum, union, variadic is implemented by wrapper function
  * defining the same name function and convert arguments and return value.
* overloaded methods are converted to 1 function
  * if converting argument to union is acceptable, then do it  
  * else choose 1 method whose argument length is longest and throw away other methods
* constants are defined in a module
* example

|category|Web IDL|BuckleScript|
|:-----------:|:-----------:|:------------:|
|enum|`enum {"a", "b", "c"}`| \[\`a \|\`b \|\`c\] |
|union|`(long or DOMString)`| \[\`Int of int \| \`String of string\] |
|object|`interface Sample{  }` | `type _Sample ... module Sample = ...`|
|variadic argument|`long...`|`int array`|
|optional argument|`long method_a(optional long arg, ...)`|`let method_a ?arg ... =`|
|constant|`const VALUE=1`| `let _VALUE = 1`|

# Note
* This tool is experimental
* although BucklScript has DOM type definitions, this tool does not associate generated types with those types.
  * because some types in Web IDL are absent from BucklScript, inheritance relations could be fail.
* Performance could be degraded because generated code uses some wrapper functions
  * the reason using wrapper is to support nested type conversion
    * for example, bucklescript supports enum by [@bs.string] but does not support an array of enum

# Roadmap
* use compilerlibs to make bucklescript code and print them
* make an example using Reason
* make more test
  * use Travis CI
* automatically generate DOM Library and merge to https://github.com/0zat/bs-dom
* support d.ts files
# License
* Web IDL files under `test/servo_idl` are https://github.com/servo/servo/blob/master/LICENSE
* Web IDL files under `test/blink_idl` are https://chromium.googlesource.com/chromium/blink/+/master/LICENSE
* The Others are MIT
