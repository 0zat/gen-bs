# gen-bs
generate [bucklescript](https://github.com/BuckleScript/bucklescript) code from Javascript type specifications(now support [Web IDL](https://heycam.github.io/webidl)).  
In the future, d.ts files of [TypeScript](https://github.com/Microsoft/TypeScript) will be supported.

# Install
```
opam install gen-bs
```
# Usage
```
gen-bs -dir <dir of Web IDL files>
```
* `-dir` option gathers Web IDL files from a directory, combines them and output bucklescript on the standard output.
* other command line options will come in the future.
* now the results are printed on the standard output only. Please use redirect to a file

## Usage example
```
gen-bs -dir test/servo_idl > servo_dom.ml
```

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

|Web IDL|BuckleScript|
|:-----------:|:------------:|
|`enum {"a", "b", "c"}`| \[\`a \|\`b \|\`c\] |
|`(long or DOMString)`| \[\`Int of int \| \`String of string\] |
|`interface Sample{  }` | `type _Sample ... module Sample = ...`|
|`long...`|`int array`|
|`long method_a(optional long arg, ...)`|`let method_a ?arg ... =`|
|`const VALUE=1`| `let _VALUE = 1`|

# Note
* This tool is experimental
* although BucklScript has DOM type definitions, this tool does not associate generated types with those types.
  * because some types in Web IDL are absent from BucklScript, inheritance relations could be fail.
* Performance could be degraded because generated code uses some wrapper functions
  * the reason using wrapper is to support nested type conversion
    * for example, bucklescript supports enum by [@bs.string] but does not support an array of enum

# Roadmap
* fix some known issues
  * mapping to buckescript dom type difinition
  * support callback function whose arguments are variadic  
* make an example using Reason
* make more test
  * use Travis CI
* automatically generate DOM Library and merge to https://github.com/0zat/bs-dom
* support d.ts files
# License
* Web IDL files under `test/servo_idl` are https://github.com/servo/servo/blob/master/LICENSE
* Web IDL files under `test/blink_idl` are https://chromium.googlesource.com/chromium/blink/+/master/LICENSE
* The Others are MIT
