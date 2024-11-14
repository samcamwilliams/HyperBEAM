## Information about the WASM files

### pow_calculator.wasm

Exports:

-   pow/2 takes a base and exponent and returns power of the number. Calls the `my_lib:mul/2` function in a loop in
    order to perform pow calculation

Imports:

-   `my_lib:mul(arg1, arg2) -> arg1 * arg2`
