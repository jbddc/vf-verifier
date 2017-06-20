# vf-verifier
Practical Assignment - Formal Verification 2016/17

Documentation for the source code can be found [here](https://jbdc.github.io/).

## Build

- Clone this repository
- *cd* to its root folder
- *stack setup* (only needed once)
- *stack build* 

## Executing program

You need a file with SL code, some examples can be found in [test](https://github.com/jbddc/vf-verifier/tree/master/test).
Then you may run *stack exec vf-verifier* and pipe your file.sl into the executable as follows:

*stack exec vf-verifier < my_file.sl*
