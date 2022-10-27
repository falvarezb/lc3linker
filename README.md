# LC-3 linker

Program to link LC-3 asm files

![build badge](https://codebuild.eu-west-1.amazonaws.com/badges?uuid=eyJlbmNyeXB0ZWREYXRhIjoiNXh0VVp1M3BYQUJqWEJIMW10U1d2QlVoR0g5dXd2ZnNpcVpFMVpmaFAvWkRib3BRd2NpRjBSUWlMWTB2SmFGQ01VakUzbmYyVTFyRm42ZjFPVG8yTTBFPSIsIml2UGFyYW1ldGVyU3BlYyI6IlhLby9BeXRnOHROOTdYQzUiLCJtYXRlcmlhbFNldFNlcmlhbCI6MX0%3D&branch=main)


## Description

This project is an assembler and linker written in Scala 3 for [Little Computer 3 (LC-3)](https://en.wikipedia.org/wiki/Little_Computer_3)

The experience of using LC-3 subroutines can be improved by having each subroutine in its own compilable unit, making it possible for different programs to re-use the same subroutines.
This approach requires a linker that assembles the code of each compilable unit and puts all the machine code into a single binary.

`lc3linker` is a simple linker that takes as parameters: 

- the main asm file
- asm files corresponding to the subroutines needed by the main program
- name of the resulting object file

It also generates a symbol table with the same name as the object file but suffix `.sym`

`lc3linker` cannot be used to:

- link previously existing object files
- generate individual object files for each asm file provided as argument


## Subroutines

As the code of the subroutines needs to be relocated, subroutine files cannot have `.ORIG` directives.

The main program, the one with `.ORIG`, must come first in the list of arguments

The code of the subroutines is relocated after the main program in the order in which they are provided in the list of arguments.

In case of long programs or having many subroutines, some PC-relative or base register-relative offsets can overflow. In such a case,
manually rearranging the order of the subroutines in the list of arguments may fix the problem.


### Example

The folder `src/test/resources` contains a few asm programs and subroutines that can be used to practise. 

The program __day_of_week.asm__ calculates the day of the week of a given date by using [Zeller's formula](https://en.wikipedia.org/wiki/Zeller%27s_congruence) and depends on the following subroutines:

- multiplication_routine.asm
- division_routine.asm
- read_multi_digit_routine.asm
- ascii_to_binary_routine.asm

To assemble __day_of_week.asm__, we need to link it with all its subroutines and specify the object file containing the final version of the code.

 
## How to run

### Docker image

The linker can be run in a docker image available in [Docker Hub](https://hub.docker.com/repository/docker/fjab76/lc3linker)

To make the asm files available to the container, the local folder containing the asm files must be mounted on 
the container's `/data` folder.

Assuming the docker command is run from the folder containing the asm files:

```
docker run --rm -v $(pwd):/data --name lc3linker fjab76/lc3linker:1.0.0 /data/day_of_week.asm /data/multiplication_routine.asm /data/division_routine.asm /data/read_multi_digit_routine.asm /data/ascii_to_binary_routine.asm /data/day_of_week.obj
```

For convenience, we provide the script `lc3linker.sh` to simplify the call to the docker command:

```
lc3linker.sh day_of_week.asm multiplication_routine.asm division_routine.asm read_multi_digit_routine.asm ascii_to_binary_routine.asm day_of_week.obj
```


### sbt

```
sbt run src/test/resources/day_of_week.asm src/test/resources/multiplication_routine.asm src/test/resources/division_routine.asm src/test/resources/read_multi_digit_routine.asm src/test/resources/ascii_to_binary_routine.asm src/test/resources/day_of_week.obj
```

## Appendix

### Implementation notes

The assembler is implemented as specified by [Introduction to Computing Systems: From bits and gates to C and beyond](https://highered.mheducation.com/sites/0072467509/)

__Note__: in addition to the instructions described in the specification, the assembler implemented in this project also supports [JMPT](https://acg.cis.upenn.edu/milom/cse240-Fall05/handouts/Ch09-a.pdf) and [RRT](https://acg.cis.upenn.edu/milom/cse240-Fall05/handouts/Ch09-a.pdf). These instructions are a variant of `JMP` and `RET`, respectively, that have the additional effect of setting the privilege bit in PS (Process Status Register).


### LC-3 emulators

The assembled/linked programs can be run in any of the multiple LC-3 emulators that can be found online:

- [lc3tools](https://highered.mheducation.com/sites/0072467509/student_view0/lc-3_simulator.html) contains several LC-3-related utilities, among them the VM emulator `lc3sim`
- there is also a [GUI](https://www.cis.upenn.edu/~milom/cse240-Fall05/handouts/lc3guide.html) for `lc3sim`
- and here's another [virtual machine](https://www.jmeiners.com/lc3-vm/)

`lc3sim` requires an [OS](https://acg.cis.upenn.edu/milom/cse240-Fall05/handouts/code/lc3os.asm) to run whereas the last VM has the OS built-in.
Additionally, `lc3sim` includes a debugger.
