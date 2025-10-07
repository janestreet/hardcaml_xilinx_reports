Synthesis reports
=================

# `Hardcaml_synthesis_estimates`

Provides a library which can take a hierarchical hardcaml design and run Xilinx
Vivado synthesis on each module in the design hierarchy.

Resource utilization and timing estimates are provided for each module.

The synthesis process for each module can be configured to report statistics for
just the local module, or for all modules below it as well. Running in different
ways can provide better insight into the design.

# Building the tool

See the example in the `bin` directory.  Roughly speaking it works as follows

```
module Synth = Hardcaml_xilinx_reports
module Command = Synth.Command.With_interface (Design.I) (Design.O)
let () = Command_unix.run (Command.command_basic ~name:"foo" Design.create)
```

Note that a command line app with a set of command line flags is generated. An
optional configuration record can be passed to tweak the synthesis projects.

# Running the tool

The current recommendation is to run twice as follows

```
./synth.exe -dir <output-dir> -hier -blackbox none -part <part> -clock <clock:freq> -run
```

and

```
./synth.exe -dir <output-dir> -hier -blackbox inst -part <part> -clock <clock:freq> -run
```

and

The former flow provides better timing estimates and performs more optimisation
on the design. It can track timing paths into submodules.

The later provides more accurate resource utilization estimates for individual
modules. It is also much, much quicker.

The only difference between the two is in the `-blackbox` argument.
