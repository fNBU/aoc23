import Lake
open Lake DSL

package «aoc23» where
  -- add package configuration options here

lean_lib «Aoc23» where
  -- add library configuration options here

@[default_target]
lean_exe «aoc23» where
  root := `Main
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  supportInterpreter := true

require std from git "https://github.com/leanprover/std4" @ "v4.3.0"
require mathlib from git "https://github.com/leanprover-community/mathlib4" @ "v4.3.0"
