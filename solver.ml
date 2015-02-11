open Types

module Make : Solver_type =
  functor (Formula : Formula_type) ->
    struct
      let solve out form = None
    end
