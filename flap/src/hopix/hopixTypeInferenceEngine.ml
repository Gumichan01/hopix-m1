(** This module implements a type inference engine for Hopix.

    The type inference engine takes an implicitly-typed program
    as input and generates an explictly-typed version of this
    program, if it exists.

    To do so, we proceed in four steps:

    1. We first translate the implicitly-typed input program
       into an explicitly typed one where every missing type
       annotations is replaced with a "unification type variable",
       that is an unknown type variable.

    2. We generate a typing constraint out of the program obtained
       in the previous step.

    3. We solve the typing constraint using first-order unification.

    4. We then have 3 cases:
       (i) if the typing constraint is unsatisfiable, the input
           program is ill-typed ;
       (ii) if the typing constraint is satisfiable, we have two
            sub-cases:
           (a) the solved form of the constraint assigns a ground
               type to each unification type variable ;
           (b) the solved form of the constraint does not assign
               a ground to each unification type variable.

       Only the subcase (ii)-(b) allows us to generate an explicitly
       typed version of the program. Other cases produce errors and
       stop the compiler.

*)

open HopixTypes
open HopixAST

