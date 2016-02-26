let initialize () =
  Compilers.register (module Compilers.Identity (Fopix));
  Compilers.register (module HobixToFopix)
