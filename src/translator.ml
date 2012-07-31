
let translate lexbuf =
  Vm.compile
    (Highlevel.compile
       (Lambda.lift_lambda
          (Constfold.const_fold
             (Beta.beta_reduce
                (Normal.serialize_let
                   (Alpha.alpha_convert
                      (Normal.normalize
                         (Parser.main Lexer.token lexbuf))))))))
