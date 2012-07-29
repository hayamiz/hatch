
let translate lexbuf =
  Vm.compile
    (Highlevel.compile
       (Lambda.lambda_lift
          (Constfold.const_fold
             (Beta.beta_reduce
                (Normal.reduce_let
                   (Alpha.alpha_convert
                      (Normal.normalize
                         (Parser.main Lexer.token lexbuf))))))))
