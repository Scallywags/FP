module Lab6 where

import FP_Grammar
import FP_ParserGen
import FP_TypesEtc

myGrammar :: Grammar
myGrammar nt = case nt of

        Nmbr    -> [[ nmbr                                         ]]

        Op      -> [[ op                                           ]]

        Expr    -> [[ Expr, Op, Expr                               ]
                   ,[ Nmbr                                         ]
                   ,[ Var                                          ]
                   ,[ lBracket, Expr, rBracket                     ]]
                   
        Stmnt   -> [[ Var, Terminal "=", Expr                      ]       -- Typecheck on op?
                   ,[ Terminal "repeat", Rep0 [Stmnt], Expr        ]]
             