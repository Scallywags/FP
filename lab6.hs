module Lab6 where

import FP_Grammar
import FP_ParserGen
import FP_TypesEtc

myGrammar :: Grammar
myGrammar nt = case nt of

        Nmbr    -> [[ nmbr                                         ]]

        Op      -> [[ op                                           ]]

        Expr    -> [[ Nmbr                                         ]
                   ,[ Var                                          ]
                   ,[ lBracket, Expr, Op, Expr, rBracket           ]]
                   
        Stmnt   -> [[ Var, Terminal "=", Expr                      ]       -- Typecheck on op?
                   ,[ Terminal "repeat", Rep0 [Stmnt], Expr        ]]
                   
        Var     -> [[var]]
        
        Boolean -> [[boolean]]
        
        ResWord -> [[resWord]]
        
        Bracket -> [[bracket]]
        
        
var         = SyntCat Var
boolean     = SyntCat Boolean
resWord     = SyntCat ResWord
bracket     = SyntCat Bracket
             