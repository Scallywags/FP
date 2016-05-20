{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module Lab6 where

import GHC.Generics
import FPPrac.Trees
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
                   
        Stmnt   -> [[ Var, assign, Expr                      ]       -- Typecheck on op?
                   ,[ rep, lBrace, Rep0 [Stmnt, semi], rBrace, Expr        ]]
                   
        Var     -> [[var]]
        
        Boolean -> [[boolean]]
        
        ResWord -> [[resWord]]
        
        Bracket -> [[bracket]]
        
        Brace   ->  [[brace]]
        
var         = SyntCat Var
boolean     = SyntCat Boolean
resWord     = SyntCat ResWord
bracket     = SyntCat Bracket
brace       = SyntCat Brace
rep         = Terminal "repeat"
lBrace      = Terminal "{"
rBrace      = Terminal "}"
semi        = Terminal ";"
assign      = Terminal "="

data GrammarTree   = ExprNum String
                   | ExprVar String
                   | ExprBool String
                   | ExprOp GrammarTree String GrammarTree
                   | StmntAss String GrammarTree
                   | StmntRep [GrammarTree] GrammarTree --TODO write appropriate toGrammarTree case.
                   deriving (Show, Eq, Generic, ToRoseTree)

toGrammarTree :: ParseTree -> GrammarTree
toGrammarTree (PLeaf (Nmbr, n, pos))            = ExprNum n
toGrammarTree (PLeaf (Var, s, pos))             = ExprVar s
toGrammarTree (PLeaf (Boolean, b, pos))         = ExprBool b
toGrammarTree (PNode Stmnt [PNode Var [PLeaf (Var, s, _)], PLeaf (Op, "=", _), e])  = StmntAss s (toGrammarTree e)
toGrammarTree (PNode Expr [PLeaf (Bracket, "(", _), e1, (PNode Op [PLeaf (Op, o, _)]), e2, PLeaf (Bracket, ")", _)]) = ExprOp (toGrammarTree e1) o (toGrammarTree e2)
toGrammarTree (PNode _ [sub])                   = toGrammarTree sub

