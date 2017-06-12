module ModuleTest where

import SlamData.Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as Console
import Data.Path.Pathy as Path
import Data.StrMap as SM
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Eval.Process as Process
import SqlSquared as Sql

sql ∷ String → Sql.Sql
sql str = unsafePartial $ fromRight $ Sql.parse str

query ∷ String → Sql.SqlQuery
query str = unsafePartial $ fromRight $ Sql.parseQuery str

type CardEvalStep =
  { query ∷ Sql.SqlQuery
  , source ∷ CID.CardId
  }

card1 ∷ CardEvalStep
card1 =
  { source: CID.legacyFromInt 1
  , query: query """
      SELECT * FROM :resource
      WHERE country = :country AND type = :type
    """
  }

card2 ∷ CardEvalStep
card2 =
  { source: CID.legacyFromInt 2
  , query: query """
      SELECT row.country AS country, row.type AS type FROM :resource AS row
      WHERE foo = :country
      GROUP BY country
      ORDER BY country
    """
  }

card3 ∷ CardEvalStep
card3 =
  { source: CID.legacyFromInt 3
  , query: query """
      SELECT DISTINCT(gender) FROM :resource
    """
  }

runCard ∷ Port.VarMap → CardEvalStep → String × Port.VarMap
runCard varMap step =
  let
    fpath =
      Path.currentDir Path.</>
      Path.dir ("out" <> CID.toString step.source) Path.</>
      Path.file (Process.processIdent step.source)
  in
    case Process.elaborate step.source varMap step.query of
      fileName × Right m → Sql.printModule m × VM.insert step.source (VM.Var "resource") (VM.Resource $ VM.Process fpath m varMap) varMap
      fileName × Left q  → Sql.printQuery q × VM.insert step.source (VM.Var "resource") (VM.Resource $ Port.View fpath q varMap) varMap

main ∷ Eff _ Unit
main = do
  let
    vm1 = SM.fromFoldable
      [ "country" × pure (CID.legacyFromInt 0 × VM.Expr (sql "\"USA\""))
      , "type" × pure (CID.legacyFromInt 0 × VM.Expr (sql "\"Gold\""))
      , "resource" × pure (CID.legacyFromInt 0 × VM.Expr (sql "`/mongodb/olympics`"))
      ]
    q1 × vm2 = runCard vm1 card1
    q2 × vm3 = runCard vm2 card2
    q3 × vm4 = runCard vm3 card3

  Console.log ""
  Console.log q1
  Console.log ""
  Console.log q2
  Console.log ""
  Console.log q3
  Console.log ""
  traceAnyA (VM.toURLVarMap vm4)
  Console.log ""
