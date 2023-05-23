{-# LANGUAGE GADTs #-}

module LoliYul.CodeGen.PlantUML (compile) where

import qualified Data.Text           as T
import           LoliYul.Core.YulCat
import           Text.Printf

compile :: String -> YulCat a b -> T.Text
compile filename cat =
  get_header filename <>
  compile_cat cat <>
  get_footer

get_header :: String -> T.Text
get_header filename = T.pack $
  "/' PlantUML CodeGen for YulCat\n" <>
  "   Input file name: " <> filename <> " '/\n" <>
  "@startuml\n" <>
  "top to bottom direction\n"

get_footer :: T.Text
get_footer = T.pack "@enduml\n"

data Env = Env { node_counter :: Int
               }

inc_env :: Env -> Env
inc_env env@(Env id) = env { node_counter = id + 1 }

gen_obj :: String -> Env -> String
gen_obj objtype (Env id) = printf "%s_%d" objtype id

type NodeName = String
type NodeOutput = String
data Node = Node NodeName NodeOutput
type Code = T.Text

compile_cat :: YulCat a b -> T.Text
compile_cat cat = code <> T.pack (
  printf "%s -> [*] : %s\n" node out
  ) where (Node node out, code, _) = go_cat cat (Env 0) (Node "[*]" "inputs")

go_cat :: YulCat a b -> Env -> Node -> (Node, Code, Env)
go_cat YulIdentity env p = (p, T.pack "", env)
go_cat (YulCompose cat1 cat2) env (Node iname iout) =
  ( Node node1 out1
  , body2 <> body1
  , env3)
  where (Node node2 out2, body2, env2) = go_cat cat2 (inc_env env) (Node iname iout)
        (Node node1 out1, body1, env3) = go_cat cat1 env2 (Node node2 out2)
go_cat (YulProduct cat1 cat2) env (Node iname iout) =
  ( Node join_obj "product"
  , T.pack (
      printf "state %s <<fork>>\n" fork_obj <>
      printf "state %s <<join>>\n" join_obj <>
      printf "%s --> %s : %s\n" iname fork_obj iout <>
      printf "%s --> %s : %s\n" node1 join_obj out1 <>
      printf "%s --> %s : %s\n" node2 join_obj out2 ) <>
    body1 <> body2
  , env3
  ) where join_obj = gen_obj "pjoin" env
          fork_obj = gen_obj "pfork" env
          (Node node1 out1, body1, env2) = go_cat cat1 (inc_env env) (Node fork_obj iout)
          (Node node2 out2, body2, env3) = go_cat cat2 env2 (Node fork_obj iout)
go_cat YulUnitorL env (Node iname iout) =
  ( Node unitor_obj "a⊗()"
  , T.pack $ printf "%s --> %s: %s\n" iname unitor_obj iout
  , env) where unitor_obj = gen_obj "unil" env
go_cat YulUnitorR env (Node iname iout) =
  ( Node unitor_obj "a"
  , T.pack $ printf "%s --> %s: %s\n" iname unitor_obj iout
  , env) where unitor_obj = gen_obj "unir" env
go_cat YulAssocL env (Node iname iout) =
  ( Node assoc_obj "a⊗(b⊗c)"
  , T.pack $ printf "%s --> %s: %s\n" iname assoc_obj iout
  , env) where assoc_obj = gen_obj "assl" env
go_cat YulAssocR env (Node iname iout) =
  ( Node assoc_obj "(a⊗b)⊗c"
  , T.pack $ printf "%s --> %s: %s\n" iname assoc_obj iout
  , env) where assoc_obj = gen_obj "assr" env
go_cat YulSwap env (Node iname iout) =
  ( Node swap_obj "b⊗a"
  , T.pack $ printf "%s --> %s: %s\n" iname swap_obj iout
  , env) where swap_obj = gen_obj "swp" env
go_cat YulDis env (Node iname iout) =
  ( Node dis_obj "()"
  , T.pack $ printf "%s --> %s: %s\n" iname dis_obj iout
  , env) where dis_obj = gen_obj "dis" env
go_cat YulDup env (Node iname iout) =
  ( Node dup_obj "a⊗a"
  , T.pack $ printf "%s --> %s: %s\n" iname dup_obj iout
  , env) where dup_obj = gen_obj "dup" env
go_cat (YulConst a) env (Node iname iout) =
  ( Node const_obj (show a),
    T.pack $ printf "%s --> %s: %s\n" iname const_obj iout
  , env) where const_obj = gen_obj "const" env
go_cat YulSGet env (Node iname iout) =
  ( Node obj "a",
    T.pack $ printf "%s --> %s: %s\n" iname obj iout
  , env) where obj = gen_obj "sget" env
go_cat YulSPut env (Node iname iout) =
  ( Node obj "()",
    T.pack $ printf "%s --> %s: %s\n" iname obj iout
  , env) where obj = gen_obj "sput" env
go_cat YulNumPlus env (Node iname iout) =
  ( Node obj "()",
    T.pack $ printf "%s --> %s: %s\n" iname obj iout
  , env) where obj = gen_obj "nplus" env
go_cat YulNumMinus env (Node iname iout) =
  ( Node obj "()",
    T.pack $ printf "%s --> %s: %s\n" iname obj iout
  , env) where obj = gen_obj "nminus" env
go_cat YulDetuple env (Node iname iout) =
  ( Node det_obj "...",
    T.pack $ printf "%s --> %s: %s\n" iname det_obj iout
  , env) where det_obj = gen_obj "detuple" env
go_cat YulJust env (Node iname iout) =
  ( Node just_obj "Just a",
    T.pack $ printf "%s --> %s: %s\n" iname just_obj iout
  , env) where just_obj = gen_obj "detuple" env
go_cat (YulFunc name body) env (Node iname iout) =
  ( Node exit_obj "returns"
  , T.pack (printf "state \"function %s\" as %s {\n"  (T.unpack name) func_obj) <>
    body' <>
    T.pack (
      printf "state %s <<expansionInput>>\n" entry_obj <>
      printf "state %s <<expansionOutput>>\n" exit_obj <>
      printf "%s -> %s : %s\n" node' exit_obj out' <>
      "}\n") <>
    T.pack (printf "%s -> %s : %s\n" iname entry_obj iout)
  , env') where func_obj = gen_obj "func" env
                entry_obj = gen_obj "entry" env
                exit_obj = gen_obj "exit" env
                (Node node' out', body', env') = go_cat body (inc_env env) (Node entry_obj "params")
