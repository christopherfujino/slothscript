open Core
open Common

let make_spec ~program ~ast ?(stdout_expect = "") ?failure name =
  let stdout_expect = String.strip stdout_expect in
  { name; program; ast; stdout_expect; failure }

let green () =
  let stats = find_child_specs "./green_specs" in
  List.iter stats ~f:(fun name -> print_endline name);
  List.map stats ~f:Spec_parser.deserialize

let red =
  [
    make_spec "vars & funcs share namespace" ~program:"let x=1;func x(){}"
      ~ast:
        "((StmtDecl(LetStmt x(Num 1)))(FuncDecl(name \
         x)(parameters())(block())))"
      ~failure:Runtime_error;
    (* Although this works in JS, Python, and Perl, this seems like a mistake *)
    (* Go does not allow it. *)
    make_spec "closures cannot capture future vars"
      ~program:"func closure() {print(x);}let x = 1;closure();"
      ~ast:
        "zzz((FuncStmt(name closure)(parameters())(block((ExprStmt(FuncInvoc \
         print((IdRef x)))))))(LetStmt x(Num 1))(ExprStmt(FuncInvoc \
         closure())))"
      ~failure:Optimizer_error;
    make_spec "function declarations can only happen at the top level"
      ~program:"func f1() {let x = 1;func f2() {print(x);}f2();}f1();"
      ~ast:
        "((FuncStmt(name f1)(parameters())(block((LetStmt x(Num \
         1))(FuncStmt(name f2)(parameters())(block((ExprStmt(FuncInvoc(IdRef \
         print)((IdRef x)))))))(ExprStmt(FuncInvoc(IdRef \
         f2)())))))(ExprStmt(FuncInvoc(IdRef f1)())))"
      ~stdout_expect:"1\n" ~failure:Parser_error;
  ]
