open Common

let make_spec ~program ~ast ?(stdout_expect = "") ?failure name =
  { name; program; ast; stdout_expect; failure }

let green =
  [
    make_spec "empty program" ~program:"" ~ast:"()";
    make_spec "num literal" ~program:"11;" ~ast:"((StmtDecl(ExprStmt(Num 11))))";
    make_spec "print num" ~program:"print(11);"
      ~ast:"((StmtDecl(ExprStmt(FuncInvoc(IdRef print)((Num 11))))))"
      ~stdout_expect:"11\n";
    make_spec "string literal" ~program:"\"Hello\";"
      ~ast:"((StmtDecl(ExprStmt(String Hello))))";
    make_spec "print string" ~program:"print(\"Hello\");"
      ~ast:"((StmtDecl(ExprStmt(FuncInvoc(IdRef print)((String Hello))))))"
      ~stdout_expect:"Hello\n";
    make_spec "bool literal" ~program:"true;"
      ~ast:"((StmtDecl(ExprStmt(Bool true))))";
    make_spec "addition" ~program:"1 + 1;"
      ~ast:"((StmtDecl(ExprStmt(Binary Add(Num 1)(Num 1)))))";
    make_spec "chained infix calls" ~program:"1 + 2 + 3;" ~ast:"";
    make_spec "assignment" ~program:"let x = 1 + 1;"
      ~ast:"((StmtDecl(LetStmt x(Binary Add(Num 1)(Num 1)))))";
    make_spec "var reference" ~program:"let x = 1 + 1;\nprint(x);"
      ~ast:
        "((StmtDecl(LetStmt x(Binary Add(Num 1)(Num \
         1))))(StmtDecl(ExprStmt(FuncInvoc(IdRef print)((IdRef x))))))"
      ~stdout_expect:"2\n";
    make_spec "re-assignment" ~program:"let x = 0;x = 1;print(x);"
      ~ast:
        "((StmtDecl(LetStmt x(Num 0)))(StmtDecl(AssignStmt x(Num \
         1)))(StmtDecl(ExprStmt(FuncInvoc(IdRef print)((IdRef x))))))"
      ~stdout_expect:"1\n";
    make_spec "func definition" ~program:"func m() {}"
      ~ast:"((FuncDecl(name m)(parameters())(block())))";
    make_spec "func invocation" ~program:"func m() {print(1);print(2);}m();"
      ~ast:
        "((FuncDecl(name m)(parameters())(block((ExprStmt(FuncInvoc(IdRef \
         print)((Num 1))))(ExprStmt(FuncInvoc(IdRef print)((Num \
         2)))))))(StmtDecl(ExprStmt(FuncInvoc(IdRef m)()))))"
      ~stdout_expect:"1\n2\n";
    make_spec "func implicit return" ~program:"func m() {1;2;3;}print(m());"
      ~ast:
        "((FuncDecl(name m)(parameters())(block((ExprStmt(Num 1))(ExprStmt(Num \
         2))(ExprStmt(Num 3)))))(StmtDecl(ExprStmt(FuncInvoc(IdRef \
         print)((FuncInvoc(IdRef m)()))))))"
      ~stdout_expect:"3\n";
    make_spec "lexical scope"
      ~program:"let x=1;func f() {print(x);}func g() {let x=2;f();}g();"
      ~ast:
        "((StmtDecl(LetStmt x(Num 1)))(FuncDecl(name \
         f)(parameters())(block((ExprStmt(FuncInvoc(IdRef print)((IdRef \
         x)))))))(FuncDecl(name g)(parameters())(block((LetStmt x(Num \
         2))(ExprStmt(FuncInvoc(IdRef \
         f)())))))(StmtDecl(ExprStmt(FuncInvoc(IdRef g)()))))"
      ~stdout_expect:"1\n";
    make_spec "closures" ~program:"let x = 0;func f() {print(x);}x = 1;f();"
      ~ast:
        "((StmtDecl(LetStmt x(Num 0)))(FuncDecl(name \
         f)(parameters())(block((ExprStmt(FuncInvoc(IdRef print)((IdRef \
         x)))))))(StmtDecl(AssignStmt x(Num \
         1)))(StmtDecl(ExprStmt(FuncInvoc(IdRef f)()))))"
      ~stdout_expect:"1\n";
    make_spec "args" ~program:"func f(x, y) {print(x);print(y);}f(1, 2);"
      ~ast:
        "((FuncDecl(name f)(parameters(x y))(block((ExprStmt(FuncInvoc(IdRef \
         print)((IdRef x))))(ExprStmt(FuncInvoc(IdRef print)((IdRef \
         y)))))))(StmtDecl(ExprStmt(FuncInvoc(IdRef f)((Num 1)(Num 2))))))"
      ~stdout_expect:"1\n2\n";
    make_spec "first class func"
      ~program:"func f() {let x = 1;func() {x;};}let xer = f();print(xer());"
      ~ast:
        "((FuncDecl(name f)(parameters())(block((LetStmt x(Num \
         1))(ExprStmt(FuncExpr(parameters())(block((ExprStmt(IdRef \
         x)))))))))(StmtDecl(LetStmt xer(FuncInvoc(IdRef \
         f)())))(StmtDecl(ExprStmt(FuncInvoc(IdRef print)((FuncInvoc(IdRef \
         xer)()))))))"
      ~stdout_expect:"1\n";
    make_spec "curry" ~program:"func a(x) {func(y) {x+y;};}print(a(1)(2));"
      ~ast:
        "((FuncDecl(name \
         a)(parameters(x))(block((ExprStmt(FuncExpr(parameters(y))(block((ExprStmt(MethodInvoc(receiver(IdRef \
         x))(target +)(args((IdRef \
         y))))))))))))(StmtDecl(ExprStmt(FuncInvoc(IdRef \
         print)((FuncInvoc(FuncInvoc(IdRef a)((Num 1)))((Num 2))))))))"
      ~stdout_expect:"3\n";
    make_spec "if true" ~program:"if true {print(\"True\");};"
      ~ast:
        "((StmtDecl(ExprStmt(IfExpr(IfCont(conditional(Bool \
         true))(block((ExprStmt(FuncInvoc(IdRef print)((String \
         True))))))(continuation()))))))"
      ~stdout_expect:"True\n";
    make_spec "if false" ~program:"if false {print(\"Unreachable\");};"
      ~ast:
        "((StmtDecl(ExprStmt(IfExpr(IfCont(conditional(Bool \
         false))(block((ExprStmt(FuncInvoc(IdRef print)((String \
         Unreachable))))))(continuation()))))))";
    make_spec "if/else"
      ~program:"if false {print(true);} else {print(\"else\");};"
      ~ast:
        "((StmtDecl(ExprStmt(IfExpr(IfCont(conditional(Bool \
         false))(block((ExprStmt(FuncInvoc(IdRef print)((Bool \
         true))))))(continuation((ElseCont((ExprStmt(FuncInvoc(IdRef \
         print)((String else)))))))))))))"
      ~stdout_expect:"else\n";
    make_spec "if/else if/else"
      ~ast:
        "((StmtDecl(ExprStmt(IfExpr(IfCont(conditional(Bool \
         false))(block((ExprStmt(FuncInvoc(IdRef print)((String \
         unreachable))))))(continuation((IfCont(conditional(Bool \
         false))(block((ExprStmt(FuncInvoc(IdRef print)((String \
         unreachable))))))(continuation((ElseCont((ExprStmt(FuncInvoc(IdRef \
         print)((String finally))))))))))))))))"
      ~program:
        "if false {print(\"unreachable\");} else if false \
         {print(\"unreachable\");} else {print(\"finally\");};"
      ~stdout_expect:"finally\n";
  ]

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
