open Common

let green =
  [
    {
      name = "empty program";
      program = "";
      ast = "()";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "num literal";
      program = "11;";
      ast = "((ExprStmt(Num 11)))";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "print num";
      program = "print(11);";
      ast = "((ExprStmt(FuncInvoc print((Num 11)))))";
      stdout_expect = "11\n";
      failure = None;
    };
    {
      name = "string literal";
      program = "\"Hello\";";
      ast = "((ExprStmt(String Hello)))";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "print string";
      program = "print(\"Hello\");";
      ast = "((ExprStmt(FuncInvoc print((String Hello)))))";
      stdout_expect = "Hello\n";
      failure = None;
    };
    {
      name = "bool literal";
      program = "true;";
      ast = "((ExprStmt(Bool true)))";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "addition";
      program = "1 + 1;";
      ast = "((ExprStmt(Binary Add(Num 1)(Num 1))))";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "assignment";
      program = "let x = 1 + 1;";
      ast = "((LetStmt x(Binary Add(Num 1)(Num 1))))";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "var reference";
      program = "let x = 1 + 1;\nprint(x);";
      ast =
        "((LetStmt x(Binary Add(Num 1)(Num 1)))(ExprStmt(FuncInvoc \
         print((IdRef x)))))";
      stdout_expect = "2\n";
      failure = None;
    };
    {
      name = "re-assignment";
      program = "let x = 0;x = 1;print(x);";
      ast =
        "((LetStmt x(Num 0))(AssignStmt x(Num 1))(ExprStmt(FuncInvoc \
         print((IdRef x)))))";
      stdout_expect = "1\n";
      failure = None;
    };
    {
      name = "func definition";
      program = "func m() {}";
      ast = "((FuncStmt(name m)(parameters())(block())))";
      stdout_expect = "";
      failure = None;
    };
    {
      name = "func invocation";
      program = "func m() {1;2;3;}print(m());";
      ast =
        "((FuncStmt(name m)(parameters())(block((ExprStmt(Num 1))(ExprStmt(Num \
         2))(ExprStmt(Num 3)))))(ExprStmt(FuncInvoc print((FuncInvoc m())))))";
      stdout_expect = "3\n";
      failure = None;
    };
    {
      name = "nested functions";
      program = "func f1() {let x = 1;func f2() {print(x);}f2();}f1();";
      ast =
        "((FuncStmt(name f1)(parameters())(block((LetStmt x(Num \
         1))(FuncStmt(name f2)(parameters())(block((ExprStmt(FuncInvoc \
         print((IdRef x)))))))(ExprStmt(FuncInvoc f2())))))(ExprStmt(FuncInvoc \
         f1())))";
      stdout_expect = "1\n";
      failure = None;
    };
    {
      name = "lexical scope";
      program = "let x=1;func f() {print(x);}func g() {let x=2;f();}g();";
      ast =
        "((LetStmt x(Num 1))(FuncStmt(name \
         f)(parameters())(block((ExprStmt(FuncInvoc print((IdRef \
         x)))))))(FuncStmt(name g)(parameters())(block((LetStmt x(Num \
         2))(ExprStmt(FuncInvoc f())))))(ExprStmt(FuncInvoc g())))";
      stdout_expect = "1\n";
      failure = None;
    };
    {
      name = "closures";
      program = "let x = 0;func f() {print(x);}x = 1;f();";
      ast =
        "((LetStmt x(Num 0))(FuncStmt(name \
         f)(parameters())(block((ExprStmt(FuncInvoc print((IdRef \
         x)))))))(AssignStmt x(Num 1))(ExprStmt(FuncInvoc f())))";
      stdout_expect = "1\n";
      failure = None;
    };
  ]

let red =
  [
    {
      name = "vars & funcs share namespace";
      program = "let x=1;func x(){}";
      ast = "((LetStmt x(Num 1))(FuncStmt(name x)(parameters())(block())))";
      stdout_expect = "";
      failure = Some Runtime_error;
    };
    {
      (* Although this works in JS, Python, and Perl, this seems like a mistake *)
      (* Go does not allow it. *)
      name = "closures cannot capture future vars";
      program = "func closure() {print(x);}let x = 1;closure();";
      ast =
        "((FuncStmt(name closure)(parameters())(block((ExprStmt(FuncInvoc \
         print((IdRef x)))))))(LetStmt x(Num 1))(ExprStmt(FuncInvoc \
         closure())))";
      stdout_expect = "";
      failure = Some Optimizer_error;
    };
  ]
