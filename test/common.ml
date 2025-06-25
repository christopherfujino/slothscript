type test_spec = {
  name : string;
  program : string;
  ast : string;
  stdout_expect : string;
  failure : failure_t option;
}

and failure_t = Parser_error | Optimizer_error | Runtime_error
