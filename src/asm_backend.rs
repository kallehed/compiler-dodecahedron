use std::collections::HashMap;

use crate::{
    parser::{ASTBody, ASTExpr, BinaryOp, InASTExpr, InASTStatement},
    IdentIdx,
};

//const DEFAULT_TYPE: &str = "int64_t ";
type RspOffset = i64;
type TmpLabel = i64;

/// return values in RAX register. Everything else on stack using RBP (base pointer)
pub fn to_asm(body: &ASTBody, ident_to_string: &[&'static str]) -> String {
    struct Output<'a> {
        code: String,
        declarations: String,

        tmp_labels: TmpLabel,

        // immutable
        ident_to_string: &'a [&'static str],
    }

    impl<'a> Output<'a> {
        fn println(&mut self, to_out: &str) {
            self.code.push_str(to_out);
            self.code.push('\n');
        }
        /// "main" -> "main:\n"
        fn print_label(&mut self, label: &str) {
            self.code.push_str(label);
            self.println(":");
        }
        fn print_store(&mut self, to: RspOffset, what: &str) {
            self.println(&format!("mov qword [rsp - {}], {}", to * 8, what));
        }
        // ex: "mov rax" NO COMMA!
        fn print_oper_on_load(&mut self, operation: &str, from: RspOffset) {
            self.println(&format!("{}, [rsp - {}]", operation, from * 8))
        }
        fn print_load(&mut self, to_register: &str, from: RspOffset) {
            self.print_oper_on_load(&format!("mov {}", to_register), from);
        }
        fn get_tmp_label(&mut self) -> TmpLabel {
            let tmp = self.tmp_labels;
            self.tmp_labels += 1;
            tmp
        }
        fn tmp_label_to_asm_name(&self, label: TmpLabel) -> String {
            format!(".L{}", label)
        }

        fn function_to_asm_name(&self, func: &IdentIdx) -> String {
            let real_name = self.ident_to_string[*func as usize];
            match real_name {
                "print_int" => "print_int".to_string(),
                "main" => "main".to_string(),
                _ => format!("func{}", func),
            }
        }

        // vars holds the previous local variables, and will be expanded upon, but new ones will be removed at end of scope
        // holds relative position from rbp to the variable on the stack
        // tmp_place should start at 1, as on 0 we have the return address.
        fn body_to_asm(
            &mut self,
            body: &ASTBody,
            vars: &mut HashMap<IdentIdx, RspOffset>,
            mut tmp_place: RspOffset,
        ) {
            let mut local_vars = Vec::new();
            for stat in body {
                match &stat.0 {
                    InASTStatement::EvalExpr(expr) => {
                        self.expr_to_asm(expr, vars, tmp_place);
                    }
                    InASTStatement::Function { name, args, body } => {
                        self.print_label(&self.function_to_asm_name(name));
                        // TODO: Add globals?
                        let mut func_vars = HashMap::new();
                        for (idx, &arg) in args.iter().enumerate() {
                            func_vars.insert(arg, idx as RspOffset + 1); // 0 is for return address
                        }
                        self.body_to_asm(body, &mut func_vars, 1 + args.len() as RspOffset);
                    }
                    &InASTStatement::CreateVar(var_name) => {
                        // assert that variable identifier does not exist previously (should be handled in semantic analysis stage)
                        assert!(vars.insert(var_name, tmp_place).is_none());
                        tmp_place += 1;
                        local_vars.push(var_name);
                    }
                    InASTStatement::Return(expr) => {
                        let place = self.expr_to_asm(expr, vars, tmp_place);
                        self.print_load("rax", place);
                        self.println("ret");
                    }
                    InASTStatement::If {
                        condition,
                        if_true_body,
                        if_false_body,
                    } => {
                        let label_else = self.get_tmp_label();
                        let label_else_name = self.tmp_label_to_asm_name(label_else);
                        let label_end = self.get_tmp_label();
                        let label_end_name = self.tmp_label_to_asm_name(label_end);

                        //           Compute conditional
                        let cond_place = self.expr_to_asm(condition, vars, tmp_place);
                        self.print_load("rax", cond_place);
                        //           jump to ELSE if 0
                        self.println("cmp rax, 0");
                        self.println(&format!("je {}", label_else_name)); // jump if 0

                        //           "true code"
                        self.body_to_asm(if_true_body, vars, tmp_place);
                        //       jump to END
                        self.println(&format!("jmp {}", label_end_name));

                        //           label: ELSE
                        self.print_label(&label_else_name);
                        //       "false code"
                        self.body_to_asm(if_false_body, vars, tmp_place);
                        //       label: END
                        self.print_label(&label_end_name);
                    }
                    InASTStatement::While { condition, body } => {
                        let while_begin_label = self.get_tmp_label();
                        let while_end_label = self.get_tmp_label();
                        let begin_name = self.tmp_label_to_asm_name(while_begin_label);
                        let end_name = self.tmp_label_to_asm_name(while_end_label);

                        self.print_label(&begin_name);
                        // do condition
                        let cond_place = self.expr_to_asm(condition, vars, tmp_place);
                        self.print_load("rax", cond_place);
                        self.println("cmp rax, 0");
                        self.println(&format!("je {}", end_name)); // jump if 0

                        // output while block
                        self.body_to_asm(body, vars, tmp_place);

                        // jump back to beginning of while loop
                        self.println(&format!("jmp {}", begin_name));

                        self.print_label(&end_name); // end label
                    }
                    InASTStatement::Block(block) => {
                        self.body_to_asm(block, vars, tmp_place);
                    }
                }
            }
            for local in local_vars.iter() {
                vars.remove(local);
            }
        }
        /// temp_place says next place to place temporary values
        fn expr_to_asm(
            &mut self,
            expr: &ASTExpr,
            vars: &mut HashMap<IdentIdx, RspOffset>,
            tmp_place: RspOffset,
        ) -> RspOffset {
            match &expr.0 {
                &InASTExpr::Int(num) => {
                    // store number into stack
                    self.print_store(tmp_place, &format!("{}", num));
                    tmp_place
                }
                &InASTExpr::VarName(name) => {
                    // just return already existing place for variable
                    *vars.get(&name).unwrap() as _
                }
                InASTExpr::FunctionCall(name, args) => {
                    // TODO: put arguments somewhere so function can use them
                    let mut place_args = tmp_place;
                    let mut places = Vec::new();
                    for arg in args {
                        let place = self.expr_to_asm(arg, vars, place_args);
                        places.push(place);
                        if place == place_args {
                            place_args += 1;
                        }
                    }
                    // place arguments onto beginning of functions stack
                    for (idx, &place) in places.iter().enumerate().rev() {
                        self.print_load("rax", place);
                        // place arguments onto function stack, +1 bc fit return address
                        self.print_store(1 + tmp_place + (idx as RspOffset), "rax");
                    }

                    // move rsp down, so locals + tmps are preserved, so call will place current instr at legal place on stack
                    let stack_move = (tmp_place - 1) * 8; // call moves one step down as well, so we only need to move tmp_place - 1.
                    self.println(&format!("sub rsp, {}", stack_move));
                    self.println(&format!("call {}", self.function_to_asm_name(name)));
                    self.println(&format!("add rsp, {}", stack_move));
                    self.print_store(tmp_place, "rax"); // functions return from rax
                    tmp_place
                }
                InASTExpr::Binary(op, left, right) => {
                    match *op {
                        op @ (BinaryOp::Set | BinaryOp::SetSub | BinaryOp::SetAdd) => {
                            match left.0 {
                                InASTExpr::VarName(lvalue_ident) => {
                                    // store resulting expression into rax first
                                    let lvalue_place = *vars.get(&lvalue_ident).unwrap();
                                    let right_place = self.expr_to_asm(right, vars, tmp_place);
                                    self.print_load("rax", right_place);
                                    if BinaryOp::SetSub == op {
                                        self.print_oper_on_load("sub rax", lvalue_place);
                                    } else if BinaryOp::SetAdd == op {
                                        self.print_oper_on_load("add rax", lvalue_place);
                                    }
                                    self.print_store(lvalue_place, "rax");
                                    -1 // This can never be used as a value so...
                                }
                                _ => panic!("left hand side MUST be lvalue"),
                            }
                        }
                        op @ (BinaryOp::Multiply
                        | BinaryOp::Add
                        | BinaryOp::Sub
                        | BinaryOp::Equals
                        | BinaryOp::Less
                        | BinaryOp::Greater) => {
                            // have to load both into memory first, as they will rewrite registers
                            let left_place = self.expr_to_asm(left, vars, tmp_place);
                            let right_place = self.expr_to_asm(
                                right,
                                vars,
                                if left_place < tmp_place {
                                    tmp_place
                                } else {
                                    tmp_place + 1
                                },
                            );
                            // load into registers
                            self.print_load("rax", left_place);
                            self.print_load("rbx", right_place);

                            match op {
                                BinaryOp::Multiply => self.println("imul rax, rbx"),
                                BinaryOp::Add => self.println("add rax, rbx"),
                                BinaryOp::Sub => self.println("sub rax, rbx"),
                                BinaryOp::Equals => {
                                    self.println("cmp rax, rbx");
                                    self.println("sete al"); // set lowest byte of rax to 1 if equal
                                    self.println("movzx rax, al"); // zero extend to rax
                                }
                                BinaryOp::Less => {
                                    self.println("cmp rax, rbx");
                                    self.println("setl al"); // set lowest byte of rax to 1 if equal
                                    self.println("movzx rax, al"); // zero extend to rax
                                }
                                BinaryOp::Greater => {
                                    self.println("cmp rax, rbx");
                                    self.println("setg al"); // set lowest byte of rax to 1 if equal
                                    self.println("movzx rax, al"); // zero extend to rax
                                }
                                _ => unreachable!(),
                            }
                            self.print_store(tmp_place, "rax");
                            tmp_place
                        }
                    }
                }
            }
        }
    }

    let mut out = Output {
        code: String::new(),
        declarations: "
section .data\n
    hello_message db \"%ld\", 10, 0\n
        section .text\n
        default rel\n     ; important so we don't have to write rel everywhere
        global _start\n
        extern printf\n
        extern fflush\n
        extern stdout\n
        _start:\n
        call main\n         ; call main
        mov rdi, [stdout]\n       ; call fflush
        call fflush\n
        mov rax, 60\n      ; exit program
        mov edi, 0\n
    	syscall\n
        print_int:\n
        mov rdi, hello_message \n
        mov rax, 0\n
        mov rsi, [rsp - 8]\n
        call printf\n
        ret\n
        "
        .to_string(),
        tmp_labels: 0,
        ident_to_string,
    };

    // 0 because global scope doesn't have stack
    out.body_to_asm(body, &mut HashMap::new(), 0);
    out.declarations.push_str(&out.code);
    out.declarations
}
