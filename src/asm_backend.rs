use std::collections::HashMap;

use crate::{
    parser::{ASTBody, ASTExpr, BinaryOp, InASTExpr, InASTStatement},
    IdentIdx,
};

//const DEFAULT_TYPE: &str = "int64_t ";
type RbpOffset = i64;
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
        fn print_store(&mut self, to: RbpOffset, what: &str) {
            self.println(&format!("mov qword [rbp - {}], {}", to * 8, what));
        }
        // ex: "mov rax" NO COMMA!
        fn print_oper_on_load(&mut self, operation: &str, from: RbpOffset) {
            self.println(&format!("{}, [rbp - {}]", operation, from * 8))
        }
        fn print_load(&mut self, to_register: &str, from: RbpOffset) {
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
        fn body_to_asm(&mut self, body: &ASTBody, vars: &mut HashMap<IdentIdx, RbpOffset>) {
            let mut local_vars = Vec::new();
            for stat in body {
                match &stat.0 {
                    InASTStatement::EvalExpr(expr) => {
                        self.expr_to_asm(expr, vars, vars.len() as _);
                    }
                    InASTStatement::Function { name, args, body } => {
                        self.print_label(&self.function_to_asm_name(name));
                        // TODO: Add globals?
                        self.body_to_asm(body, &mut HashMap::new());
                    }
                    &InASTStatement::CreateVar(var_name) => {
                        // assert that variable identifier does not exist previously (should be handled in semantic analysis stage)
                        assert!(vars.insert(var_name, vars.len() as _).is_none());
                        local_vars.push(var_name);
                    }
                    InASTStatement::Return(expr) => {
                        let place = self.expr_to_asm(expr, vars, vars.len() as _);
                        self.print_load("rax", place);
                        self.println("ret");
                    }
                    InASTStatement::If { condition, body } => {
                        let after_if_label = self.get_tmp_label();
                        let label_name = self.tmp_label_to_asm_name(after_if_label);
                        // do condition
                        let cond_place = self.expr_to_asm(condition, vars, vars.len() as _);
                        self.print_load("rax", cond_place);
                        self.println("cmp rax, 0");
                        self.println(&format!("je {}", label_name)); // jump if 0
                        
                        // NOW: output if block, and then after label
                        self.body_to_asm(body, vars);
                        self.print_label(&label_name);
                    }
                    InASTStatement::While { condition, body } => {
                        let while_begin_label = self.get_tmp_label();
                        let while_end_label = self.get_tmp_label();
                        let begin_name = self.tmp_label_to_asm_name(while_begin_label);
                        let end_name = self.tmp_label_to_asm_name(while_end_label);
                        
                        self.print_label(&begin_name);
                        // do condition
                        let cond_place = self.expr_to_asm(condition, vars, vars.len() as _);
                        self.print_load("rax", cond_place);
                        self.println("cmp rax, 0");
                        self.println(&format!("je {}", end_name)); // jump if 0
                        
                        // output while block
                        self.body_to_asm(body, vars);

                        // jump back to beginning of while loop
                        self.println(&format!("jmp {}", begin_name));

                        self.print_label(&end_name); // end label
                    }
                }
            }
            for local in local_vars.iter() {
                vars.remove(local);
            }
        }
        /// temp_place says next place to place temporary values 
        fn expr_to_asm(&mut self, expr: &ASTExpr, vars: &mut HashMap<IdentIdx, RbpOffset>, temp_place: RbpOffset) -> RbpOffset {
            match &expr.0 {
                &InASTExpr::Int(num) => {
                    // store number into stack
                    self.print_store(temp_place, &format!("{}", num));
                    temp_place
                }
                &InASTExpr::VarName(name) => { // just return already existing place for variable
                    *vars.get(&name).unwrap() as _
                }
                InASTExpr::FunctionCall(name, args) => {
                    // TODO: put arguments somewhere so function can use them
                    self.println(&format!("call {}", self.function_to_asm_name(name)));
                    self.print_store(vars.len() as _, "rax"); // functions return from rax
                    vars.len() as _
                }
                InASTExpr::Binary(op, left, right) => {
                    match *op {
                        op @(BinaryOp::Set | BinaryOp::SetSub | BinaryOp::SetAdd)  => {
                            match left.0 {
                                InASTExpr::VarName(lvalue_ident) => {
                                    // store resulting expression into rax first
                                    let lvalue_place = *vars.get(&lvalue_ident).unwrap();
                                    let right_place = self.expr_to_asm(right, vars, temp_place);
                                    self.print_load("rax", right_place);
                                    if BinaryOp::SetSub == op {
                                        self.print_oper_on_load("sub rax", lvalue_place);
                                    }
                                    else if BinaryOp::SetAdd == op {
                                        self.print_oper_on_load("add rax", lvalue_place);
                                    }
                                    self.print_store(lvalue_place, "rax");
                                    -1 // This can never be used as a value so...
                                }
                                _ => panic!("left hand side MUST be lvalue") 
                            }
                        }
                        op @(BinaryOp::Multiply | BinaryOp::Add | BinaryOp::Sub | BinaryOp::Equals | BinaryOp::Less | BinaryOp::Greater) => {
                            self.expr_to_asm(left, vars, temp_place);
                            self.expr_to_asm(right, vars, temp_place + 1);
                            // load into registers
                            self.print_load("rax", temp_place);
                            self.print_load("rbx", temp_place + 1);
                            
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
                            self.print_store(temp_place, "rax");
                            temp_place
                        }
                    }
                }
            }
        }

    }

    let mut out = Output {
        code: String::new(),
        declarations: "section .text\n
        default rel\n
        global _start\n
        extern printf\n
        _start:\n
        mov rbp, rsp\n
        call main\n
        mov rax, 60\n
        mov edi, 0\n
    	syscall\n
        print_int:\n
        ret\n
        ".to_string(),
        tmp_labels: 0,
        ident_to_string,
    };

    out.body_to_asm(body, &mut HashMap::new());
    out.declarations.push_str(&out.code);
    out.declarations
}
