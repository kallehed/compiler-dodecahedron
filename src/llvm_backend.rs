use std::collections::HashMap;
use std::ffi::CStr;

use crate::ir::{IRFunc, Instr, InstrIterator};
use crate::lexer::IntStor;

use llvm_sys::bit_writer::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{analysis::*, LLVMOpcode};
use llvm_sys::{LLVMCallConv, LLVMLinkage};

pub unsafe fn llvm_gen(
    ir: &mut InstrIterator,
    ir_functions: &[IRFunc],
    intstor: &IntStor,
    ident_idx_to_string: &Vec<&'static str>,
) {
    let ctx = LLVMContextCreate();
    let md = LLVMModuleCreateWithNameInContext(c"yolo".as_ptr(), ctx);
    let builder = LLVMCreateBuilderInContext(ctx);
    let int = LLVMInt64TypeInContext(ctx);

    struct Func {
        func: LLVMValueRef,
        typ: LLVMTypeRef,
    }

    let mut my_funcs = vec![];

    for func in ir_functions.iter() {
        let mut params = vec![int; func.params as usize];

        let func_type = LLVMFunctionType(int, params.as_mut_ptr(), func.params as u32, 0);

        let real_name = ident_idx_to_string[func.deadname as usize];
        let mut real_name = real_name.to_owned();
        real_name.push('\0');

        let my_func = LLVMAddFunction(md, real_name.as_ptr() as *const i8, func_type);
        LLVMSetFunctionCallConv(my_func, LLVMCallConv::LLVMCCallConv as u32);
        LLVMSetLinkage(my_func, LLVMLinkage::LLVMExternalLinkage);

        my_funcs.push(Func {
            func: my_func,
            typ: func_type,
        })
    }

    let mut cur_regs = vec![];
    let mut cur_func: &Func = &my_funcs[0];
    macro_rules! get_reg {
        ($e:expr) => {
            cur_regs[$e as usize]
        };
    }
    // lazy label basic_block creation when we either find a label or jump to a label
    let mut our_labels = HashMap::new();

    macro_rules! get_label_bb {
        ($label:expr) => {{
            if let Some(&bb) = our_labels.get(&$label) {
                bb
            } else {
                let new_bb =
                    LLVMAppendBasicBlockInContext(ctx, cur_func.func, c"le_label".as_ptr());
                our_labels.insert($label, new_bb);
                new_bb
            }
        }};
    }

    // it's not legal to generate code after a branch or a return.
    // also use this to generate some extra branches if a block had no code.
    let mut legal_to_gen = true;

    while let Some(instr) = ir.next() {
        if !legal_to_gen {
            match instr {
                Instr::FuncDef(_) | Instr::EndFunc | Instr::Label(_) => (),
                _ => continue,
            }
        }
        match instr {
            Instr::LoadReg(into, from) => {
                let le_val = LLVMBuildLoad2(builder, int, get_reg![from], c"from_le".as_ptr());
                LLVMBuildStore(builder, le_val, get_reg![into]);
            }
            Instr::LoadInt(reg, iidx) => {
                let val = LLVMConstInt(int, intstor.get(iidx) as _, 1);
                LLVMBuildStore(builder, val, get_reg![reg]);
            }
            Instr::Op(into, op, left, right) => {
                let left = LLVMBuildLoad2(builder, int, get_reg![left], c"lef".as_ptr());
                let right = LLVMBuildLoad2(builder, int, get_reg![right], c"rig".as_ptr());
                let res = LLVMBuildBinOp(
                    builder,
                    LLVMOpcode::LLVMAdd,
                    left,
                    right,
                    c"res_of_binop".as_ptr(),
                );
                LLVMBuildStore(builder, res, get_reg![into]);
            }
            Instr::Call(to, fnidx, args) => {
                let func = &my_funcs[fnidx.0 as usize];
                let ir_func = &ir_functions[fnidx.0 as usize];
                let mut ll_args = vec![];
                for &arg in args.iter() {
                    let ssa_arg = LLVMBuildLoad2(builder, int, get_reg![arg], c"arg".as_ptr());
                    ll_args.push(ssa_arg);
                }
                let res = LLVMBuildCall2(
                    builder,
                    func.typ,
                    func.func,
                    ll_args.as_mut_ptr(),
                    ir_func.params as u32,
                    c"le_call".as_ptr(),
                );
                LLVMBuildStore(builder, res, get_reg![to]);
            }
            Instr::Jump(label) => {
                LLVMBuildBr(builder, get_label_bb!(label));
                legal_to_gen = false;
            }
            // if we get here, we will create a new block, but since we haven't returned or branched yet
            // we don't need to set ret_hack to anything, it's already true
            Instr::JumpRegZero(reg, label) => {
                let the_reg = LLVMBuildLoad2(builder, int, get_reg![reg], c"getit".as_ptr());
                let cmp = LLVMBuildICmp(
                    builder,
                    llvm_sys::LLVMIntPredicate::LLVMIntEQ,
                    LLVMConstInt(int, 0, 1),
                    the_reg,
                    c"res_of_cmpeq".as_ptr(),
                );
                let dont_jump_bb = LLVMAppendBasicBlock(cur_func.func, c"no_jump_br".as_ptr());
                LLVMBuildCondBr(builder, cmp, get_label_bb!(label), dont_jump_bb);
                LLVMPositionBuilderAtEnd(builder, dont_jump_bb);
            }
            Instr::Label(label) => {
                /*|| {
                    let a = LLVMGetInstructionOpcode(some_instr);
                    println!("not null: {:?}", a);
                    match a {
                        LLVMOpcode::LLVMRet | LLVMOpcode::LLVMBr => false,
                        _ => true,
                        }
                }*/
                if legal_to_gen {
                    LLVMBuildBr(builder, get_label_bb!(label));
                }
                /*if ret_hack {
                // because ret_hack was true, the previous block never returned, so
                // we will have to branch for them to this block
                // so if we have LABEL1: LABEL2: we will insert a branch after LABEL1: to LABEL2
                }*/
                // builder shall build here now
                LLVMPositionBuilderAtEnd(builder, get_label_bb!(label));
                legal_to_gen = true;
            }
            Instr::FuncDef(fnidx) => {
                legal_to_gen = true;
                cur_regs.clear();
                let func = &my_funcs[fnidx.0 as usize];
                cur_func = func;
                let ir_func = &ir_functions[fnidx.0 as usize];
                // add entry bb to function
                let entry_bb = LLVMAppendBasicBlockInContext(ctx, func.func, c"bb_entry".as_ptr());
                // position builder at end of first bb of function
                LLVMPositionBuilderAtEnd(builder, entry_bb);
                for i in 0..ir_func.regs_used {
                    let alloc = LLVMBuildAlloca(builder, int, c"l_alloca".as_ptr());
                    cur_regs.push(alloc);
                    // load args into the first regs we have on stack
                }
                for i in 0..ir_func.params {
                        // set to arg
                        let param = LLVMGetParam(func.func, i as u32);
                        LLVMBuildStore(builder, param, get_reg![i]);
                }
            }
            Instr::Return(reg) => {
                let res = LLVMBuildLoad2(builder, int, get_reg!(reg), c"to_retun_this".as_ptr());
                LLVMBuildRet(builder, res);
                legal_to_gen = false;
            }
            // what to do here?
            Instr::EndFunc => {
                // we reached the end without branching or returning ...
                // insert return that will never happen, because we are in the situation:
                // LABEL1: [end_of_func]
                if legal_to_gen {
                    LLVMBuildRet(builder, LLVMConstInt(int, 5454, 1));
                }
                println!("\n\nDONE WITH THIS FUNC: \n \n");
                LLVMDumpValue(cur_func.func);
                LLVMVerifyFunction(
                    cur_func.func,
                    LLVMVerifierFailureAction::LLVMAbortProcessAction,
                );
            }
        }
    }

    let mut msg: *mut i8 = std::ptr::null_mut();
    println!("\n\nLLVM IR DUMP:\n\n ");
    LLVMDumpModule(md);
    // will look like IOT instruction core dumped for some reason, if we fail
    LLVMVerifyModule(
        md,
        LLVMVerifierFailureAction::LLVMAbortProcessAction,
        (&mut msg) as *mut *mut i8,
    );
    LLVMDisposeMessage(msg);

    LLVMSetTarget(md, c"x86_64-unknown-linux-gnu".as_ptr());

    LLVMWriteBitcodeToFile(md, c"./test.bc".as_ptr());
    LLVMDisposeBuilder(builder);
    LLVMDisposeModule(md);
    LLVMContextDispose(ctx);
}
