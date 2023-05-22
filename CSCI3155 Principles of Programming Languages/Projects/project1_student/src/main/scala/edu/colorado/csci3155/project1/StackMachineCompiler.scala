package edu.colorado.csci3155.project1

object StackMachineCompiler {


    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        /* Begin Solution */
        e match {
            /* TODO: Your code here must handle the cases for Expr (see Expr.scala) */
            case Const(f: Double) => List(PushNumI(f))
            case BoolConst(b: Boolean) => List(PushBoolI(b))
            case Ident(id: String) => List(StoreEnv(id))
            case Plus(e1: Expr, e2: Expr) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(AddI)
            case Minus(e1: Expr, e2: Expr) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(SubI)
            case Mult(e1: Expr, e2: Expr) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(MultI)
            case Div(e1: Expr, e2: Expr) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(DivI)
            case Exp(e:Expr) => compileToStackMachineCode(e) ++ List(ExpI)
            case Log(e: Expr) => compileToStackMachineCode(e) ++ List(LogI)
            case Sine(e: Expr) => compileToStackMachineCode(e) ++ List(SinI)
            case Cosine(e:Expr) => compileToStackMachineCode(e) ++ List(CosI)
            case Geq(e1: Expr, e2: Expr) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(GeqI)
            case Eq(e1: Expr, e2: Expr) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(EqI)
            case And(e1:Expr, e2: Expr) => {
                val c = compileToStackMachineCode(e1)
                val d = compileToStackMachineCode(e2)
                c ++ List(CSkipI(d.length+1)) ++ d ++ List(SkipI(1)) ++ List(PushBoolI(false))
            }
            case Or (e1: Expr, e2: Expr) => {
                val c = compileToStackMachineCode(e1)
                val d = compileToStackMachineCode(e2)
                c ++ List(CSkipI(2)) ++ List(PushBoolI(true)) ++ List(SkipI(d.length)) ++ d
            }
            case Not(e: Expr) => compileToStackMachineCode(e) ++ List(NotI)
            case IfThenElse(cond: Expr, tExpr: Expr, elseExpr: Expr) => {
                val c = compileToStackMachineCode(cond)
                val d = compileToStackMachineCode(tExpr)
                val e = compileToStackMachineCode(elseExpr)
                c ++ List(CSkipI(d.length+1)) ++ d ++ List(SkipI(e.length)) ++ e
            }
            case Let(ident: String, e1: Expr, e2: Expr) => compileToStackMachineCode(e1) ++ List(LoadEnv(ident)) ++ compileToStackMachineCode(e2) ++ List(PopEnv)
            case _ => throw new IllegalArgumentException(s"I do not handle $e")
        }
        /* End Solution */
    }
}
