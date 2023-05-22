package edu.colorado.csci3155.project1

import scala.annotation.tailrec
import scala.math._


sealed trait StackMachineInstruction
/*-- Complete the byte code instructions as specified in the documentation --*/
case class LoadEnv(s: String) extends StackMachineInstruction
case class  StoreEnv(s: String) extends StackMachineInstruction
case object PopEnv extends StackMachineInstruction

case class PushNumI(f: Double) extends StackMachineInstruction
case class PushBoolI(b: Boolean) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object GeqI extends StackMachineInstruction
case object EqI extends StackMachineInstruction 
case object NotI extends StackMachineInstruction
case object PopI extends StackMachineInstruction

case class CSkipI(numToSkip: Int) extends StackMachineInstruction
case class SkipI(numToSkip: Int) extends StackMachineInstruction

object StackMachineEmulator {

    /*-- An environment stack is a list of tuples containing strings and values --*/
    type RuntimeStack = List[(String, Value)]
    /*-- An operand stack is a list of values --*/
    type OpStack = List[Value]

    /* Function emulateSingleInstruction
        Given a list of values to represent a operand stack
              a list of tuples (string, value) to represent runtime stack
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified runtime that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: OpStack,
                                 env: RuntimeStack,
                                 ins: StackMachineInstruction): (OpStack, RuntimeStack) = {
        ins match {
            /*TODO:  Your code here must handle each instruction type and 
                     execute the appropriate instructions to modify the 
                     runtime/operand stacks as specified */
            case LoadEnv(s) => {
                if(stack.length < 1){
                    throw new IllegalArgumentException("Not enough arguments for LoadEnv")
                }else{
                    stack.head match{
                        case Num(a) => {
                            (stack.tail,(s,Num(a)) :: env)
                        }
                        case Bool(a) => {
                            (stack.tail,(s,Bool(a)) :: env)
                        }
                        case _ => throw new IllegalArgumentException("Illegal arguments for LoadEnv")
                    }
                }
            }
            case StoreEnv(s) => {
                val s1 = env.find { case (str, _) => (str == s) }
                s1 match{
                    case None => throw new NoSuchElementException("Element $s not in environment")
                    case _ => (s1.get._2 :: stack,env)
                }
            }
            case PopEnv => (stack,env.tail)
            case PushNumI(f) => (Num(f) :: stack, env)
            case PushBoolI(b) => (Bool(b) :: stack, env)
            case AddI => {
                if(stack.length < 2){
                    throw new IllegalArgumentException("Not enough arguments for addition")
                }else{
                    val x = stack.tail
                    val y = x.tail
                    (stack.head,x.head) match{
                        case (Num(a),Num(b)) => {
                            val z = b + a
                            (Num(z) :: y,env)
                        }
                        case _ => throw new IllegalArgumentException("Illegal arguments for addition")
                    }
                }
            }
            case SubI =>{
                if(stack.length < 2){
                    throw new IllegalArgumentException("Not enough arguments for subtraction")
                }else{
                    val x = stack.tail
                    val y = x.tail
                    (stack.head,x.head) match{
                        case (Num(a),Num(b)) => {
                            val z = b - a
                            (Num(z) :: y,env)
                        }
                        case _ => throw new IllegalArgumentException("Illegal arguments for subtraction")
                    }
                }
            }
            case MultI =>{
                if(stack.length < 2){
                    throw new IllegalArgumentException("Not enough arguments for multiplication")
                }else{
                    val x = stack.tail
                    val y = x.tail
                    (stack.head,x.head) match{
                        case (Num(a),Num(b)) => {
                            val z = b * a
                            (Num(z) :: y,env)
                        }
                        case _ => throw new IllegalArgumentException("Illegal arguments for multiplication")
                    }
                }
            }
            case DivI =>{
                if(stack.length < 2){
                    throw new IllegalArgumentException("Not enough arguments for division")
                }else{
                    val x = stack.tail
                    val y = x.tail
                    (stack.head,x.head) match{
                        case (Num(a),Num(b)) if a != 0 => {
                            val z = b / a
                            (Num(z) :: y,env)
                        }
                        case _ => throw new IllegalArgumentException("Illegal arguments for division")
                    }
                }
            }
            case ExpI =>{
                if(stack.length < 1){
                    throw new IllegalArgumentException("Not enough arguments for exp")
                }else{
                    val x = stack.tail
                    stack.head match{
                        case Num(a) => {
                            val z = exp(a)
                            (Num(z) :: x,env)
                        }
                        case _ => throw new IllegalArgumentException("Illegal arguments for exp")
                    }
                }
            }
            case LogI =>{
                if(stack.length < 1){
                    throw new IllegalArgumentException("Not enough arguments for log")
                }else{
                    val x = stack.tail
                    stack.head match{
                        case Num(a) if a > 0 => {
                            val z = log(a)
                            (Num(z) :: x,env)
                        }
                        case _ => throw new IllegalArgumentException("Illegal arguments for log")
                    }
                }
            }
            case SinI =>{
                if(stack.length < 1){
                    throw new IllegalArgumentException("Not enough arguments for sine")
                }else{
                    val x = stack.tail
                    stack.head match{
                        case Num(a) => {
                            val z = sin(a)
                            (Num(z) :: x,env)
                        }
                        case _ => throw new IllegalArgumentException("Illegal arguments for sine")
                    }
                }
            }
            case CosI =>{
                if(stack.length < 1){
                    throw new IllegalArgumentException("Not enough arguments for cosine")
                }else{
                    val x = stack.tail
                    stack.head match{
                        case Num(a) => {
                            val z = cos(a)
                            (Num(z) :: x,env)
                        }
                        case _ => throw new IllegalArgumentException("Illegal arguments for cosine")
                    }
                }
            }
            case GeqI => {
                if(stack.length < 2){
                    throw new IllegalArgumentException("Not enough arguments for geq")
                }else{
                    val x = stack.tail
                    val y = x.tail
                    (stack.head,x.head) match{
                        case (Num(a),Num(b))=> {
                            val z = b >= a
                            (Bool(z) :: y,env)
                        }
                        case _ => throw new IllegalArgumentException("Illegal arguments for geq")
                    }
                }
            }
            case EqI => {
                if(stack.length < 2){
                    throw new IllegalArgumentException("Not enough arguments for eq")
                }else{
                    val x = stack.tail
                    val y = x.tail
                    (stack.head,x.head) match{
                        case (Num(a),Num(b)) => {
                            val z = b == a
                            (Bool(z) :: y,env)
                        }
                        case (Bool(a),Bool(b)) => {
                            val z = b == a
                            (Bool(z) :: y,env)
                        }
                        case _ => throw new IllegalArgumentException("Illegal arguments for eq")
                    }
                }
            }
            case NotI =>{
                if(stack.length < 1){
                    throw new IllegalArgumentException("Not enough arguments for not operator")
                }else{
                    val x = stack.tail
                    stack.head match{
                        case Bool(a) => {
                            val z = !a
                            (Bool(z) :: x,env)
                        }
                        case _ => throw new IllegalArgumentException("Illegal arguments for not operator")
                    }
                }
            }
            case PopI => (stack.tail,env)
            case _ => throw new RuntimeException(s"Unknown instruction type: $ins ")
        }
        
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Return the final runtimeStack and the top element of the opStack
     */
    @tailrec
    def emulateStackMachine(instructionList: List[StackMachineInstruction], 
                            opStack: OpStack=Nil, 
                            runtimeStack: RuntimeStack=Nil): (Value, RuntimeStack) =
        {
            /*-- Are we out of instructions to execute --*/
            if (instructionList.isEmpty){
                /*-- output top elt. of operand stack and the runtime stack --*/
                (opStack.head, runtimeStack)
            } else {
                /*- What is the instruction on top -*/
                val ins = instructionList.head
                ins match {
                    /*-- Conditional skip instruction --*/
                    case CSkipI(n) => {
                        /* get the top element in operand stack */
                        val topElt = opStack.head 
                        val restOpStack = opStack.tail 
                        val b = topElt.getBooleanValue /* the top element better be a boolean */
                        if (!b) {
                            /*-- drop the next n instructions --*/
                            val restOfInstructions = instructionList.drop(n+1)
                            emulateStackMachine(restOfInstructions, restOpStack, runtimeStack)
                        } else {
                            /*-- else just drop this instruction --*/
                            emulateStackMachine(instructionList.tail, restOpStack, runtimeStack)
                        }
                    }
                    case SkipI(n) => {
                        /* -- drop this instruction and next n -- continue --*/
                        emulateStackMachine(instructionList.drop(n+1), opStack, runtimeStack)
                    }

                    case _ => {
                        /*- Otherwise, just call emulateSingleInstruction -*/
                        val (newOpStack: OpStack, newRuntime:RuntimeStack) = emulateSingleInstruction(opStack, runtimeStack, ins)
                        emulateStackMachine(instructionList.tail, newOpStack, newRuntime)
                    }
                }
            }
        }
}