// Importing Modules
// const { 
//     random, 
//     problemReplicated, 
//     generateNumeratorCombinations,
//     problem
// } = require("./scripts");

/* Functions */

// selects whether operator is addition, subtraction, multiplication, or division
// Not in use yet, the program currently only runs decimal addition
var operator_selector = Math.floor(Math.random() * 4);

// Array to store all the problems in 
var problems = new Array();

// this is a count of the problems and an index for problems array
var problemNumber = 0;  

// number of problems correct, to be incremented when user submits correct answer
var correct = 0;

// number of problems attempted, to be incremented when user submits any answer
var attempted = 0;

// selects whether operator is addition or subtraction
var operator_selector = Math.floor(Math.random() * 2);

function readAnswer(){
    var answer = parseFloat(document.getElementById("inputAnswer").value);  // the user's input
    if (isNaN(answer))
    {
        document.getElementById("answerMessage1").innerHTML = `Invalid Answer. Please try again!`;
        return;
    }
    var num1 = parseFloat(document.getElementById("numeral_a").innerHTML);
    var num2 = parseFloat(document.getElementById("numeral_b").innerHTML);
    let ans = 0;
    if(operator_selector === 0){
        ans = num1 + num2;
    }else{
        ans = num1 - num2;
    }
    var answerRounded = answer.toFixed(2);
    let ansRounded = ans.toFixed(2);

    problems.push({
        "numeral_a": num1,
        "numeral_b": num2,
        "user_answer": parseFloat(answerRounded),  // the user's rounded input answer 
        "answer": parseFloat(ansRounded),  // the correct rounded answer
        "operator": document.getElementById("operator").innerHTML,
        "correct": Boolean(answerRounded === ansRounded),
        "problem_number": problemNumber - 1
    });

    console.log(problems[problemNumber - 1]);

    if(answerRounded === ansRounded){
        correct++;
        attempted++;	
        document.getElementById("answerMessage1").innerHTML = `Correct Answer! Your answer: ${answerRounded}`;
    }else{
        attempted++;
        document.getElementById("answerMessage1").innerHTML = `Incorrect Answer. Your answer: ${answerRounded} | Correct answer: ${ansRounded}`;
    }
    document.getElementById("answerMessage2").innerHTML = `You've answered ${correct} problems correctly in ${attempted} attempts`;
    document.getElementById("submitbutton").disabled = true;
    document.getElementById("nextbutton").disabled = false;
}

function changeProblem(){
    operator_selector = Math.floor(Math.random() * 2);
    if(operator_selector === 0){
        document.getElementById("operator").innerHTML = "+";
    }else{
        document.getElementById("operator").innerHTML = "-";
    }
    document.getElementById("equality_sign").innerHTML = "=";

    
    let numOneWhole = Math.floor(Math.random() * 100);
    let numTwoWhole = Math.floor(Math.random() * 100);
    let numOneDecimal = Math.floor(Math.random() * 100) / 100;
    let numTwoDecimal = Math.floor(Math.random() * 100) / 100;
    let numeral_a = numOneWhole + numOneDecimal;
    let numeral_b = numTwoWhole + numTwoDecimal;

    problemNumber++;
    document.getElementById("numeral_a").innerHTML = numeral_a;
    document.getElementById("numeral_b").innerHTML = numeral_b;
    document.getElementById("answerMessage1").innerHTML = "";
    const inputField = document.getElementById("inputAnswer");
    inputField.value = "";
    document.getElementById("submitbutton").disabled = false;
    document.getElementById("nextbutton").disabled = true;
}
function score(){
    document.getElementById("problem_form").remove();
    var percent_correct = correct/attempted*100;
    percent_correct = percent_correct.toFixed(2);
    document.getElementById("answerMessage1").innerHTML = `Your score is ${percent_correct}%`;
    document.getElementById("submitbutton").remove();
    document.getElementById("nextbutton").remove();
    document.getElementById("scorebutton").remove();
    document.getElementById("missed_questions_title").innerHTML="Some Questions You Missed:";
    var countWrong=0;
    for(let i=0;i<problems.length && countWrong<3;i++){
        if(!problems[i].correct){
            if(problems[i].operator==='+'){
                if(countWrong===0){
                    document.getElementById("incorrect_problem_1").innerHTML = `The correct answer for ${problems[i].numeral_a} + ${problems[i].numeral_b} is ${problems[i].answer}`;
                    document.getElementById("incorrect_answer_1").innerHTML = `You answered ${problems[i].user_answer}`;
                }else if(countWrong===1){
                    document.getElementById("incorrect_problem_2").innerHTML = `The correct answer for ${problems[i].numeral_a} + ${problems[i].numeral_b} is ${problems[i].answer}`;
                    document.getElementById("incorrect_answer_2").innerHTML = `You answered ${problems[i].user_answer}`;
                }else{
                    document.getElementById("incorrect_problem_3").innerHTML = `The correct answer for ${problems[i].numeral_a} + ${problems[i].numeral_b} is ${problems[i].answer}`;
                    document.getElementById("incorrect_answer_3").innerHTML = `You answered ${problems[i].user_answer}`;
                }
            }else{
                if(countWrong===0){
                    document.getElementById("incorrect_problem_1").innerHTML = `The correct answer for ${problems[i].numeral_a} - ${problems[i].numeral_b} is ${problems[i].answer}`;
                    document.getElementById("incorrect_answer_1").innerHTML = `You answered ${problems[i].user_answer}`;
                }else if(countWrong===1){
                    document.getElementById("incorrect_problem_2").innerHTML = `The correct answer for ${problems[i].numeral_a} - ${problems[i].numeral_b} is ${problems[i].answer}`;
                    document.getElementById("incorrect_answer_2").innerHTML = `You answered ${problems[i].user_answer}`;
                }else{
                    document.getElementById("incorrect_problem_3").innerHTML = `The correct answer for ${problems[i].numeral_a} - ${problems[i].numeral_b} is ${problems[i].answer}`;
                    document.getElementById("incorrect_answer_3").innerHTML = `You answered ${problems[i].user_answer}`;
                }
            }
            countWrong++;
        }
    }
}
