// Importing Modules
// const { 
//     random, 
//     problemReplicated, 
//     generateNumeratorCombinations,
//     problem
// } = require("./scripts");

/* Functions */
// Array to store all the problems in 
var problems = new Array();

// this is a count of the problems and an index for problems array
var problemNumber = 0;  

// number of problems correct, to be incremented when user submits correct answer
var correct = 0;

// number of problems attempted, to be incremented when user submits any answer
var attempted = 0;

// The readAnswer function currently doesn't have its intended function
// It should determine whether the user's answer was correct and u[date the display accordingly]
function readAnswer(){
    var answer = Math.round(parseFloat(document.getElementById("inputAnswer").value), 2);
    if (isNaN(answer))
    {
        document.getElementById("answerMessage1").innerHTML = `Invalid Answer. Please try again!`;
        return;
    }
    var num1 = parseInt(document.getElementById("numeral_a").innerHTML);
    var num2 = parseInt(document.getElementById("numeral_b").innerHTML);
    let ans = Math.round(num1 / num2, 2);

    problems.push({
        "numeral_a": num1,
        "numeral_b": num2,
        "user_answer": answer,
        "answer": ans,
        "operator": document.getElementById("operator").innerHTML,
        "correct": Boolean(ans === answer),
        "problem_number": problemNumber - 1
    });

    console.log(problems[problemNumber - 1]);

    if(answer === ans){
        correct++;
        attempted++;
        // document.getElementById("answerMessage1").innerHTML = `Correct Answer! Your answer: ${ans}`;
        document.getElementById("answerMessage1").innerHTML = `Correct Answer! Your answer: ${document.getElementById("inputAnswer").value}`;
    }else{
        attempted++;
        document.getElementById("answerMessage1").innerHTML = `Incorrect Answer. Your answer: ${answer} | Correct answer: ${ans}`;
    }
    document.getElementById("answerMessage2").innerHTML = `You've answered ${correct} problems correctly in ${attempted} attempts`;
    document.getElementById("submitbutton").disabled = true;
    document.getElementById("nextbutton").disabled = false;
}
function changeProblem(){
    document.getElementById("operator").innerHTML = "÷";
    document.getElementById("equality_sign").innerHTML = "=";

    let numeral_b = Math.floor(Math.random() * 12) + 1;
    let numeral_a = numeral_b * Math.floor(Math.random() * 12);

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
            if(countWrong===0){
                document.getElementById("incorrect_problem_1").innerHTML = `The correct answer for ${problems[i].numeral_a} ÷ ${problems[i].numeral_b} is ${problems[i].answer}`;
                document.getElementById("incorrect_answer_1").innerHTML = `You answered ${problems[i].user_answer}`;
            }else if(countWrong===1){
                document.getElementById("incorrect_problem_2").innerHTML = `The correct answer for ${problems[i].numeral_a} ÷ ${problems[i].numeral_b} is ${problems[i].answer}`;
                document.getElementById("incorrect_answer_2").innerHTML = `You answered ${problems[i].user_answer}`;
            }else{
                document.getElementById("incorrect_problem_3").innerHTML = `The correct answer for ${problems[i].numeral_a} ÷ ${problems[i].numeral_b} is ${problems[i].answer}`;
                document.getElementById("incorrect_answer_3").innerHTML = `You answered ${problems[i].user_answer}`;
            }
            countWrong++;
        }
    }
}
