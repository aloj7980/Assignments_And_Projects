// Importing Modules
// const { 
//     random, 
//     problemReplicated, 
//     generateNumeratorCombinations,
//     problem
// } = require("./scripts");

/* Functions */
/** random()
 * random number generator that generates an integer between low and high parameters.
 * @param {number} low low parameter
 * @param {number} high high parameter
 */
 function random(low, high){
    return Math.floor(Math.random() * (high - low + 1) ) + low;
}

// Array to store all the problems in 
var problems = new Array();

// this is a count of the problems and an index for problems array
var problemNumber = 0;  

// number of problems correct, to be incremented when user submits correct answer
var correct = 0;

// number of problems attempted, to be incremented when user submits any answer
var attempted = 0;

// selects whether operator is addition, subtraction, multiplication, or division
var operator_selector = Math.floor(Math.random() * 4);
function readAnswer(){
    var answer_numerator = parseFloat(document.getElementById("inputAnswer1").value);
    if (isNaN(answer_numerator))
    {
        document.getElementById("answerMessage1").innerHTML = `Invalid Answer. Please try again!`;
        return;
    }
    var answer_denominator = parseFloat(document.getElementById("inputAnswer2").value);
    if (isNaN(answer_denominator))
    {
        document.getElementById("answerMessage1").innerHTML = `Invalid Answer. Please try again!`;
        return;
    }
    var num1_numerator = parseFloat(document.getElementById("numeral_a_numerator").innerHTML);
    var num1_denominator = parseFloat(document.getElementById("numeral_a_denominator").innerHTML);
    var num2_numerator = parseFloat(document.getElementById("numeral_b_numerator").innerHTML);
    var num2_denominator = parseFloat(document.getElementById("numeral_b_denominator").innerHTML);
    let ans = 0;

    if(operator_selector === 0){
        ans = (num1_numerator / num1_denominator) + (num2_numerator / num2_denominator);
    }else if(operator_selector === 1){
        ans = (num1_numerator/num1_denominator) - (num2_numerator / num2_denominator);
    }else if(operator_selector === 2){
        ans = (num1_numerator / num1_denominator) * (num2_numerator / num2_denominator);
    }else{
        ans = (num1_numerator / num1_denominator) / (num2_numerator / num2_denominator);
    }

    let answer = answer_numerator / answer_denominator;
    let answerRounded = answer.toFixed(5);
    let ansRounded = ans.toFixed(5);

    problems.push({
        "numeral_a_numerator": num1_numerator,
        "numeral_a_denominator": num1_denominator,
        "numeral_b_numerator": num2_numerator,
        "numeral_b_denominator": num2_denominator,
        "user_answer": answerRounded,
        "answer": ansRounded,
        "correct": Boolean(answerRounded === ansRounded),
        "problem_number": problemNumber - 1,
        "operator": document.getElementById("operator").innerHTML
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
    operator_selector = Math.floor(Math.random() * 4);
    if(operator_selector === 0){
        document.getElementById("operator").innerHTML = "+";
    }else if(operator_selector === 1){
        document.getElementById("operator").innerHTML = "-";
    }else if(operator_selector === 2){
        document.getElementById("operator").innerHTML = "*";
    }else{
        document.getElementById("operator").innerHTML = "÷";
    }
    document.getElementById("fraction_operator_1").innerHTML = "/";
    document.getElementById("fraction_operator_2").innerHTML = "/";
    document.getElementById("fraction_operator_3").innerHTML = "/";
    document.getElementById("equality_sign").innerHTML = "=";


    let numeral_a_numerator = Math.floor(Math.random() * 10);
    let numeral_a_denominator = random(1, 10);  // so the denominator is not 0, can't divide by 0
    let numeral_b_numerator = Math.floor(Math.random() * 10);
    let numeral_b_denominator = random(1, 10);  // so the denominator is not 0, can't divide by 0

    problemNumber++;
    document.getElementById("numeral_a_numerator").innerHTML = numeral_a_numerator;
    document.getElementById("numeral_b_numerator").innerHTML = numeral_b_numerator;
    document.getElementById("numeral_a_denominator").innerHTML = numeral_a_denominator;
    document.getElementById("numeral_b_denominator").innerHTML = numeral_b_denominator;
    document.getElementById("answerMessage1").innerHTML = "";
    document.getElementById("submitbutton").disabled = false;
    document.getElementById("nextbutton").disabled = true;
    // const inputField = document.getElementById("inputAnswer");
    // inputField.value = "";
    document.getElementById("inputAnswer1").value = "";
    document.getElementById("inputAnswer2").value = "";
}

function score(){
    document.getElementById("problem_form").remove();
    var percent_correct = correct / attempted * 100;
    percent_correct = percent_correct.toFixed(2);
    document.getElementById("answerMessage1").innerHTML = `Your score is ${percent_correct}%`;
    document.getElementById("submitbutton").remove();
    document.getElementById("nextbutton").remove();
    document.getElementById("scorebutton").remove();
    document.getElementById("missed_questions_title").innerHTML = "Some Questions You Missed:";
    var countWrong = 0;
    var operator = '';
    for(let i = 0; i < problems.length && countWrong < 3; i++){
        if(!problems[i].correct){
            if(problems[i].operator === '+'){
                operator = '+';
            }else if(problems[i].operator === '-'){
                operator = '-'
            }else if(problems[i].operator === '*'){
                operator = '*';
            }else{
                operator = '÷';
            }
            if(countWrong === 0){
                document.getElementById("incorrect_problem_1").innerHTML = `The correct answer for ${problems[i].numeral_a_numerator} / ${problems[i].numeral_a_denominator} ${operator} ${problems[i].numeral_b_numerator} / ${problems[i].numeral_b_denominator} is ${problems[i].answer}`;
                document.getElementById("incorrect_answer_1").innerHTML = `You answered ${problems[i].user_answer}`;
            }else if(countWrong === 1){
                document.getElementById("incorrect_problem_2").innerHTML = `The correct answer for ${problems[i].numeral_a_numerator} / ${problems[i].numeral_a_denominator} ${operator} ${problems[i].numeral_b_numerator} / ${problems[i].numeral_b_denominator} is ${problems[i].answer}`;
                document.getElementById("incorrect_answer_2").innerHTML = `You answered ${problems[i].user_answer}`;
            }else{
                document.getElementById("incorrect_problem_3").innerHTML = `The correct answer for ${problems[i].numeral_a_numerator} / ${problems[i].numeral_a_denominator} ${operator} ${problems[i].numeral_b_numerator} / ${problems[i].numeral_b_denominator} is ${problems[i].answer}`;
                document.getElementById("incorrect_answer_3").innerHTML = `You answered ${problems[i].user_answer}`;
            }
            countWrong++;
        }
    }
}

