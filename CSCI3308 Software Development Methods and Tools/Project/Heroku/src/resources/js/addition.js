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

const operator = '+';

/**
 * The readAnswer function should determine whether the user's answer was 
 * correct and update the display accordingly 
*/
function readAnswer(){
    var answer = parseInt(document.getElementById("inputAnswer").value);  // this is the user's input
    if (isNaN(answer))
    {
        document.getElementById("answerMessage1").innerHTML = `Invalid Answer. Please try again!`;
        return;
    }
    var num1 = parseInt(document.getElementById("numeral_a").innerHTML);
    var num2 = parseInt(document.getElementById("numeral_b").innerHTML);
    let ans = num1 + num2;
    
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

    var query = `INSERT INTO user_problems(problem_type, numeral_a, operator, numeral_b, answer, correct, attempted)`;
    query += `VALUES('Addition', ${num1}, '+', ${num2}, ${ans}, ${correct}, ${attempted});`;
    if(answer === ans){
        correct++;
        attempted++;
        document.getElementById("answerMessage1").innerHTML = `Correct Answer! Your answer: ${answer}`;
    }else{
        attempted++;
        document.getElementById("answerMessage1").innerHTML = `Incorrect Answer. Your answer: ${answer} | Correct answer: ${ans}`;
    }
    document.getElementById("answerMessage2").innerHTML = `You've answered ${correct} problems correctly in ${attempted} attempts`;
    document.getElementById("submitbutton").disabled = true; 
    document.getElementById("nextbutton").disabled = false;
}

function changeProblem(){
    document.getElementById("operator").innerHTML = "+";
    document.getElementById("equality_sign").innerHTML = "=";

    let numeral_a = Math.floor(Math.random() * 100);
    let numeral_b = Math.floor(Math.random() * 100);
    let answer = numeral_a + numeral_b;
    
    document.getElementById("numeral_a").innerHTML = numeral_a;
    document.getElementById("numeral_b").innerHTML = numeral_b;
    document.getElementById("answerMessage1").innerHTML = "";
    const inputField = document.getElementById("inputAnswer");
    problemNumber++;
    inputField.value = "";
    document.getElementById("submitbutton").disabled = false;
    document.getElementById("nextbutton").disabled = true;
}

/**
 * call this when the user is done and wants to view the scores 
 */
function score(){
    document.getElementById("problem_form").remove();
    //It won't let me remove problem_form like it does in the other problems html pages
    //this is probably because we used a get request instead of regular html
    //someone should fix this
    var percent_correct = (correct / attempted) * 100;
    percent_correct = percent_correct.toFixed(2);
    document.getElementById("answerMessage1").innerHTML = `Your score is ${percent_correct}%`;
    document.getElementById("submitbutton").remove();
    document.getElementById("nextbutton").remove();
    document.getElementById("scorebutton").remove();
    document.getElementById("missed_questions_title").innerHTML="Some Questions You Missed:";
    var countWrong = 0;
    for(let i = 0; i < problems.length && countWrong < 3; i++){
        if(!problems[i].correct){
            if(countWrong === 0){
                document.getElementById("incorrect_problem_1").innerHTML = `The correct answer for ${problems[i].numeral_a} + ${problems[i].numeral_b} is ${problems[i].answer}`;
                document.getElementById("incorrect_answer_1").innerHTML = `You answered ${problems[i].user_answer}`;
            }else if(countWrong === 1){
                document.getElementById("incorrect_problem_2").innerHTML = `The correct answer for ${problems[i].numeral_a} + ${problems[i].numeral_b} is ${problems[i].answer}`;
                document.getElementById("incorrect_answer_2").innerHTML = `You answered ${problems[i].user_answer}`;
            }else{
                document.getElementById("incorrect_problem_3").innerHTML = `The correct answer for ${problems[i].numeral_a} + ${problems[i].numeral_b} is ${problems[i].answer}`;
                document.getElementById("incorrect_answer_3").innerHTML = `You answered ${problems[i].user_answer}`;
            }
            countWrong++;
        }
    }
    var json_problems = JSON.stringify(
        {
            "addition": [problems, correct, attempted - correct]
        });
    // console.log(problems);
    // console.log(json_problems);
    // writeProblemsToJSONFile();  
}

function writeProblemsToJSONFile(){
    var json_problems = JSON.stringify(
        {
            "addition": [problems, correct, attempted - correct]
        }
    );
    // parse json
    var jsonObj = JSON.parse(json_problems);
    // console.log(jsonObj);

    // // stringify json object
    // var jsonContent = JSON.stringify(jsonObj);
    // console.log(jsonContent);
    
    // fs.writeFile("output.json", jsonContent, 'utf8', function (err) {
    //     if(err){
    //         console.log("An error occured while writing JSON Object to File.");
    //         return console.log(err);
    //     }
    //     console.log("JSON file has been saved.");
    // });
}

function sendToReportCard(){
    console.log(problems);
    var body = document.getElementById("report-card-body");
    // var tally_table = document.getElementById("tally-table");
    document.getElementById("num_correct").innerHTML = correct;
    document.getElementById("num_incorrect").innerHTML = attempted - correct;
    for(let i = 0; i < problems.length; i++){
        var row = `<tr>`;
        row += `<td id="problem-number">${i + 1}</td>`;
        row += `<td id="operation">${operator}</td>`;
        row += `<td id="user_answer">${problems[i].user_answer}</td>`;
        row += `<td id="solution">${problems[i].answer}</td>`;
        row += `</tr>`;
        body += row;
    }
}
