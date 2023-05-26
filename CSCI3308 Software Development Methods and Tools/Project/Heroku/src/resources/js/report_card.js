
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

