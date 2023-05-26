
function insertName(){
	// const queryString = window.location.search;
	// console.log(`queryString: ${queryString}`);
	// const urlParams = new URLSearchParams(queryString);
	// const Fname = urlParams.get('Fname');
	// console.log(`Fname: ${Fname}`);

	// const
	// 	keys = urlParams.keys(),
	// 	values = urlParams.values(),
	// 	entries = urlParams.entries();

	// for (const key of keys) console.log(key);

	// for (const value of values) console.log(value);

	var name = window.location.search.split("=")[1];
	// for(const entry of entries) {
		// if(entry[0] == 'Fname'){
			// name = entry[1];
		// }
		// console.log(`${entry[0]}: ${entry[1]}`);
	// }
	if(name == undefined){
		document.getElementById("welcome-msg").style.visibility = "hidden";
		document.getElementById("welcome-msg").style.height = "0px";
	}
	else{
		document.getElementById("welcome-msg").style.visibility = "visible";
		document.getElementById("welcome-msg").style.height = "auto";
		document.getElementById("Fname-welcome").innerHTML = name;
	}
}

function checkSignInStatus(){
	var signin_btn = document.getElementById("sign-in-btn");
	var reg_btn = document.getElementById("register-btn");
	console.log(document.getElementById("inputPassword").value);
}

function uploadDataToDB(){
	var full_name = document.getElementById("input_full_name").value;
	var user_email = document.getElementById("inputEmail").value;
	var user_pass = document.getElementById("Password").value;
	var confirm_pass = document.getElementById("ConfirmPassword").value;
	if(user_pass != confirm_pass){
		alert("Passwords do not match.");
		document.getElementById("passNoMatch").innerHTML = "Passwords do not match.";
		// break;
	}
	var query = `INSERT INTO user_problems(problem_type, numeral_a, operator, numeral_b, answer, correct, attempted)`;
    query += `VALUES('Addition', ${num1}, '+', ${num2}, ${ans}, ${correct}, ${attempted});`;
	var query = `INSERT INTO user_info_db(full_name, email_address, password)`;
	query += `VALUES(${full_name}, ${user_email}, ${user_pass}) ON CONFLICT DO NOTHING`;  // add the user's info to the database.
	window.location.href = "../../views/login.html"; // This supposedly makes page change but can't get to work
}

function validateSignIn(){
	var valid_email = false;
	var valid_password = false;
	var user_email = document.getElementById("inputEmail").value;
	var user_pass = document.getElementById("inputPassword").value;
	
	var all_emails = new Array();  // make an array of all the emails in the database
	var all_passwords = new Array();  // make an array of all the passwords in the database
	var num_users = all_emails.length;

	var query = `SELECT email_address, password FROM user_info_db WHERE email_address=${user_email} AND password=${user_pass};`;  // query to get the row where the user's email and password match.
	db.any(query).then(info => {
		all_emails = info[0],
		all_passwords = info[1]
	}).catch(err => {
		console.log(`Error: ${err}`);
	});

	for(let i = 0; i < num_users; i++){  
		if(all_emails[i] === user_email){  // match the emails
			valid_email = true;
		}
		if(all_passwords[i] === user_pass){  // match the passwords
			valid_password = true;
		}
		if(valid_email && valid_password){
			break;  // User is authenticated.
		}
	}

	if(valid_email && valid_password){
		console.log("User is authenticated");
	}
	else if(valid_email && !valid_password){
		console.log("Incorrect password");
		document.getElementById("invalid_message").innerHTML = "Incorrect Password";
	}
	else if(!valid_email && valid_password){
		console.log("Incorrect email");
		document.getElementById("invalid_message").innerHTML = "Incorrect Email";
	}
	else{
		document.getElementById("invalid_message").innerHTML = "Incorrect Email and Password";
	}
} 


function showPassword(id) {
	var pwd = document.getElementById(id);
	if(pwd.type === "password"){
		pwd.type = "text";
	}
	else{
		pwd.type = "password";
	}
}

function validateEmail(email){
	if(!email){
		return false;
	}
	if(email.length > 254){
		return false;
	}
	var valid = tester.test(email);
	// if(!valid)
}


function proceedToProblem(id, toggle){
	var x = document.getElementById(id);
	if(toggle == 0){
		x.style.visibility = "hidden";
		x.style.height = "0px";
	}
	if(toggle == 1){
		x.style.visibility = "visible";
		x.style.height = "auto";
	}
}

/** random()
 * random number generator that generates an integer between low and high parameters.
 */
function random(low, high){
    return Math.floor(Math.random() * (high - low + 1) ) + low;
}

/** Check if the *numeralCombinationsUsed* array has the combination of the given parameters.
 * We don't want the same problem being generated multiple times so make function to check if problem has been replicated.
 * @param numeral_a (Number) First numeral in the equation.
 * @param numeral_b (Number) Second numeral in the equation.
 * @param numeralCombinationsUsed (Array) 2d Array of numeral combinations used for that specific file. 
 * @returns **True** if the combination is in the numeralCombinationsUsed array.  **False** if the combination isn't in the numeralCombinationsUsed array.
 */
function problemReplicated(numeral_a, numberal_b, numeralCombinationsUsed){
	for(let i = 0; i < numeralCombinationsUsed.length; i++){
		let arr1 = [numeral_a, numberal_b];
		let arr2 = numeralCombinationsUsed[i];
		let n = arr1.length;
		let m = arr2.length;
		if(n != m){
			return false;
		}
		arr1.sort();
		arr2.sort();
		for(let j = 0; j < n; j++){
			if(arr1[j] != arr2[j]){
				return false;
			}
		}
		return true;
	}
	return false;
}

/** Generate a given number of unique numerator combinations.
 * @param numComb (Number) The number of unique numerator combinations.
 * @returns A 2d array of the numerator combinations.
 */
function generateNumeratorCombinations(numComb){
	var numeralCombinationsUsed = new Array();

	var a = random(1, 100);
	var b = random(1, 100);
	a = 2;
	b = 1;
	numeralCombinationsUsed.push([a, b]);

	while(numeralCombinationsUsed.length != numComb){
		a = random(1, 100);
		b = random(1, 100);
		if(!problemReplicated(a, b, numeralCombinationsUsed)){
			var numerals = [a, b];
			numeralCombinationsUsed.push(numerals);
		}
	}

	return numeralCombinationsUsed;
}

/** Generate a new problem.
 * @param number_ (Number) The problem number
 * @param type_ (String) The problem type ('addition', 'subtraction', etc....)
 * @param difficulty_ (String or Number) difficulty level of the problem.
 * @param solution_ (Number) The problem answer.
 * @param incorrect_explanations_ (Object) Object for different explanations for why the problem is wrong.
 * @returns A new problem object.
 */
function generateProblem(number_, type_, difficulty_, solution_, incorrect_explanations_){
	if(!(typeof type_ === 'string')){
		console.log(`Error: parameter type_ must be a string but is instead of type: (${typeof type_})`);
	}
	/* Base Object for each problem */
	const problem = {
		number: number_,  // problem number
		type: type_,  // problem type.  Can be addition, subtraction, division, etc....
		difficulty: difficulty_,  // difficulty level of the problem
		solution: solution_,  // problem answer
		incorrect_explanations: incorrect_explanations_,  // object for different explanations for why the problem is wrong.
		num_attempts: 0,  // index to access user attempts for the problem.
		answers: new Array()  // array of attempted answers.  
	};
	return problem;
}

// Exporting
module.exports = { random, problemReplicated, generateNumeratorCombinations, problem };