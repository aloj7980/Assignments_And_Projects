const express = require("express");
const bodyParser = require('body-parser');
const app = express();
const path = require('path');
app.use(bodyParser.urlencoded({ extended: false }));
app.use(express.json());

const port = 8080;

app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'ejs');

//So we're letting ExpressJS handle the images/styles/etc so we need to do this:
// Here's the doc (https://expressjs.com/en/starter/static-files.html)
app.use(express.static('resources'));

// COMMENT: This uses the pg-promise dependency and creates a connection to Postgres db. (http://vitaly-t.github.io/pg-promise/index.html)
var pgp = require('pg-promise')();
const connection = {
    user: 'postgres',
    host: 'localhost',
    database: 'postgres',
    password: 'password',
    port: 5432,
    max: 20,
};
const db = pgp(connection);

// COMMENT: Changed the route to /main_menu/:id so if you pass /main_menu/3 then it'll get the person with the ID 3 and render their name.
// NOTE: As for routing, referred to this: (https://expressjs.com/en/guide/routing.html#route-parameters)
app.get("/main_menu/:id", function(req, res) {
	var params = req.params;
	var id = params.id;

	// COMMENT: referred to this: (https://github.com/vitaly-t/pg-promise/wiki/Learn-by-Example#simple-select)
	db.any('SELECT * FROM test WHERE id = $1', [id])
		.then(function(data) {
			var name = data[0].name;
			res.render('main_menu', {
				name: name
			});
		})
		.catch(function(error) {
			
			res.render('main_menu', {
				name: "No one"
			});
		});
});

// COMMENT: Created this method to handle the POST form action from views/name.html
// NOTE: Edited the form to change ID from ID to Fname
app.post('/submit', function(req,res) {
	var formData = req.body;
	
	var name = formData.Fname;
	if (name) {
		db.one('INSERT INTO test(name) VALUES($1) RETURNING id', [name])
			.then(function(data) {
				var newId = data.id;
				res.status(200).send({ id: newId});
			})
			.catch(function(error){
				console.error(error);
				res.status(500).send(error);
			});
	} else {
		res.status(500).send("No name");
	}
});

app.get('/', function(req, res){
	res.render('home');
});

app.get('/main_menu', function(req, res){
	res.render('main_menu', {
		name: "Logan"
	});
});

app.get('/name', function(req, res){
	res.render('name');
});

app.get('/:dynamicRoute', function (req,res) {
	res.render(req.params.dynamicRoute);
});

app.listen(port, function(){
	console.log("Running on port 8080");	
})

/***********************
  Load Components!

  Express      - A Node.js Framework
  Body-Parser  - A tool to help use parse the data in a post request
  Pg-Promise   - A database tool to help use connect to our PostgreSQL database

var express = require('express'); //Ensure our express framework has been added
var app = express();
var bodyParser = require('body-parser'); //Ensure our body-parser tool has been added
const { json } = require('body-parser');
app.use(bodyParser.json());              // support json encoded bodies
app.use(bodyParser.urlencoded({ extended: true })); // support encoded bodies

// Create Database Connection
var pgp = require('pg-promise')();

  Database Connection information
  host: This defines the ip address of the server hosting our database.
		We'll be using `db` as this is the name of the postgres container in our
		docker-compose.yml file. Docker will translate this into the actual ip of the
		container for us (i.e. can't be access via the Internet).
  port: This defines what port we can expect to communicate to our database.  We'll use 5432 to talk with PostgreSQL
  database: This is the name of our specific database.  From our previous lab,
		we created the football_db database, which holds our football data tables
  user: This should be left as postgres, the default user account created when PostgreSQL was installed
  password: This the password for accessing the database. We set this in the
		docker-compose.yml for now, usually that'd be in a seperate file so you're not pushing your credentials to GitHub :).

const dbConfig = {
	host: 'localhost',
	port: 3333,
	database: 'user_info_db',
	user: 'postgres',
	password: 'pwd'
};

var db = pgp(dbConfig);

app.use(express.static(__dirname + '/'));  // This line is necessary for us to use relative paths and access our resources directory


// login page
app.get('/login', function(req, res) {
	var query = ``;
	var data = {};
	data.email = document.getElementById("inputEmail");  // getting the email from the form
	data.password = document.getElementById("Password");

	console.log('params: ' + JSON.stringify(req.params));
	console.log('body: ' + JSON.stringify(req.body));
	console.log('query: ' + JSON.stringify(req.query));

	email = req.query.emailName;
	password = req.query.passowrd;
	var query = `SELECT * FROM user_info_db WHERE email_adderss='${email} AND password='${passowrd}';`;

	db.any(query).then(function(rows){
		console.log('result = ', rows[0]);
		user_email = rows[0].email_adderss;
		user_password = rows[0].password;
	})

	res.render('views/login',{
		local_css: "signin.css",
		my_title: "Login Page"
	});
});
***********************/
