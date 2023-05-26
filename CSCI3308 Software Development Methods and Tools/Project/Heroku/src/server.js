/***********************
  Load Components!

  Express      - A Node.js Framework
  Body-Parser  - A tool to help use parse the data in a post request
  Pg-Promise   - A database tool to help use connect to our PostgreSQL database
***********************/
var express = require('express'); //Ensure our express framework has been added
var app = express();
var bodyParser = require('body-parser'); //Ensure our body-parser tool has been added
app.use(bodyParser.json());              // support json encoded bodies
app.use(bodyParser.urlencoded({ extended: true })); // support encoded bodies

//Create Database Connection
var pgp = require('pg-promise')();

/**********************
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
**********************/
const dev_dbConfig = {
	host: 'db',
	port: 5432,
	database: process.env.POSTGRES_DB,
	user:  process.env.POSTGRES_USER,
	password: process.env.POSTGRES_PASSWORD
};

/** If we're running in production mode (on heroku), the we use DATABASE_URL
 * to connect to Heroku Postgres.
 */
const isProduction = process.env.NODE_ENV === 'production';
const dbConfig = isProduction ? process.env.DATABASE_URL : dev_dbConfig;

// Heroku Postgres patch for v10
// fixes: https://github.com/vitaly-t/pg-promise/issues/711
if (isProduction) {
  pgp.pg.defaults.ssl = {rejectUnauthorized: false};
}

const db = pgp(dbConfig);

// set the view engine to ejs
app.set('view engine', 'ejs');
app.set('views', __dirname + '/views');
app.use(express.static(__dirname + '/'));//This line is necessary for us to use relative paths and access our resources directory


// home page
app.get('/', function(req, res) {
	res.render('pages/home',{
		local_css:"signin.css",
		my_title:"Home Page"
	});
});

// registration page
app.get('/register', function(req, res) {
	res.render('pages/register',{
		my_title:"Registration Page"
	});
});

// Login page
app.get('/login', function(req, res) {
	res.render('pages/login',{
		my_title:"Login Page"
	});
});

//Page to get users name
app.get('/name', function(req, res) {
	res.render('pages/name',{
		my_title:"Name Page"
	});
});
/*
app.post('/name/add_name',function(req,res){

	var name = req.body.name;

	var insert_statement = "INSERT INTO names(fname) VALUES('" + name + "') ON CONFLICT DO NOTHING;";

	var names = 'select * from names;';

	db.task('post-game-data', task => {
        return task.batch([
            task.any(insert_statement),
						task.any(names)
        ]);
    })
	.then(data => {
		res.render('pages/Main_Menu',{
			my_title:"Main Menu",
			names: data[1],
		})
	})
	.catch(err => {
		console.log('Uh Oh I made an oopsie');
		req.flash('error', err);
		res.render('pages/names',{
			my_title: "Names",
			players: '',
			playerinfo: '',
			games: ''
		})
	});

});
*/
// menu page
app.get('/Main_Menu', function(req, res) {
	res.render('pages/Main_Menu',{
		my_title:"Main Menu"
	});
});
/*
app.get('/Main_Menu', function(req,res){
    var names = 'select * from names;';
    db.task('get-everything', task => {
        return task.batch([
            task.any(names)
        ]);
    })
});
*/
app.post("/Main_Menu", function(req,res) {
	var name = String(req.body.name);
	console.log(name);
	res.send("Welcome " + name);
});

app.get('/About_Us', function(req, res) {
	res.render('pages/About_Us',{
		my_title:"About us Page"
	});
});
app.get('/addition', function(req, res) {
	res.render('pages/addition',{
		my_title:"addition Page"
	});
});
app.get('/subtraction', function(req, res) {
	res.render('pages/subtraction',{
		my_title:"subtraction Page"
	});
});
app.get('/multiplication', function(req, res) {
	res.render('pages/multiplication',{
		my_title:"multiplication Page"
	});
});
app.get('/division', function(req, res) {
	res.render('pages/division',{
		my_title:"division Page"
	});
});
app.get('/fractions', function(req, res) {
	res.render('pages/fractions',{
		my_title:"fractions Page"
	});
});
app.get('/decimals', function(req, res) {
	res.render('pages/decimals',{
		my_title:"decimals Page"
	});
});
app.get('/algebra', function(req, res) {
	res.render('pages/algebra',{
		my_title:"algebra Page"
	});
});
app.get('/report_card', function(req, res){
	res.render('pages/report_card', {
		my_title: "Report Card"
	});
});
//app.listen(3000);
const server = app.listen(process.env.PORT || 3000, () => {
  console.log(`Express running → PORT ${server.address().port}`);
});