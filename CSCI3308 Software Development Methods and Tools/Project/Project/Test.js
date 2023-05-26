const express = require("express");
const app = express();

//Handling Database Request
app.get('/name', function(req,res){
	var query = 'SELECT * from test;';
	db.any(query)
		.then(function (rows){
			res.render('views/main_menu',{
				
			})
		})
		.catch(function (err){
			console.log('error', err);
			res.render('views/main_menu', {
				my_title: 'main_menu'
			})
		
		})
})


//Handling POST Request Parameters
app.post('/', function(req,res) {
	var getName = 'SELECT * from test';
	db.task('get-everything', task => {
		return task.batch([
			task.any(getName)
		]);
	})
	
	.then(info => {
		res.render('/views/main_menu', {
			my_title: "Main Menu"
		})
	})
	
	.catch(err => {
		console.log('error', err);
		res.render('views/main_menu', {
			my_title: "Main Menu"
		})
	});
});








/*const { Pool } = require('pg');
const pool = new Pool({
    user: 'postgres',
    host: 'localhost',
    database: 'postgres',
    password: 'password',
    port: 5432,
});
pool.connect(err => {
  if (err) {
    console.error('connection error', err.stack);
  } else {
    console.log('connected');
  }
})
pool.query('SELECT * from test', (err, res) => {
    if(err){
        console.log(err);
    }else {
		console.log("Success");
		
		console.log(res);
        //for (var i = 0; i <res.rowCount; i++){
        //    console.log(res.rows[i].name);
		//}
		
    }
    pool.end();
});
*/

/*
var pg = require(‘pg’);
var connectionString = "postgres://userName:password@serverName/ip:port/nameOfDatabase";
var pgClient = new pg.Client(connectionString);
pgClient.connect();
var query = pgClient.query("SELECT id from Customer where name = 'customername'");


query.on("row", function(row,result){

result.addRow(row);

});


query.on("end", function(result){

	if(result.rows[0] === undefined){

		return;

	}

	else{

		var id = result.rows[0].id;

		var query = "delete from CustomerAddress where customer_id = " + id ;
	
		pgClient.query(query);

	}

	pgClient.end();

});
*/