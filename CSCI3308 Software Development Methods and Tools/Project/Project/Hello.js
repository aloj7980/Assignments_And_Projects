//Code to try to connect to PSQL DB
const { Pool } = require('pg')
const pool = new Pool({
    user: 'postgres',
    host: 'localhost',
    database: 'postgres',
    password: 'password',
    port: 5432,
    max: 20,
})



//Error Handling
pool.on('error', (err, client) => {
    console.error('Problems', err);
    process.exit(-1);
})

//Create Table if not exists SQL Statement
var createTable= 'CREATE TABLE IF NOT EXISTS "USERNAME" ("USERNAME" VARCHAR(100) NOT NULL, PRIMARY KEY("USERNAME"));';

//Insert Statement SQL Statement
//var insertString='INSERT INTO USERNAME(USERNAME) VALUES("NEW_USER");';
var insertString = 'INSERT INTO TEST VALUES("NEW_USER");'

/*
pool.query(
"CREATE TABLE IF NOT EXISTS 'test' ('ID' VARCHAR(100) NOT NULL, PRIMARY KEY('ID'))",
	(err, res) => {
		console.log(err, res);
		pool.end();
	}
);

pool.query(insertString, (err,res) => {});
*/
/*
pool.connect((err,client,done) => {
        if (err) throw err
		client.query(createTable, (err, res) => {
            client.release();
            if (err) {
                console.log(err.stack)
            } else {
				console.log(res);
			}
		})
        client.query(insertString, (err, res) => {
            client.release();
            if (err) {
                console.log(err.stack)
            } else {
				console.log(res);
				/*
                for (var i = 0; i < res.rowCount; i++) {
                    console.log(res.rows[i].name, '\t\t', res.rows[i].major);
                }
				
            }
        })
    })
*/


/*
    .connect()
    .then(client => {
        return client
            .query(insertString)
            .then(res => {
                client.release();
                if (res.rowCount ==1){
                    console.log("Inserted 1 Row!!");
                }else{
                    console.log(res)
                }
            })
            .catch(err => {
                client.release()
                console.log(err.stack)
            })
    })
*/


//var finalSelect='SELECT * from football_players where id >6';

/*
pool
    .connect()
    .then(client => {
        return client
            .query(finalSelect)
            .then(res => {
                client.release()
                if (res.rowCount >0){
                    for (var i = 0; i <res.rowCount; i++){
                        console.log(res.rows[i].name,'\t\t',res.rows[i].major);
                    }
                }else{
                    console.log(res)
                }
            })
            .catch(err => {
                client.release()
                console.log(err.stack)
            })
    })
*/

//pool.end();



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
pool.query('SELECT * from USERNAME', (err, res) => {
    if(err){
        console.log(err);
    }else {
		console.log("Success");
		
        for (var i = 0; i <res.rowCount; i++){
            console.log(res.rows[i].name);
        }
		
    }
    pool.end();
});

const { Client } = require('pg');
const client = new Client({
	host: 'localhost',
  port: 5432,
  user: 'postgres',
  password: 'password',
})
client.connect(err => {
  if (err) {
    console.error('connection error', err.stack);
  } else {
    console.log('connected');
  }
})

var query = client.query("SELECT email from login where password = 'password'");
console.log(query);

client.end();
*/

/*
var pg = require('pg');
var connectionString = 'postgres://postgres:password@localhost:5432/postgres';
var pgClient = new pg.Client(connectionString);
pgClient.connect();

var query = pgClient.query("SELECT email from login where password = 'password'");

console.log(query);


pgClient.end();

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