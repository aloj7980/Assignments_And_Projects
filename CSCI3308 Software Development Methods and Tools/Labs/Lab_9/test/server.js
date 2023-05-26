// Imports the server.js file to be tested.
let server = require("../server");
//Assertion (Test Driven Development) and Should, Expect(Behaviour driven development) library
let chai = require("chai");
// Chai HTTP provides an interface for live integration testing of the API's.
let chaiHttp = require("chai-http");
chai.should();
chai.use(chaiHttp); 
const { expect } = chai;
var assert = chai.assert;




describe("Server!", () => {

    // Sample test case given to test / endpoint. 
    it("Returns the default welcome message", done => {
      chai
        .request(server)
        .get("/")
        .end((err, res) => {
          expect(res).to.have.status(200);
          expect(res.body.status).to.equals("success");
          assert.strictEqual(res.body.message, "Welcome!");
          done();
        });
    });

    // Please add your test cases here.
    
    it("should return an Array with length>0", done => {
      chai
        .request(server)
        .get("/ops")
        .end((err, res) => {
          //test whether ops is an array
          expect(ops).to.be.an('array');
          //test whether ops isn't empty
          expect(ops).to.have.lengthOf.above(0);          
          done();
        });
      });

      it("should return 1 when the id equals 1", done => {
        chai
          .request(server)
          .get("/operations/:id")
          .end((err, res) => {
            ops.should.have.property(id).eq(1);
            ops.should.have.property(name);
            ops.should.have.property(sign);
            done();
          });
        });
        it("should return 4 when the id equals 4", done => {
          chai
            .request(server)
            .post("/operations")
            .end((err, res) => {
              ops.should.have.property(id).eq(4);
              ops.should.have.property(name);
              ops.should.have.property(sign);
              done();
            });
          });
    
  });