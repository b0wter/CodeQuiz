
var express = require('express');
var app = express();

const filename = "data/State_of_Iowa_-_Monthly_Voter_Registration_Totals_by_County.csv";

// Read data
var parse_data = require('./database');
var db = parse_data(filename);

// Routing
app.get('/api', function (req, res) {
  var response = db.find(req.query);
  res.send(response);
});

// Launch app
app.listen(3000, function () {
  console.log('Example app listening on port 3000!');
});
