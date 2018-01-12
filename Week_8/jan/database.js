var fs = require('fs');
const loki = require("lokijs");
var parse = require('csv-parse');
var transform = require('stream-transform');

var db = new loki();
const names = ["Date", "FIPS", "County", "Democrat - Active", "Republican - Active", "Libertarian - Active", "No Party - Active", "Other - Active", "Total - Active", "Democrat - Inactive", "Republican - Inactive", "Libertarian - Inactive", "No Party - Inactive", "Other - Inactive", "Total - Inactive", "Grand Total", "Primary Lat Dec", "Primary Long Dec", "Primary County Coordinates"];
const template = {"Date": "", "Month": "", "Year": "", "FIPS": "",  "County": "",  "Democrat - Active": "",  "Republican - Active": "",  "Libertarian - Active": "",  "No Party - Active": "",  "Other - Active": "",  "Total - Active": "",  "Democrat - Inactive": "",  "Republican - Inactive": "",  "Libertarian - Inactive": "",  "No Party - Inactive": "",  "Other - Inactive": "",  "Total - Inactive": "",  "Grand Total": "",  "Primary Lat Dec": "",  "Primary Long Dec": "",  "Primary County Coordinates": ""};

// Convert CSV line to object
function toObject(data) {
    var rv = Object.assign({}, template);
    for (var i = 0; i < names.length; ++i)
        rv[names[i]] = data[i];
    const date = new Date(data[0]);
    if (date) {
        rv.Month = date.getMonth().toString();
        rv.Year = date.getFullYear().toString();
    }
    return rv;
}

// Pipe file throught parser
function parse_csv(filename) {
    var csv_collection = db.addCollection("csv");
    const parser = parse({delimiter: ','})
    const input = fs.createReadStream(filename);
    const transformer = transform(function(record, callback){
        csv_collection.insert(toObject(record));
    });
    input.pipe(parser).pipe(transformer).pipe(process.stdout);

    return csv_collection;
}

// Make it public
module.exports = parse_csv;