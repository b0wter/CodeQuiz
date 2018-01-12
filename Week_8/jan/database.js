var fs = require('fs');
const loki = require("lokijs");
var parse = require('csv-parse');
var transform = require('stream-transform');

var db = new loki();
const names = ["Date", "FIPS", "County", "Democrat - Active", "Republican - Active", "Libertarian - Active", "No Party - Active", "Other - Active", "Total - Active", "Democrat - Inactive", "Republican - Inactive", "Libertarian - Inactive", "No Party - Inactive", "Other - Inactive", "Total - Inactive", "Grand Total", "Primary Lat Dec", "Primary Long Dec", "Primary County Coordinates"];

function toObject(data) {
    var rv = {};
    for (var i = 0; i < names.length; ++i)
        rv[names[i]] = data[i];
    return rv;
}

function parse_csv(filename) {
    var csv_collection = db.addCollection("csv");
    const parser = parse({delimiter: ','})
    const input = fs.createReadStream(filename);
    const transformer = transform(function(record, callback){
    setTimeout(function(){
        csv_collection.insert(toObject(record));
    }, 500);
    }, {parallel: 10});
    input.pipe(parser).pipe(transformer).pipe(process.stdout);

    return csv_collection;
}

module.exports = parse_csv;