const readline = require('readline');

function lshift(arr, k) {
	while (k--) {arr.push(arr.shift())};
	return arr;
}

(function main() {
	var lines = [];
	var rl = readline.createInterface({
		input: process.stdin,
		output: process.stdout
	});

	rl.on('line', function(line) {
		if (lines.push(line.trim().split(' ').map(x => {return parseInt(x)})) == 2) {
			console.log(lshift(lines[1], lines[0][1]).join(' '));
			rl.close();
		}
	});
})();
