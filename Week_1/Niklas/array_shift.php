<?php
	if (isset($_GET["arg"])) {
		$numbers = explode(" ", $_GET["arg"]);
	} else {
		$numbers = $argv;
		array_shift($numbers); // script name
	}


	$len = array_shift($numbers);
	$shift = array_shift($numbers);

	for ($i=0; $i<$shift; $i++) {
		array_push($numbers, array_shift($numbers));
	}

	echo implode(" ", $numbers);
?>
