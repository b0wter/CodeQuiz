# skip cell 0
>

# read n = one byte to cell 1
++++>,<[->----- ----- --<]>>
++++>,<[->----- ----- --<]>>
++++>,<[->----- ----- --<]

<[->>+++++ +++++<<]
<<[->>>+++++ +++++[->+++++ +++++<]<<<]
>>>>[-<<<<<+>>>>>]<<<<

# read k = one byte to cell 2
++++>,<[->----- ----- --<]>>
++++>,<[->----- ----- --<]>>
++++>,<[->----- ----- --<]

<[->>+++++ +++++<<]
<<[->>>+++++ +++++[->+++++ +++++<]<<<]
>>>>[-<<<<<+>>>>>]<<<<
<

# copy cell 1 to cell 3 and 4
<[->>+>+<<<]>>>>

# read (cell 3) bytes
<[
	>,
	<<[->>>+>+<<<<]
	>>>>[-<<<<+>>>>]<<<
	
	[->>>+>+<<<<]>>>>
	[-<<<<+>>>>]<-
]

# move to cell 0
<[<<<]

# copy cell 2 to cell 1 and increment both of them
>>+[-<+<+>>]
<<[->>+<<]>

# copy current value to cell (i plus 3) and decrement till 0
[
	>>>[-]<<<
	[->>>+<<<]>>>-
]

# read all entered numbers to the right including current number
+[>.>>]

# move to cell 0
<[<<<]

# copy cell 2 to cell 1 and decrement both of them
>>-[-<+<+>>]
<<[->>+<<]>

# read all entered numbers to the right where 0 (using flags)
>>[
	[-]+>
	[<->[-]]
	<[>>.<<-]>
>>]
