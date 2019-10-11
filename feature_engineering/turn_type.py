####
# A function that calculates which direction a car is turning.
# Jimmy Hickey
# 2019-10-11
####


# FUTURE JIMMY
# Try relative angles


# turn type calculator
def turn_type(entry, exit):

	# Directions map (radial mapping)
	DIRECTIONS = {
		"N" : 0,
		"NE" : 45,
		"E" : 90,
		"SE" : 135,
		"S" : 180,
		"SW" : 225,
		"W" : 270,
		"NW" : 315
	}

	entry_deg = DIRECTIONS[entry]
	exit_deg = DIRECTIONS[exit]

	diff = (entry_deg - exit_deg) % 360

	turn_type = ''

	if diff == 0:
		turn_type = "Straight"
	elif diff == 180:
		turn_type = "U"
	elif diff > 0 and diff < 180:
		turn_type = "left"
	elif diff > 180:
		turn_type = "right"

	return(turn_type)

def main():
	print("~~~~~~~~~ CARDINAL RIGHT TURNS ~~~~~~~~~")
	print(turn_type("N", "E"))
	print(turn_type("E", "S"))
	print(turn_type("W", "N"))
	print(turn_type("S", "W"))




	print("~~~~~~~~~ CARDINAL LEFT TURNS ~~~~~~~~~")
	print(turn_type("W", "S"))
	print(turn_type("S", "E"))
	print(turn_type("E", "N"))
	print(turn_type("N", "W"))


	print("~~~~~~~~~ CARDINAL LEFT TURNS TO ANGLES ~~~~~~~~~")
	print(turn_type("W", "SE"))
	print(turn_type("W", "SW"))

	print(turn_type("S", "NE"))
	print(turn_type("S", "NW"))

	print(turn_type("N", "SW"))
	print(turn_type("N", "NW"))

	print(turn_type("E", "NW"))
	print(turn_type("E", "NE"))


	print("~~~~~~~~~~~~~~~~~~~ ANGLE LEFT TURNS TO CARDINAL ~~~~~~~~~~~~~~~~~")
	print(turn_type("NE","N"))
	print(turn_type("NE","W"))

	print(turn_type("NW","S"))
	print(turn_type("NW","W"))

	print(turn_type("SE","N"))
	print(turn_type("SE","E"))


	print("~~~~~~~~~~~~~~~~~~ uh oh~~~~~~~~~~~~~~~~~~")
	print(turn_type("N", "N"))
	print(turn_type("S", "N"))

if __name__ == "__main__":
	main()
