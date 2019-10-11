####
# A function that calculates which direction a car is turning.
# Jimmy Hickey
# 2019-10-11
####

import pandas as pd

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

	turn = ''

	if diff == 0:
		turn = "Straight"
	elif diff == 180:
		turn = "U"
	elif diff > 0 and diff < 180:
		turn = "Left"
	elif diff > 180:
		turn = "Right"

	return(turn)


def write_to_file(input_df, output_file_path):

	with open(output_file_path, 'w+') as out:
		for index, row in input_df.iterrows():
			out_string = str(row["RowId"]) + "," + turn_type(row["EntryHeading"], row["ExitHeading"]) + '\n'
			out.write(out_string)



def main():
	training_data = "/Users/jimmy/git/geotab-intersection-congestion/data/train.csv"
	testing_data = "/Users/jimmy/git/geotab-intersection-congestion/data/test.csv"

	training_output = "/Users/jimmy/git/geotab-intersection-congestion/data/train_turn-angle.csv"
	test_output = "/Users/jimmy/git/geotab-intersection-congestion/data/test_turn-angle.csv"

	training = pd.read_csv(training_data)
	test = pd.read_csv(testing_data)

	write_to_file(training, training_output)
	write_to_file(test, test_output)

if __name__ == "__main__":
	main()
