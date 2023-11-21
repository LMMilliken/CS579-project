input_file_path = '/home/roberto/Scaricati/cmudict.0.6d'
output_file_path = './lexicon.pl'  # Specify the desired output file path

try:
    with open(input_file_path, 'r') as input_file, open(output_file_path, 'w') as output_file:
        for line in input_file:
            # Process each line as needed
            processed_line = line.strip()  # Example: Remove leading/trailing whitespaces
            parts = processed_line.split('  ')
            word = parts[0]
            if "'" in word or "." in word:
                continue
            if "(" in word:
                #remove the (1) (2) etc
                word = word.split('(')[0]
            # make word lower case
            word = word.lower()
            phonemes = parts[1]
            phArr = phonemes.split(' ')
            # Write the processed line to the output file
            output_file.write(f'ph({word}, {phArr}).\n')  # Add a newline after each processed line
except FileNotFoundError:
    print(f"Input file not found at path: {input_file_path}")
except Exception as e:
    print(f"An error occurred: {e}")
