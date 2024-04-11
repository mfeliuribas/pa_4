####################################
# Praat script to create textgrids #
# Created by                       #
# Joseph V. Casillas 04/06/2022    #
####################################


form Enter information
	comment Participant ID (folder with .wav files):
	sentence fileID ne03
endform


#
# Enter the path to where the files are kept -------------------
#

dirFiles$ = "../recordings/"+fileID$+"/wavs/"
newDir$ = "../recordings/"+fileID$+"/wavs/"
number = 1

# --------------------------------------------------------------







#
# Prepare the loop ---------------------------------------------
#

# Find the .wav files
Create Strings as file list: "allFiles", dirFiles$ + "/*.wav"

# Select allFiles
select Strings allFiles

# Count number of stings (this is the iterator for the loop!)
numberOfFiles = Get number of strings

# Clear info window just in case
clearinfo

# --------------------------------------------------------------








#
# Begin loop ---------------------------------------------------
#

for i from number to 45
	select Strings allFiles
	fileName$ = Get string... i
	prefix$ = fileName$ - ".wav"
	Read from file... 'dirFiles$'/'fileName$'
	nameSound$ = selected$("Sound")
        To TextGrid: "release voice vowel note", "release voice"
	select TextGrid 'nameSound$'
        select Sound 'nameSound$'
	plus TextGrid 'nameSound$'
	Edit
	pause Continue?
	select Sound 'nameSound$'
	Write to WAV file... 'newDir$'/'nameSound$'.wav
	select TextGrid 'nameSound$'
	Write to binary file... 'newDir$'/'nameSound$'.TextGrid
	select all
	minus Strings allFiles
	Remove
	printline 'nameSound$'	'i'
endfor

# --------------------------------------------------------------

