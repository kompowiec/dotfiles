:%s/^/"/: # add something to begin line
#Press 0 to go to the beginning of a line, or ^ to go to the first non-blank character in a line.
:%s/$/",/ # add something to end of the line
gq #wrap lines
vipJ #unwrap lines
:g/^$/d - Remove all blank lines
%s/.\{-}\(http[s]\?:\/\/[[:alnum:]%\/_#.-]*\)/\1\r/g|g!//d #parse HTML links
#What's a quick way to comment/uncomment lines in Vim?
#visually select the text rows (using V as usual)
norm i# #commenting
norm x #uncommenting

