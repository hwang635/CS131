How to test:
compress w/ jpigz
decompress compressed file.gz w/ pigz -d file.gz
cmp decompressed file w/ original file.txt

javac Pigzj.java
java Pigzj <file.txt >file.gz
pigz -d <file.gz | cmp file file.txt

Sources:
https://stackoverflow.com/questions/21287715/standard-inputpiping-txt-file-and-readline-results-in-a-idling-state
https://stackoverflow.com/questions/5488072/reading-in-from-system-in-java
