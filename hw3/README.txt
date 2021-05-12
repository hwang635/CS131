How to test:
compress w/ jpigz
decompress compressed file.gz w/ pigz -d file.gz
cmp decompressed file w/ original file.txt

javac Pigzj.java
java Pigzj <file.txt >file.gz
pigz -d <file.gz | cmp file file.txt

java Pigzj <book.txt >book.gz
pigz -d <book.gz | cmp book book.txt

Sources:
https://stackoverflow.com/questions/21287715/standard-inputpiping-txt-file-and-readline-results-in-a-idling-state
https://stackoverflow.com/questions/5488072/reading-in-from-system-in-java
https://www.geeksforgeeks.org/thread-pools-java/
https://www.geeksforgeeks.org/runnable-interface-in-java/
https://www.w3schools.com/java/java_arraylist.asp
http://tutorials.jenkov.com/java-concurrency/creating-and-starting-threads.html
