HW3 README (CS131, 5/8/21)

Summary:
I based the compression off the TA's started code and used the Runnable interface
with an Executor threadpool to do multithreading. My goal was to have multiple threads
read in from input and divide read in input into blocks, and then each read thread
would have P number of processes to do compression. After the compression was done,
the original starter class would join all of the compressed blocks together to print
them out. My code works by having the first Pigzj class parse command line input to
get the number P of processes if passed in and the default if not; then it instantiates
an instance of the starter code's SingleThreadedGZipCompressor. SingleThreadedGZipCompressor
writes to the header and then creates a Thread for RunnableInput, which implements the
Runnable interface so that it can be used by a thread and is in charge of reading in
the input. I originally attempted to create a threadpool with several RunnableInput 
threads but stuck with just 1 thread because I couldn't make it work. The RunnableInput
class initialises a BufferedInputStream to read in from standard input, instantiates
an ArrayList of RunnableCompress to keep track of compressions, and creates a threadPool
with P number of threads to have multithreaded compression. RunnableInput reads in the
input and loops through BLOCK_SIZE bytes. Each iteration, it creates a new RunnableCompress 
task, adds it to the threadpool to execute, and adds it to the ArrayList to keep track of.
RunnableCompress performs compression of whatever block it's given; it implements Runnable
so that it can be used by threads. It keeps a boolean of whether it's finished or not so that
SingleThreadedGZipCompressor can check when all compressions are done. After starting the
RunnableInput thread to take care of reading and dividing input, SingleThreadedGZipCompressor
continuously polls in order until all blocks have finished compressing. Finally, when all
blocks are done it goes through the ArrayList of compressed blocks to print them out
in order.

There are several potential problems or blocks in my code. One inefficiency is that there is
only 1 reader thread; I tried to have multiple threads with a threadpool for RunnableInput
threads but was unsuccessful. Another major inefficiency is how SingleThreadedGZipCompressor
waits for completion. SingleThreadedGZipCompressor polls for completion by continously checking
to see if the RunnableInput thread has died and the index has gone through the length of 
the compressed ArrayList, but this is very inefficient since it goes in order and stays spinning
at the current index until that block has completely finished processing. The output is only
printed out after that step, when all blocks have finished, which is again inefficient because
of the extra waiting, but I wasn't able to get it reliably working in a more efficient way.
I expect that the overhead from waiting for completion gets much worse when file size gets
bigger and/or there are more threads, since there are more blocks and threads to wait on and
perform I/O operations for. Space usage will also get worse, since more RunnableCompress 
tasks will be needed. However, more threads may amortise the overhead of creating 
the threadpool. One also problem I noticed is that when a file is extremely large or there 
are many threads, the program hangs -- I'm not sure why that is, but I hypothesise
that there was some issue with creating/assigning the compression tasks and threadpool that
I overlooked.

Performance measurement:
I measured on lnxsrv12 using a book file downloaded from Project Gutenberg.
Below is the data for each trial's time and compression level. Looking at the
timing data shows that on average, pigz is reliably faster than gzip and
both are faster than Pigz. As the number of threads increase, pigz stayed
about the same speed while Pigz became a little bit faster. Pigzj had a
noticeably greater sys time. The compression ratios for gzip, pigz, and Pigzj
were approximately the same for all trials; the number of threads didn't affect
the compression ratio.

DEFAULT, 3x:
gzip 
real    0m0.181s    0m0.036s    0m0.029s ==> Average = 0m0.082s
user    0m0.021s    0m0.022s    0m0.022s ==> Average = 0m0.022s 
sys     0m0.004s    0m0.002s    0m0.002s ==> Average = 0m0.003s
gzip COMPRESSION (same for each trial):
compressed   original reduced
102893       277209   62.9%  

pigz
real    0m0.056s    0m0.019s    0m0.020s ==> Average = 0m0.032s
user    0m0.025s    0m0.026s    0m0.024s ==> Average = 0m0.025s
sys     0m0.003s    0m0.001s     0m0.005s ==> Average = 0m0.003s
pigz COMPRESSION (same for each trial):
compressed   original reduced
103119       277209   62.8% 

Pigzj
real    0m0.072s    0m0.084s    0m0.078s ==> Average = 0m0.078s
user    0m0.093s    0m0.086s    0m0.069s ==> Average = 0m0.082s
sys     0m0.030s    0m0.024s    0m0.029s ==> Average = 0m0.028s
Pigzj COMPRESSION (same for each trial):
compressed   original reduced
103119       277209   62.8%

10 PROCESSES, 3x:
pigz -p 10:
real    0m0.019s    0m0.018s    0m0.019s
user    0m0.025s    0m0.026s    0m0.024s
sys     0m0.002s    0m0.001s    0m0.004s
pigz COMPRESSION (same for each trial):
compressed   original reduced
103119       277209   62.8% 

Pigzj -p 10
real    0m0.091s    0m0.074s    0m0.075s
user    0m0.079s    0m0.077s    0m0.075s
sys     0m0.032s    0m0.027s    0m0.030s
Pigzj COMPRESSION (same for each trial):
compressed   original reduced
103119       277209   62.8%

20 PROCESSES, 3x:
pigz -p 20
real    0m0.019s    0m0.019s    0m0.021s
user    0m0.024s    0m0.023s    0m0.024s
sys     0m0.003s    0m0.005s    0m0.003s
pigz COMPRESSION (same for each trial):
compressed   original reduced
103119       277209   62.8% 

Pigzj -p 20
real    0m0.074s     0m0.082s   0m0.081s
user    0m0.085s     0m0.083s   0m0.079s
sys     0m0.021s     0m0.025s   0m0.023s
Pigzj COMPRESSION (same for each trial):
compressed   original reduced
103119       277209   62.8%

STRACE analysis:
My strace results, shown below, explain one factor of why my Pigzj implementation is 
significantly slower than the gzip and pigz programs. Pigzj has a much greater
system time, as shown in the data above. The strace summary shows that gzip and pigz
spend most of their time on read and write syscalls; they have relatively few 
syscalls and none of their calls have very long times per call. However, Pigzj
has significantly more system calls and to more functions; in particular it calls
futex, which is very time-expensive, and other time-expensive functions much
more often.  

strace -c gzip <$input >gzip.gz
gzip:
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
  0.00    0.000000           0        11           read
  0.00    0.000000           0         7           write
  0.00    0.000000           0         4           close
  0.00    0.000000           0         3           fstat
  0.00    0.000000           0         1           lseek
  0.00    0.000000           0         5           mmap
  0.00    0.000000           0         4           mprotect
  0.00    0.000000           0         1           munmap
  0.00    0.000000           0         1           brk
  0.00    0.000000           0        12           rt_sigaction
  0.00    0.000000           0         1         1 ioctl
  0.00    0.000000           0         1         1 access
  0.00    0.000000           0         1           execve
  0.00    0.000000           0         2         1 arch_prctl
  0.00    0.000000           0         2           openat
------ ----------- ----------- --------- --------- ----------------
100.00    0.000000                    56         3 total

pigz:
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
 41.69    0.000296          22        13           read
 14.79    0.000105           5        21           mmap
 11.97    0.000085           6        14           mprotect
  9.15    0.000065          16         4           clone
  3.66    0.000026           4         6           openat
  2.82    0.000020           3         6           brk
  2.54    0.000018           2         7           munmap
  2.39    0.000017           2         6           close
  2.25    0.000016           2         6           fstat
  1.83    0.000013           4         3           lseek
  1.83    0.000013           4         3           rt_sigaction
  1.27    0.000009           4         2         2 ioctl
  0.70    0.000005           2         2         1 arch_prctl
  0.70    0.000005           0         7         2 futex
  0.70    0.000005           5         1           set_tid_address
  0.56    0.000004           4         1           rt_sigprocmask
  0.56    0.000004           4         1           set_robust_list
  0.56    0.000004           4         1           prlimit64
  0.00    0.000000           0         1         1 access
  0.00    0.000000           0         1           execve
------ ----------- ----------- --------- --------- ----------------
100.00    0.000710                   106         6 total

Pigzj:
strace -c java Pigzj <$input >Pigzj.gz
% time     seconds  usecs/call     calls    errors syscall
------ ----------- ----------- --------- --------- ----------------
 93.29    0.017444        8722         2           futex
  2.15    0.000402           8        49        39 openat
  1.06    0.000198           6        33        30 stat
  0.83    0.000155           6        23           mmap
  0.66    0.000124           8        15           mprotect
  0.43    0.000080          26         3           munmap
  0.33    0.000062           5        12           read
  0.28    0.000053           5        10           fstat
  0.27    0.000051           4        11           close
  0.15    0.000028          14         2           readlink
  0.12    0.000023          23         1           clone
  0.09    0.000017           4         4           brk
  0.07    0.000014           7         2         1 access
  0.06    0.000012           4         3           lseek
  0.04    0.000008           4         2           rt_sigaction
  0.04    0.000008           4         2         1 arch_prctl
  0.02    0.000004           4         1           rt_sigprocmask
  0.02    0.000004           4         1           getpid
  0.02    0.000004           4         1           set_tid_address
  0.02    0.000004           4         1           set_robust_list
  0.02    0.000004           4         1           prlimit64
  0.00    0.000000           0         1           execve
------ ----------- ----------- --------- --------- ----------------
100.00    0.018699                   180        71 total

Tests:
javac Pigzj.java
java Pigzj <file.txt >file.gz
pigz -d <file.gz | cmp - file.txt
java Pigzj <book.txt >book.gz
pigz -d <book.gz | cmp - book.txt
java Pigzj <README.txt >README.gz
pigz -d <README.gz | cmp - README.txt

pigz -lv file.gz (gets compression info + ratio)

Resources:
I heavily relied on the started code for the input reading and compression parts. I also
looked at MessAdmin and Pigz for understanding how to divide up blocks and ideas on
parallelisation, and some online resources for threading I linked below for reference.

https://stackoverflow.com/questions/21287715/standard-inputpiping-txt-file-and-readline-results-in-a-idling-state
https://stackoverflow.com/questions/5488072/reading-in-from-system-in-java
https://www.geeksforgeeks.org/thread-pools-java/
https://www.geeksforgeeks.org/runnable-interface-in-java/
https://www.w3schools.com/java/java_arraylist.asp
http://tutorials.jenkov.com/java-concurrency/creating-and-starting-threads.html
