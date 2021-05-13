import java.util.zip.*;
import java.io.*;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

class SingleThreadedGZipCompressor {
    public final static int BLOCK_SIZE = 131072;
    public final static int DICT_SIZE = 32768;
    private final static int GZIP_MAGIC = 0x8b1f;
    private final static int TRAILER_SIZE = 8;
    
    public ByteArrayOutputStream outStream;
    private CRC32 crc = new CRC32();
    //# of processes/threads
    private int numThreads;

   /* Reads in input from filename */
   public SingleThreadedGZipCompressor(int p) {
      //Set # of processes
      numThreads = p;
      //Init output stream
      this.outStream = new ByteArrayOutputStream();
   }

   private void writeHeader() throws IOException {
      outStream.write(new byte[] {
         (byte) GZIP_MAGIC,        // Magic number (short)
         (byte)(GZIP_MAGIC >> 8),  // Magic number (short)
         Deflater.DEFLATED,        // Compression method (CM)
         0,                        // Flags (FLG)
         0,                        // Modification time MTIME (int)
         0,                        // Modification time MTIME (int)
         0,                        // Modification time MTIME (int)
         0,                        // Modification time MTIME (int)Sfil
         0,                        // Extra flags (XFLG)
         0                         // Operating system (OS)
      });
   }

    /*
     * Writes GZIP member trailer to a byte array, starting at a given
     * offset.
     */
    private void writeTrailer(long totalBytes, byte[] buf, int offset) throws IOException {
        writeInt((int)crc.getValue(), buf, offset); // CRC-32 of uncompr. data
        writeInt((int)totalBytes, buf, offset + 4); // Number of uncompr. bytes
    }

    /*
     * Writes integer in Intel byte order to a byte array, starting at a
     * given offset.
     */
    private void writeInt(int i, byte[] buf, int offset) throws IOException {
        writeShort(i & 0xffff, buf, offset);
        writeShort((i >> 16) & 0xffff, buf, offset + 2);
    }

    /*
     * Writes short integer in Intel byte order to a byte array, starting
     * at a given offset
     */
   private void writeShort(int s, byte[] buf, int offset) throws IOException {
        buf[offset] = (byte)(s & 0xff);
        buf[offset + 1] = (byte)((s >> 8) & 0xff);
   }

   public void compress() throws FileNotFoundException, IOException {
      this.writeHeader();
      this.crc.reset();

      /* Only 1 reader thread (tried to have multiple, didn't work) */
      /*ExecutorService readerThreadPool = Executors.newFixedThreadPool(1);  
      RunnableInput inputReader = new RunnableInput(crc, numThreads);
      readerThreadPool.execute(inputReader); */
      RunnableInput inputReader = new RunnableInput(crc, numThreads); //Needs to be same crc or else mismatch
      Thread inputReaderThread = new Thread(inputReader);
      inputReaderThread.start();

      ArrayList<RunnableCompress> compressedBlocks = inputReader.threadArr;
      boolean allDone = false;
      int i = 0;
      int numBlocks = 0;
      //Loop until all blocks done deflating
      for(;;) {
         numBlocks = compressedBlocks.size();
         //If went through everything in array + thread dead, done!
         if(i >= numBlocks && !inputReaderThread.isAlive()) {
            allDone = true;
            break;
         } else if (i >= numBlocks) {
            continue;
         } else { //i < numBlocks
            //This block is done, mark as done + write
            if(compressedBlocks.get(i).getFinished()) {
               ++i;
            }
         }
      }

      //When all compressions done, print out all blocks in order
      if(allDone == true) {
         for(int j = 0; j<numBlocks; ++j) {
            RunnableCompress currentBlock = compressedBlocks.get(j);
            outStream.write(currentBlock.cmpBlockBuf, 0, currentBlock.getDeflatedBytes());
         }
         //inputReaderThread.stop(); DEPRECATED
      }

      /* Finally, write the trailer and then write to STDOUT */
      byte[] trailerBuf = new byte[TRAILER_SIZE];
      writeTrailer(inputReader.getTotalBytesRead(), trailerBuf, 0);
      outStream.write(trailerBuf);

      outStream.writeTo(System.out);
   }
}

//Implements Runnable to divide reading input into blocks + have P threads to compress
class RunnableInput implements Runnable {
   public final static int BLOCK_SIZE = 131072;
   public final static int DICT_SIZE = 32768;
   private final static int GZIP_MAGIC = 0x8b1f;

   public ArrayList<RunnableCompress> threadArr; //Public so SingleThreaded can access
   private CRC32 crc;
   private int numThreads; // # of P processes
   private int totalBytesRead;
   private boolean isFinished;

   private BufferedInputStream inputReader; //Holds input from stdin
   private ExecutorService threadPool;

   private byte[] blockBuf = new byte[BLOCK_SIZE];
   private byte[] cmpBlockBuf = new byte[BLOCK_SIZE * 2];
   private byte[] dictBuf = new byte[DICT_SIZE];

   public RunnableInput(CRC32 crc, int numThreads) {
      this.crc = crc;
      this.numThreads = numThreads;
      threadArr = new ArrayList<RunnableCompress>();
      totalBytesRead = 0;
      isFinished = false;

      //Init input to read in from stdin
      inputReader = new BufferedInputStream(System.in);
      //Init threadpool w/ P threads
      threadPool = Executors.newFixedThreadPool(numThreads);  
   }

   //Getter methods
   public int getTotalBytesRead() {
      return totalBytesRead;
   }
   public boolean getFinished() {
      return isFinished;
   }

   /* Read input from stdin, based off starter code */
   public void run() {
      this.crc.reset();

      /* reads next BLOCK_SIZE # of bytes into blockBuf, rets # of bytes read or -1 if end reached */
      int currentBytesRead = 0;
      try {
         currentBytesRead = inputReader.read(blockBuf, 0, BLOCK_SIZE);
      } catch (IOException e) {
         System.err.println("Error reading in from input.");
         System.exit(1);
      }

      boolean hasDict = false;
      boolean lastBlock = false;
       /* Loop + read in data until end of stream reached */
      while (currentBytesRead > 0) {
         /* Update the CRC checksum every time we read in a new block. */
         crc.update(blockBuf, 0, currentBytesRead);
     
         /* If # of bytes read < block_size, this is the last block bc no more chars to read. */
         if (currentBytesRead < BLOCK_SIZE) {
            lastBlock = true;
         }
         //Create new compress task, exec + add to arraylist to keep track, + add to threadpool to exec
         RunnableCompress newCompress = new RunnableCompress(currentBytesRead, blockBuf, dictBuf, hasDict, lastBlock);
         threadPool.execute(newCompress);
         threadArr.add(newCompress);

         /* If we read in enough bytes in this block, store the last part as the dictionary for the
         next iteration */ 
         if (currentBytesRead >= DICT_SIZE) {
            System.arraycopy(blockBuf, currentBytesRead - DICT_SIZE, dictBuf, 0, DICT_SIZE);
            hasDict = true;
         } else {
            hasDict = false;
         }
         
         totalBytesRead += currentBytesRead; //Add # bytes to counter before next read
         try {
            currentBytesRead = inputReader.read(blockBuf, 0, BLOCK_SIZE);
         } catch (IOException e) {
            System.err.println("Error reading in from input.");
            System.exit(1);
         }
      }

      //When all tasks done, shutdown threadpool
      isFinished = true;
      threadPool.shutdown();
   }
}

//Implements Runnable interface to have compression performed by threads
class RunnableCompress implements Runnable {
   public final static int BLOCK_SIZE = 131072;
   public final static int DICT_SIZE = 32768;
   //# of bytes in + out
   private int currentBytes;
   private int deflatedBytes;

   /* Buffers to store dict + input, init to empty max size */
   private byte[] blockBuf = new byte[BLOCK_SIZE];
   private byte[] dictBuf = new byte[DICT_SIZE];
   public byte[] cmpBlockBuf = new byte[BLOCK_SIZE*2]; //public so SingleThreaded can access

   private boolean hasDict;
   private boolean lastBlock;
   private boolean isFinished;

   public RunnableCompress(int numBytes, byte[] blockBuf, byte[] dictBuf, boolean hasDict, boolean lastBlock) {
      //Copy in buffer arrays (src, src offset, dest, dest offset, # elem)
      System.arraycopy(blockBuf, 0, this.blockBuf, 0, numBytes);
      System.arraycopy(dictBuf, 0, this.dictBuf, 0, dictBuf.length); 

      this.currentBytes = numBytes;
      this.hasDict = hasDict;
      this.lastBlock = lastBlock;
      this.isFinished = false;
      this.deflatedBytes = 0;
   }

   //Getter methods
   public boolean getFinished() {
      return isFinished;
   }
   public int getDeflatedBytes() {
      return deflatedBytes;
   }

   //Do the compression for this block (from starter code)
   public void run() {
       /* Init deflator that does compressing */
      Deflater compressor = new Deflater(Deflater.DEFAULT_COMPRESSION, true);
   
      //Resets so new input data can be processed
      compressor.reset();

      /* If we saved a dictionary from the last block, prime the deflater with it */
      if (hasDict) {
         compressor.setDictionary(dictBuf);
      }

      //Set input data for compression
      compressor.setInput(blockBuf, 0, currentBytes);

      //If last block, need to clean deflator
      if(lastBlock == true) {
         if (!compressor.finished()) {
            compressor.finish(); //Comp ends w/ current contents of input buffer
            while (!compressor.finished()) {
               //Compress input + fills buffer w/ compressed data, rets # of bytes written + writes to output
               deflatedBytes = compressor.deflate(cmpBlockBuf, 0, cmpBlockBuf.length, Deflater.NO_FLUSH);
            }
         }
      } else { //Just deflate + don't need to clean out
         deflatedBytes = compressor.deflate(cmpBlockBuf, 0, cmpBlockBuf.length, Deflater.SYNC_FLUSH);
      }

      isFinished = true; //Done, so mark as finished
   }
}

public class Pigzj {
   public static void main (String[] args) throws FileNotFoundException, IOException {      
      int numProc = -1;
      //No p argument, default # of processes
      if(args.length == 0) {
         numProc = Runtime.getRuntime().availableProcessors();
      } else if (args.length == 2) { //Correct # of args, find req # of processes 
         if(!args[0].equals("-p")) {
            System.err.println("Error: only valid argument is -p processes.");
            System.exit(1);
         }
         //Check that # entered is valid + set if valid
         try {
            int proc = Integer.parseInt(args[1]);
            numProc = proc;
            if(proc < 1) {
               System.err.println("Error: # of processes must be an positive integer.");
               System.exit(1);
            }
         } catch (NumberFormatException e) {
            System.err.println("Error: # of processes must be an positive integer.");
            System.exit(1);
         }
      } else { //Incorrect # of args
         System.err.println("Error: invalid number of arguments.");
         System.exit(1);
      }

      SingleThreadedGZipCompressor cmp = new SingleThreadedGZipCompressor(numProc);
      cmp.compress();
   }
}