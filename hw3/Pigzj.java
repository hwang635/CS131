import java.util.zip.*;
import java.io.*;
import java.nio.file.*;

class SingleThreadedGZipCompressor {
    public final static int BLOCK_SIZE = 131072;
    public final static int DICT_SIZE = 32768;
    private final static int GZIP_MAGIC = 0x8b1f;
    private final static int TRAILER_SIZE = 8;
    
    BufferedInputStream inputReader; //Holds input from stdin
    public ByteArrayOutputStream outStream;
    private CRC32 crc = new CRC32();

    //# of processes
    int numProc = -1;

   /* Reads in input from filename */
   public SingleThreadedGZipCompressor(int p) {
      //Set # of processes
      int numProc = p;

      //Read in from stdin + puts input in inputReader
      inputReader = new BufferedInputStream(System.in);

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

      /* Buffers for input blocks, compressed bocks, and dictionaries */
      byte[] blockBuf = new byte[BLOCK_SIZE];
      byte[] cmpBlockBuf = new byte[BLOCK_SIZE * 2];
      byte[] dictBuf = new byte[DICT_SIZE];
 
      /* Init deflator that does compressing */
      Deflater compressor = new Deflater(Deflater.DEFAULT_COMPRESSION, true);

      /* Init bytes counter + hasDict bool to keep track of processed input */
      long totalBytesRead = 0;
      boolean hasDict = false;
      /* reads next BLOCK_SIZE # of bytes into blockBuf, rets # of bytes read or -1 if end reached */
      int currentBytesRead = inputReader.read(blockBuf, 0, BLOCK_SIZE);
 
      /* Loop + read in data until end of stream reached */
      while (currentBytesRead > 0) {
         totalBytesRead += currentBytesRead;
         /* Update the CRC checksum every time we read in a new block. */
         crc.update(blockBuf, 0, currentBytesRead);
         
         //Resets so new input data can be processed
         compressor.reset();

         /* If we saved a dictionary from the last block, prime the deflater with it */
         if (hasDict) {
            compressor.setDictionary(dictBuf);
         }
         
         //Set input data for compression
         compressor.setInput(blockBuf, 0, currentBytesRead);
     
         /* If # of bytes read < block_size, this is the last block bc no more chars to read.
            We have to clean out the deflater properly */
         if (currentBytesRead < BLOCK_SIZE) {
            if (!compressor.finished()) {
               compressor.finish(); //Comp ends w/ current contents of input buffer
               while (!compressor.finished()) {
                  //Compressed input + fills buffer w/ compressed data, rets # of bytes written + writes to output
                  int deflatedBytes = compressor.deflate(cmpBlockBuf, 0, cmpBlockBuf.length, Deflater.NO_FLUSH);
                  if (deflatedBytes > 0) {
                     outStream.write(cmpBlockBuf, 0, deflatedBytes);
                  }
               }
            }
         } else {
         /* Otherwise, just deflate and then write the compressed block out. Not using SYNC_FLUSH here leads to
            some issues, but using it probably results in less efficient compression. There's probably a better
            way to deal with this. */
            int deflatedBytes = compressor.deflate(cmpBlockBuf, 0, cmpBlockBuf.length, Deflater.SYNC_FLUSH);
            if (deflatedBytes > 0) {
                  outStream.write(cmpBlockBuf, 0, deflatedBytes);
            }
         }

         /* If we read in enough bytes in this block, store the last part as the dictionary for the
         next iteration */ 
         if (currentBytesRead >= DICT_SIZE) {
            System.arraycopy(blockBuf, currentBytesRead - DICT_SIZE, dictBuf, 0, DICT_SIZE);
            hasDict = true;
         } else {
            hasDict = false;
         }
         
         currentBytesRead = inputReader.read(blockBuf, 0, BLOCK_SIZE);
      }

      /* Finally, write the trailer and then write to STDOUT */
      byte[] trailerBuf = new byte[TRAILER_SIZE];
      writeTrailer(totalBytesRead, trailerBuf, 0);
      outStream.write(trailerBuf);

      outStream.writeTo(System.out);
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
         }
         //Check that # entered is valid + set if valid
         try {
            int proc = Integer.parseInt(args[1]);
            numProc = proc;
            if(proc < 1) {
               System.err.println("Error: # of processes must be an positive integer.");
            }
         } catch (NumberFormatException e) {
            System.err.println("Error: # of processes must be an positive integer.");
         }
      } else { //Incorrect # of args
         System.err.println("Error: invalid number of arguments.");
      }

      SingleThreadedGZipCompressor cmp = new SingleThreadedGZipCompressor(numProc);
      //System.out.println(numProc);
      cmp.compress();
   }
}