package adept.hash;

import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import static javax.xml.bind.DatatypeConverter.printHexBinary;

public class Hasher {

  static ThreadLocal<MessageDigest> md = new ThreadLocal<MessageDigest>() {
    @Override
    public MessageDigest initialValue() {
      try {
        return MessageDigest.getInstance("SHA-256");
      } catch (Exception exception) {
        throw new RuntimeException(exception);
      }
    }
  };

  public static String hash(byte[] bytes) {
    MessageDigest currentMd = md.get();
    currentMd.reset();
    try {
      currentMd.update(bytes);
      return printHexBinary(currentMd.digest()).toLowerCase();
    } finally {
      currentMd.reset();
    }
  }

  public static String hash(InputStream is) throws IOException {
    MessageDigest currentMd = md.get();
    currentMd.reset();
    try {
      byte[] buf = new byte[1024 * 4]; // _seemed_ to be the fastest when I
                                       // tried it out when I was writing the
                                       // scala version of this file
      int len = is.read(buf);

      //streaming was much more efficient than using digest on Array[Byte] - to be verified...
      while (len > 0) {
        currentMd.update(buf, 0, len);
        len = is.read(buf);
      }
      return printHexBinary(currentMd.digest()).toLowerCase();
    } finally {
      currentMd.reset();
    }
  }

}
