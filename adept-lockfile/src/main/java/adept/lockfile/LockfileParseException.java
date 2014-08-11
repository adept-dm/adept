package adept.lockfile;

import java.io.File;

public class LockfileParseException extends Exception {
  private static final long serialVersionUID = -652005626925351016L;

  LockfileParseException(File file, Exception cause) {
    super(String.format("Parsing of lockfile '%s' failed", file.getName()), cause);
  }
  LockfileParseException(String cause) { super(cause); }
}
