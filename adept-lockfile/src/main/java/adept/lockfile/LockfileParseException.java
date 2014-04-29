package adept.lockfile;

public class LockfileParseException extends Exception {
  private static final long serialVersionUID = -652005626925351016L;

  LockfileParseException(Exception cause) {
    super("Parsing of lockfile failed", cause);
  } 
}
