package adept.logging;

public class TextLogger implements JavaLogger {
  public static int DEBUG = 0;
  public static int INFO = 1;
  public static int WARN = 2;
  public static int ERROR = 3;

  final private int level;

  public TextLogger(int level) {
    this.level = level;
  }

  public void debug(String message) {
    if (level <= DEBUG) {
      System.out.println("[DEBUG] " + message);
    }
  }

  public void info(String message) {
    if (level <= INFO) {
      System.out.println("[INFO] " + message);
    }
  }

  public void warn(String message) {
    if (level <= WARN) {
      System.out.println("[WARN] " + message);
    }
  }

  public void error(String message) {
    if (level <= ERROR) {
      System.out.println("[ERROR] " + message);
    }
  }

  public void error(String message, Exception exception) {
    if (level >= ERROR) {
      System.out.println("[DEBUG] " + message + ". Stack trace: ");
      exception.printStackTrace();
    }
  }
}
