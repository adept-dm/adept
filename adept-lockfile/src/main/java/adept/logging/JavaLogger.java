package adept.logging;

public interface JavaLogger {
  public void debug(String message);

  public void info(String message);

  public void warn(String message);

  public void error(String message);

  public void error(String message, Exception exception);
}
