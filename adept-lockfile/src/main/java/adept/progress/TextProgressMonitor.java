package adept.progress;

import java.io.PrintStream;

public class TextProgressMonitor implements ProgressMonitor {
  private String currentStatusString = "";
  private String lastMessage = "";
  private int currentProgress = 0;
  private int currentMax = 0;

  static private PrintStream printStream = System.err;

  @Override
  public void beginTask(String status, int max) {
    if (max > 0) {
      currentMax = max;
      currentStatusString = status;
    } else
      throw new RuntimeException("Cannot start a task with less than 0: " + max);
  }

  @Override
  public void update(int i) {
    synchronized (lastMessage) { //FIXME: jeeez! we can't do synchronize here really, first one to fix get's a cake - I promise!
      if (!currentStatusString.isEmpty()) {
        currentProgress += i; 
        String progress;
        if (currentMax > 0) {
          progress = String.format("%.0f%%", ((currentProgress * 100.0) / currentMax));
        } else {
          progress = String.format("%s", currentProgress);
        }
        wipe();
        lastMessage = currentStatusString + ": " + progress;
        printStream.print(lastMessage);
      }
    }
  }

  private void wipe() {
    String wipeoutString = "";
    for (int j = 0; j < lastMessage.length(); j++) {
      wipeoutString += "\r";
    }
    printStream.print(wipeoutString);
  }

  @Override
  public void endTask() {
    wipe();
    printStream.println(lastMessage);
    lastMessage = "";
    currentStatusString = "";
    currentProgress = 0;
    currentMax = 0;
  }

}
