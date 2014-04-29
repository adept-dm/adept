package adept.progress;

public interface ProgressMonitor {
  public void beginTask(String status, int max);
  public void update(int i);
  public void endTask();
}
