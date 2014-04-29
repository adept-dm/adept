package adept.artifact;

import java.io.File;
import adept.artifact.models.Artifact;

public class ArtifactDownloadResult {
  final public Artifact artifact;
  final public Exception exception; //null if tmpFile is set
  final public File tmpFile; //null if exception is set
  final public String filename; //null if exception is set
  private File cachedFile;
  
  public ArtifactDownloadResult(Artifact artifact, File tmpFile, String filename) {
    this.artifact = artifact;
    this.filename = filename;
    this.exception = null;
    this.tmpFile = tmpFile;
  }

  public ArtifactDownloadResult(Artifact artifact, String filename, Exception exception) {
    this.artifact = artifact;
    this.exception = exception;
    this.tmpFile = null;
    this.filename = filename;
  }
  
  public boolean isFailed() {
    return exception != null && tmpFile == null;
  }

  public boolean isSuccess() {
    return exception == null && tmpFile != null;
  }

  public File getCachedFile() {
    return cachedFile;
  }

  public void setCachedFile(File cachedFile) {
    this.cachedFile = cachedFile;
  }
}
