package adept.artifact;

import java.io.File;
import adept.artifact.models.Artifact;

public class ArtifactDownloadResult {
  final public Artifact artifact;
  final public Exception exception; //null if failed
  final public File tmpFile; //null if no tmpFile was used
  final public String filename; //null if exception is set
  private File cachedFile; //null if failed
  
  public ArtifactDownloadResult(Artifact artifact, String filename) {
    this.artifact = artifact;
    this.filename = filename;
    this.exception = null;
    this.tmpFile = null;
  }
  
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
    return exception != null && cachedFile == null;
  }

  public boolean isSuccess() {
    return exception == null && cachedFile != null;
  }

  public File getCachedFile() {
    return cachedFile;
  }

  public void setCachedFile(File cachedFile) {
    this.cachedFile = cachedFile;
  }
}
