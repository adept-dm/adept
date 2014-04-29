package adept.artifact;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.net.URLConnection;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.util.concurrent.Callable;

import adept.artifact.models.Artifact;
import adept.artifact.models.ArtifactHash;
import adept.artifact.models.ArtifactLocation;
import adept.logging.JavaLogger;
import adept.progress.ProgressMonitor;

public class ArtifactDownloader implements Callable<ArtifactDownloadResult> {

  static public String USER_AGENT = "AdeptDownloader/1.0-ALPHA";
  static private int MAX_RETRIES_DEFAULT = 5;

  final private Artifact artifact;
  final private File tmpFile;
  final private JavaLogger logger;
  final private int maxRetries;
  final private String filename;
  final private ProgressMonitor progress;
  private File baseDir;

  public ArtifactDownloader(Artifact artifact, String filename, File tmpFile, JavaLogger logger, ProgressMonitor progress) {
    this.artifact = artifact;
    this.tmpFile = tmpFile;
    this.filename = filename;
    this.logger = logger;
    this.progress = progress;
    this.maxRetries = MAX_RETRIES_DEFAULT;
  }

  public ArtifactDownloader(File baseDir, Artifact artifact, String filename, File tmpFile, int maxRetries,
      JavaLogger logger, ProgressMonitor progress) {
    this.baseDir = baseDir;
    this.artifact = artifact;
    this.tmpFile = tmpFile;
    this.filename = filename;
    this.logger = logger;
    this.maxRetries = maxRetries;
    this.progress = progress;
  }

  protected boolean verify = true;

  @Override
  public ArtifactDownloadResult call() throws Exception {
    if (artifact.locations.size() != 1)
      throw new Exception("Locations different from 1 is not currently implemented and we got: " + artifact); // TODO:
                                                                                                              // implement!
    ArtifactLocation location = artifact.locations.iterator().next();
    URL url = location.getURL();
    int retries = 0;
    ArtifactDownloadResult result = null;
    while (result == null) {
      FileOutputStream fos = null;
      InputStream is = null;
      try {
        ArtifactHash hash = artifact.hash;
        File currentCachedFile = ArtifactCache.getOrCreateExistingCacheFile(baseDir, hash, filename, verify);
        if (currentCachedFile != null && currentCachedFile.isFile()) {
          result = new ArtifactDownloadResult(artifact, tmpFile, filename);
          result.setCachedFile(currentCachedFile);
        } else {
          URLConnection conn = url.openConnection();
          conn.setRequestProperty("User-Agent", USER_AGENT);
          is = conn.getInputStream();
          ReadableByteChannel rbc = Channels.newChannel(is);
          fos = new FileOutputStream(tmpFile);
          fos.getChannel().transferFrom(rbc, 0, Long.MAX_VALUE);
          result = new ArtifactDownloadResult(artifact, tmpFile, filename);
          File srcFile = tmpFile;
          result.setCachedFile(ArtifactCache.cache(baseDir, srcFile, hash, filename));
        }
        progress.update(artifact.size.intValue());

      } catch (IOException ioException) {
        logger.debug("Got exception: " + ioException.getMessage() + " cause: " + ioException.getCause());
        retries = retries + 1;
        if (retries > maxRetries)
          result = new ArtifactDownloadResult(artifact, filename, ioException);
        else {
          logger.debug("Sleeping before retrying...");
          Thread.sleep(500); // sleeping in a callable makes me worried.... make
                             // sure we are not doing something too stupid
                             // here...
        }
      } finally {
        if (is != null) {
          is.close();
        }
        if (fos != null) {
          fos.close();
        }
      }
    }
    return result;
  }
}
