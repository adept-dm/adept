package adept.artifact;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import adept.artifact.models.ArtifactHash;
import adept.hash.Hasher;

public class ArtifactCache {
  public static String ARTIFACT_CACHE_DIR = "cache";
  public static int LEVEL_1_LENGTH = 4;
  public static int LEVEL_2_LENGTH = 4;
  public static int HASH_LENGTH = 64;
  public static int LEVEL_3_LENGTH = HASH_LENGTH - LEVEL_2_LENGTH - LEVEL_2_LENGTH;

  private static void copy(File src, File dest) throws FileNotFoundException, IOException {
    FileOutputStream fos = null;
    FileInputStream fis = null;
    try {
      createParentDir(dest);
      fos = new FileOutputStream(dest);
      fis = new FileInputStream(src);
      fos.getChannel().transferFrom(fis.getChannel(), 0, Long.MAX_VALUE);
    } finally {
      if (fos != null)
        fos.close();
      if (fis != null)
        fis.close();
    }
  }

  private static File createParentDir(File file) throws IOException {
    File dir = file.getParentFile();
    if (!(dir.isDirectory() || dir.mkdirs()))
      throw new IOException("Could not create dir: " + dir.getAbsolutePath());
    else
      return file;
  }

  public static File getCacheFile(File baseDir, ArtifactHash hash, String filename) {
    assert (hash.value.length() == HASH_LENGTH); // we are slicing later and
                                                 // need at least 8 chars
    File artifactDir = new File(baseDir, ARTIFACT_CACHE_DIR);
    File level1 = new File(artifactDir, hash.value.subSequence(0, LEVEL_1_LENGTH).toString());
    File level2 = new File(level1, hash.value.subSequence(LEVEL_1_LENGTH, LEVEL_1_LENGTH + LEVEL_2_LENGTH).toString());
    File level3 = new File(level2, hash.value.subSequence(LEVEL_1_LENGTH + LEVEL_2_LENGTH, HASH_LENGTH).toString());
    return new File(level3, filename);
  }

  private static String hashFile(File file) throws IOException {
    FileInputStream fis = new FileInputStream(file);
    try {
      return Hasher.hash(fis);
    } finally {
      fis.close();
    }
  }

  /**
   * Get current cache file if exists. Creates a new one if there are other //
   * cached files with the same hash, but different file names
   * 
   * @throws IOException
   */
  public static File getOrCreateExistingCacheFile(File baseDir, ArtifactHash hash, String filename, boolean verify)
      throws IOException {
    File currentCacheFile = getCacheFile(baseDir, hash, filename);

    if (currentCacheFile.isFile()) {
      if (verify)
        assert (hashFile(currentCacheFile).equals(hash.value));
      return currentCacheFile;
    } else { // cache file is not there, but perhaps there is one with a
             // different name?
      File parentDir = currentCacheFile.getParentFile();
      if (parentDir != null && parentDir.listFiles() != null) {
        for (File file : parentDir.listFiles()) {
          if (!verify || hashFile(file).equals(hash.value)) {
            copy(file, currentCacheFile);
            return currentCacheFile;
          }
        }
        return null;
      } else {
        return null;
      }
    }
  }

  public static File cache(File baseDir, File srcFile, ArtifactHash expectedHash, String filename) throws AdeptCacheException,
      IOException {
    String actualHash = hashFile(srcFile);
    if (!actualHash.equals(expectedHash.value))
      throw new AdeptCacheException("Expected file: " + srcFile.getAbsolutePath() + " (with new name: " + filename
          + ") to have hash: " + expectedHash.value + " but it was: " + actualHash);
    else {
      File existingCacheFile = getOrCreateExistingCacheFile(baseDir, expectedHash, filename, true);
      if (existingCacheFile == null) {
        File newCacheFile = getCacheFile(baseDir, expectedHash, filename);
        copy(srcFile, newCacheFile);
        return newCacheFile;
      } else
        return existingCacheFile;
    }
  }
}
