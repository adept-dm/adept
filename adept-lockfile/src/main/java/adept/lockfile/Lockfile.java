package adept.lockfile;

import adept.artifact.AdeptCacheException;
import adept.artifact.ArtifactCache;
import adept.artifact.ArtifactDownloadResult;
import adept.artifact.ArtifactDownloader;
import adept.artifact.models.ArtifactAttribute;
import adept.artifact.models.ArtifactHash;
import adept.artifact.models.ArtifactLocation;
import adept.logging.JavaLogger;
import adept.progress.ProgressMonitor;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;

import java.io.*;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.*;

public class Lockfile {
  final Set<LockfileRequirement> requirements;
  final Set<LockfileContext> context;
  final Set<LockfileArtifact> artifacts;

  protected File getTmpDir() {
    String tmpDir = System.getProperty("java.io.tmpdir");
    if (tmpDir == null) {
      throw new RuntimeException("Could not find a tmp directory because java.io.tmpdir is not set");
    } else {
      return new File(tmpDir);
    }
  }

  /**
   * Lockfiles gets created by factory read methods or by LockfileManager (in adept-core), only
   * package visibility
   */
  public Lockfile(Set<LockfileRequirement> requirements, Set<LockfileContext> context,
                  Set<LockfileArtifact> artifacts) {
    // we are in control of the Sets (only we can instantiate) here so even if
    // they are mutable it is OK (yeah! :)
    if (requirements == null) {
      throw new IllegalArgumentException("requirements is null");
    }
    if (context == null) {
      throw new IllegalArgumentException("context is null");
    }
    if (artifacts == null) {
      throw new IllegalArgumentException("artifacts is null");
    }
    this.requirements = requirements;
    this.context = context;
    this.artifacts = artifacts;
  }

  // Getters: TODO: make copies of requirements to ensure immutability? seems a
  // bit too strict though
  public Set<LockfileRequirement> getRequirements() {
    return requirements;
  }

  public Set<LockfileArtifact> getArtifacts() {
    return artifacts;
  }

  public Set<LockfileContext> getContext() {
    return context;
  }

  public static Constraint parseConstraint(JsonParser parser) throws IOException {
    String name = null;
    Set<String> values = null;
    while (parser.nextToken() != JsonToken.END_OBJECT) {
      String fieldName = parser.getCurrentName();
      // Get value or array start token
      parser.nextToken();
      if (fieldName.equals("name")) {
        name = parser.getValueAsString();
      } else if (fieldName.equals("values")) {
        values = new HashSet<String>();
        assert (parser.getCurrentToken() == JsonToken.START_ARRAY);
        while (parser.nextToken() != JsonToken.END_ARRAY) {
          values.add(parser.getValueAsString());
        }
      }
    }

    return new Constraint(name, values);
  }

  public static LockfileRequirement parseRequirement(JsonParser parser) throws IOException {
    assert (parser.getCurrentToken() == JsonToken.START_OBJECT);
    String id = null;
    Set<Constraint> constraints = null;
    Set<Id> exclusions = null;
    while (parser.nextToken() != JsonToken.END_OBJECT) {
      assert (parser.getCurrentToken() == JsonToken.FIELD_NAME);
      String fieldName = parser.getCurrentName();
      // Get value or array start token
      parser.nextToken();
      if (fieldName.equals("id")) {
        id = parser.getValueAsString();
      } else if (fieldName.equals("constraints")) {
        constraints = new HashSet<Constraint>();
        assert (parser.getCurrentToken() == JsonToken.START_ARRAY);
        while (parser.nextToken() != JsonToken.END_ARRAY) {
          constraints.add(parseConstraint(parser));
        }
      } else if (fieldName.equals("exclusions")) {
          exclusions = new HashSet<Id>();
          assert (parser.getCurrentToken() == JsonToken.START_ARRAY);
          while (parser.nextToken() != JsonToken.END_ARRAY) {
            exclusions.add(new Id(parser.getValueAsString()));
          }
      }
    }

    return new LockfileRequirement(new Id(id), constraints, exclusions);
  }

  private static LockfileContext parseContext(JsonParser parser) throws IOException {
    String info = null;
    Id id = null;
    RepositoryName repository = null;
    Set<RepositoryLocation> locations = null;
    Commit commit = null;
    VariantHash hash = null;
    assert (parser.getCurrentToken() == JsonToken.START_OBJECT);
    while (parser.nextToken() != JsonToken.END_OBJECT) {
      assert (parser.getCurrentToken() == JsonToken.FIELD_NAME);
      String fieldName = parser.getCurrentName();
      // Get value or array start token
      parser.nextToken();
      if (fieldName.equals("info")) {
        info = parser.getValueAsString();
      } else if (fieldName.equals("id")) {
        id = new Id(parser.getValueAsString());
      } else if (fieldName.equals("repository")) {
        repository = new RepositoryName(parser.getValueAsString());
      } else if (fieldName.equals("locations")) {
        locations = new HashSet<RepositoryLocation>();
        assert (parser.getCurrentToken() == JsonToken.START_ARRAY);
        while (parser.nextToken() != JsonToken.END_ARRAY) {
          locations.add(new RepositoryLocation(parser.getValueAsString()));
        }
      } else if (fieldName.equals("commit")) {
        commit = new Commit(parser.getValueAsString());
      } else if (fieldName.equals("hash")) {
          hash = new VariantHash(parser.getValueAsString());
      }
    }

    return new LockfileContext(info, id, repository, locations, commit,
        hash);
  }

  private static ArtifactAttribute parseAttribute(JsonParser parser) throws IOException {
    String name = null;
    Set<String> values = null;
    assert (parser.getCurrentToken() == JsonToken.START_OBJECT);
    while (parser.nextToken() != JsonToken.END_OBJECT) {
      assert (parser.getCurrentToken() == JsonToken.FIELD_NAME);
      String fieldName = parser.getCurrentName();
      // Get value or array start token
      parser.nextToken();
      if (fieldName.equals("name")) {
        name = parser.getValueAsString();
      } else if (fieldName.equals("values")) {
          values = new HashSet<String>();
          assert (parser.getCurrentToken() == JsonToken.START_ARRAY);
          while (parser.nextToken() != JsonToken.END_ARRAY) {
            values.add(parser.getValueAsString());
          }
      }
    }
    return new ArtifactAttribute(name, values);
  }

  private static LockfileArtifact parseArtifact(JsonParser parser) throws IOException {
    ArtifactHash hash = null;
    Integer size = null;
    Set<ArtifactLocation> locations = null;
    Set<ArtifactAttribute> attributes = null;
    String filename = null;
    assert (parser.getCurrentToken() == JsonToken.START_OBJECT);
    while (parser.nextToken() != JsonToken.END_OBJECT) {
      assert (parser.getCurrentToken() == JsonToken.FIELD_NAME);
      String fieldName = parser.getCurrentName();
      // Get value or array start token
      parser.nextToken();
      if (fieldName.equals("hash")) {
        hash = new ArtifactHash(parser.getValueAsString());
      } else if (fieldName.equals("size")) {
        size = parser.getValueAsInt();
      } else if (fieldName.equals("locations")) {
        locations = new HashSet<ArtifactLocation>();
        assert (parser.getCurrentToken() == JsonToken.START_ARRAY);
        while (parser.nextToken() != JsonToken.END_ARRAY) {
          locations.add(new ArtifactLocation(parser.getValueAsString()));
        }
      } else if (fieldName.equals("attributes")) {
        attributes = new HashSet<ArtifactAttribute>();
        assert (parser.getCurrentToken() == JsonToken.START_ARRAY);
        while (parser.nextToken() != JsonToken.END_ARRAY) {
          attributes.add(parseAttribute(parser));
        }
      } else if (fieldName.equals("filename")) {
          filename = parser.getValueAsString();
          break;
      }
    }
    return new LockfileArtifact(hash, size, locations, attributes, filename);
  }

  public static Lockfile read(Reader reader) throws LockfileParseException, IOException {
    Set<LockfileRequirement> requirements = null;
    Set<LockfileContext> contexts = null;
    Set<LockfileArtifact> artifacts = null;
    char[] buffer = new char[1024];
    int n;
    Writer writer = new StringWriter();
    while ((n = reader.read(buffer)) != -1) {
      writer.write(buffer, 0, n);
    }
    String json = writer.toString();
    JsonParser parser = new JsonFactory().createParser(json);
    try {
      // Get START_OBJECT
      parser.nextToken();
      assert parser.getCurrentToken() == JsonToken.START_OBJECT;
      // Read field name or END_OBJECT
      while (parser.nextToken() != JsonToken.END_OBJECT) {
        assert (parser.getCurrentToken() == JsonToken.FIELD_NAME);
        String fieldName = parser.getCurrentName();
        // Read value, or START_OBJECT/START_ARRAY
        parser.nextToken();
        if (fieldName.equals("requirements")) {
          requirements = new HashSet<LockfileRequirement>();
          while (parser.nextToken() != JsonToken.END_ARRAY) {
            requirements.add(parseRequirement(parser));
          }
        } else if (fieldName.equals("context")) {
          contexts = new HashSet<LockfileContext>();
          while (parser.nextToken() != JsonToken.END_ARRAY) {
            contexts.add(parseContext(parser));
          }
        } else if (fieldName.equals("artifacts")) {
            artifacts = new HashSet<LockfileArtifact>();
            while (parser.nextToken() != JsonToken.END_ARRAY) {
              artifacts.add(parseArtifact(parser));
            }
        }
      }
    }
    finally {
      parser.close();
    }

    if (requirements == null) {
      requirements = new HashSet<LockfileRequirement>();
    }
    if (contexts == null) {
      contexts = new HashSet<LockfileContext>();
    }
    if (artifacts == null) {
      artifacts = new HashSet<LockfileArtifact>();
    }
    return new Lockfile(requirements, contexts, artifacts);
  }

  public static Lockfile read(File file) throws LockfileParseException, IOException {
    FileReader reader = null;
    try {
      if (!file.isFile()) {
        return new Lockfile(new HashSet<LockfileRequirement>(), new HashSet<LockfileContext>(),
            new HashSet<LockfileArtifact>());
      } else {
        reader = new FileReader(file);
        try {
          return read(reader);
        }
        catch (LockfileParseException err) {
          throw new LockfileParseException(file, err);
        }
      }
    } finally {
      if (reader != null) {
        reader.close();
      }
    }
  }

  protected int THREAD_POOL_SIZE = 30;

  public Set<ArtifactDownloadResult> download(File baseDir, Long timeout, TimeUnit timeoutUnit,
                                              int maxRetries, JavaLogger logger, ProgressMonitor progress)
  throws InterruptedException, ExecutionException,
      AdeptCacheException, IOException {
    ExecutorService executorService = Executors.newFixedThreadPool(THREAD_POOL_SIZE);
    Set<ArtifactDownloadResult> results = new HashSet<ArtifactDownloadResult>(this.artifacts.size());

    Set<LockfileArtifact> nonLocalArtifacts = new HashSet<LockfileArtifact>();
    for (LockfileArtifact artifact : this.artifacts) {
      File currentCachedFile = ArtifactCache.getOrCreateExistingCacheFile(baseDir, artifact.hash,
          artifact.filename, true);
      if (currentCachedFile == null || !currentCachedFile.isFile()) {
        nonLocalArtifacts.add(artifact);
      } else {
        ArtifactDownloadResult result = new ArtifactDownloadResult(artifact.getArtifact(),
            currentCachedFile.getName());
        result.setCachedFile(currentCachedFile);
        results.add(result);
      }
    }

    Set<Future<ArtifactDownloadResult>> futures = new HashSet<Future<ArtifactDownloadResult>>(
        nonLocalArtifacts.size());

    int allSizes = 0;
    for (LockfileArtifact lockfileArtifact : nonLocalArtifacts) {
      allSizes += lockfileArtifact.size / 1024;
    }
    boolean displayProgress = !nonLocalArtifacts.isEmpty();
    if (displayProgress)
      progress.beginTask("Downloading (kB)", allSizes);

    for (LockfileArtifact lockfileArtifact : nonLocalArtifacts) {
      File tmpFile = File.createTempFile("adept-", lockfileArtifact.filename, getTmpDir());
      //Initiate downloads:
      futures.add(executorService.submit(new ArtifactDownloader(baseDir, lockfileArtifact.getArtifact(),
          lockfileArtifact.filename, tmpFile, maxRetries, logger, progress)));
    }
    executorService.shutdown();
    executorService.awaitTermination(timeout, timeoutUnit);

    for (Future<ArtifactDownloadResult> future : futures) {
      ArtifactDownloadResult result = future.get();
      if (result.isSuccess()) {
        results.add(result);
      } else if (result.isFailed()) {
        final String causeString;
        if (result.exception.getCause() == null)
          causeString = "";
        else
          causeString = " " + result.exception.getCause() + ".";

        String locationsString = "";
        for (ArtifactLocation location : result.artifact.locations) {
          locationsString += location.value + ",";
        }
        locationsString = locationsString.substring(0, locationsString.length() - 1); // cut last ',' off

        logger.error("Failed to get artifact with filename: '" + result.filename + "' from: " +
            locationsString + "."
            + causeString + " Hash: " + result.artifact.hash.value);
        results.add(result);
      } else {
        // Illegal state: got a download result that is neither failed nor successful!?
        throw new AssertionError("Reached illegal state");
      }
    }
    if (displayProgress)
      progress.endTask();

    return results;
  }
}
