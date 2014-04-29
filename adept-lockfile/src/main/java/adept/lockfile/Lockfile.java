package adept.lockfile;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import adept.artifact.AdeptCacheException;
import adept.artifact.ArtifactCache;
import adept.artifact.ArtifactDownloadResult;
import adept.artifact.ArtifactDownloader;
import adept.artifact.models.ArtifactHash;
import adept.artifact.models.ArtifactLocation;
import adept.logging.JavaLogger;
import adept.progress.ProgressMonitor;
import net.minidev.json.JSONArray;
import net.minidev.json.JSONObject;
import net.minidev.json.JSONValue;
import net.minidev.json.parser.ParseException;

public class Lockfile {

  private final Set<LockfileRequirement> requirements;
  private final Set<LockfileArtifact> artifacts;

  protected File getTmpDir() {
    String tmpDir = System.getProperty("java.io.tmpdir");
    if (tmpDir == null) {
      throw new RuntimeException("Could not find a tmp directory because java.io.tmpdir is not set");
    } else {
      return new File(tmpDir);
    }
  }

  /**
   * Lockfiles get's created by factory read methods or by LockfileGenerator,
   * only package visibility
   */
  Lockfile(Set<LockfileRequirement> requirements, Set<LockfileArtifact> artifacts) {
    this.requirements = requirements;
    this.artifacts = artifacts;
  }

  private static Set<String> deserializeStringSet(JSONArray jsonStrings) {
    Set<String> values = new HashSet<String>(jsonStrings.size());
    for (int i = 0; i < jsonStrings.size(); i++) {
      values.add((String) jsonStrings.get(i));
    }
    return values;
  }

  private static Set<Id> deserializeIdSet(JSONArray jsonStrings) {
    Set<Id> values = new HashSet<Id>(jsonStrings.size());
    for (int i = 0; i < jsonStrings.size(); i++) {
      values.add(new Id((String) jsonStrings.get(i)));
    }
    return values;
  }

  private static Set<RepositoryLocation> deserializeRepositoryLocationSet(JSONArray jsonStrings) {
    Set<RepositoryLocation> values = new HashSet<RepositoryLocation>(jsonStrings.size());
    for (int i = 0; i < jsonStrings.size(); i++) {
      values.add(new RepositoryLocation((String) jsonStrings.get(i)));
    }
    return values;
  }

  private static Set<ArtifactLocation> deserializeArtifactLocationSet(JSONArray jsonStrings) {
    Set<ArtifactLocation> values = new HashSet<ArtifactLocation>(jsonStrings.size());
    for (int i = 0; i < jsonStrings.size(); i++) {
      values.add(new ArtifactLocation((String) jsonStrings.get(i)));
    }
    return values;
  }

  private static Set<Constraint> deserializeConstraints(JSONObject jsonConstraints) {
    Set<Constraint> constraints = new HashSet<Constraint>(jsonConstraints.size());
    for (String name : jsonConstraints.keySet()) {
      JSONArray jsonConstraintValues = (JSONArray) jsonConstraints.get(name);
      Set<String> values = deserializeStringSet(jsonConstraintValues);
      constraints.add(new Constraint(name, values));
    }
    return constraints;
  }

  private static Set<ArtifactAttribute> deserializeArtifactAttributes(JSONObject jsonArtifactAttributes) {
    Set<ArtifactAttribute> attributes = new HashSet<ArtifactAttribute>(jsonArtifactAttributes.size());
    for (String name : jsonArtifactAttributes.keySet()) {
      JSONArray jsonConstraintValues = (JSONArray) jsonArtifactAttributes.get(name);
      Set<String> values = deserializeStringSet(jsonConstraintValues);
      attributes.add(new ArtifactAttribute(name, values));
    }
    return attributes;
  }

  private static Lockfile deserialize(JSONObject jsonLockfile) {
    JSONArray jsonRequirements = (JSONArray) jsonLockfile.get("requirements");
    JSONArray jsonArtifacts = (JSONArray) jsonLockfile.get("artifacts");

    Set<LockfileRequirement> requirements = new HashSet<LockfileRequirement>(jsonRequirements.size());
    Set<LockfileArtifact> artifacts = new HashSet<LockfileArtifact>(jsonArtifacts.size());

    for (int i = 0; i < jsonRequirements.size(); i++) {
      JSONObject jsonRequirement = (JSONObject) jsonRequirements.get(i);
      Id id = new Id((String) jsonRequirement.get("id"));
      Set<Constraint> constraints = deserializeConstraints((JSONObject) jsonRequirement.get("constraints"));
      Set<Id> exclusions = deserializeIdSet((JSONArray) jsonRequirement.get("exclusions"));
      Set<RepositoryLocation> locations = deserializeRepositoryLocationSet((JSONArray) jsonRequirement.get("locations"));
      RepositoryName repository = new RepositoryName((String) jsonRequirement.get("repository"));
      Commit commit = new Commit((String) jsonRequirement.get("commit"));
      VariantHash hash = new VariantHash((String) jsonRequirement.get("variant"));
      LockfileRequirement requirement = new LockfileRequirement(id, constraints, exclusions, repository, locations,
          commit, hash);
      requirements.add(requirement);
    }

    for (int i = 0; i < jsonArtifacts.size(); i++) {
      JSONObject jsonArtifact = (JSONObject) jsonArtifacts.get(i);
      ArtifactHash hash = new ArtifactHash((String) jsonArtifact.get("hash"));
      Integer size = (Integer) jsonArtifact.get("size");
      Set<ArtifactLocation> locations = deserializeArtifactLocationSet((JSONArray) jsonArtifact.get("locations"));
      Set<ArtifactAttribute> attributes = deserializeArtifactAttributes((JSONObject) jsonArtifact.get("attributes"));
      String filename = (String) jsonArtifact.get("filename");
      LockfileArtifact artifact = new LockfileArtifact(hash, size, locations, attributes, filename);
      artifacts.add(artifact);
    }
    return new Lockfile(requirements, artifacts);
  }

  public static Lockfile read(String data) throws LockfileParseException {
    try {
      return deserialize((JSONObject) JSONValue.parseStrict(data));
    } catch (ParseException e) {
      throw new LockfileParseException(e);
    }
  }

  public static Lockfile read(Reader data) throws LockfileParseException, IOException {
    try {
      return deserialize((JSONObject) JSONValue.parseStrict(data));
    } catch (ParseException e) {
      throw new LockfileParseException(e);
    }
  }

  public static Lockfile read(File file) throws LockfileParseException, FileNotFoundException, IOException {
    FileReader reader = null;
    try {
      reader = new FileReader(file);
      return read(reader);
    } finally {
      if (reader != null)
        reader.close();
    }
  }

  protected int THREAD_POOL_SIZE = 20;

  public Set<ArtifactDownloadResult> download(File baseDir, Long timeout, TimeUnit timeoutUnit, int maxRetries, JavaLogger logger, ProgressMonitor progress)
      throws InterruptedException, ExecutionException, AdeptCacheException, IOException {
    ExecutorService executorService = Executors.newFixedThreadPool(THREAD_POOL_SIZE);
    Set<Future<ArtifactDownloadResult>> futures = new HashSet<Future<ArtifactDownloadResult>>(artifacts.size());
    
    int allSizes = 0;
    for (LockfileArtifact lockfileArtifact : artifacts) {
      allSizes += lockfileArtifact.size;
    }
    progress.beginTask("Downloading artifacts", allSizes);

    for (LockfileArtifact lockfileArtifact : artifacts) {
      File tmpFile = File.createTempFile("adept-", lockfileArtifact.filename, getTmpDir());
      futures.add(executorService.submit(new ArtifactDownloader(baseDir, lockfileArtifact.getArtifact(), lockfileArtifact.filename, tmpFile, maxRetries, logger, progress)));
    }
    executorService.shutdown();
    executorService.awaitTermination(timeout, timeoutUnit);

    Set<ArtifactDownloadResult> results = new HashSet<ArtifactDownloadResult>(artifacts.size());  
    
    for (Future<ArtifactDownloadResult> future : futures) {
      ArtifactDownloadResult result = future.get();
      if (result.isSuccess()) {
        results.add(result);
      } else if (result.isFailed()) {
        final String causeString;
        if (result.exception.getCause() == null) causeString = ""; else causeString = " "+result.exception.getCause()+".";
        
        String locationsString = "";
        for (ArtifactLocation location: result.artifact.locations) {
          locationsString += location.value + ",";
        }
        locationsString = locationsString.substring(0, locationsString.length() - 1); //cut last , off
        
        logger.error("Failed to get artifact with filename: '" + result.filename + "' from: " + locationsString  + "." + causeString + " Hash: " + result.artifact.hash.value);
        results.add(result);
      } else {
        assert(false); //Illegal state: got a download result that is neither failed nor successful!?
      }
    }
    progress.endTask();
    return results;
  }
}
