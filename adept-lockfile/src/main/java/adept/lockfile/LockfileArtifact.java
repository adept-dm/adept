package adept.lockfile;

import java.util.Set;

import adept.artifact.models.Artifact;
import adept.artifact.models.ArtifactAttribute;
import adept.artifact.models.ArtifactHash;
import adept.artifact.models.ArtifactLocation;

/**
 * Limited to this package only, use Lockfile factory methods to create lockfile instead, or LockfileGenerator 
 */
class LockfileArtifact { //TODO: equals, hashCode
  public final ArtifactHash hash;
  public final Integer size;  //TODO: we have to make this a Long, because we might hit some strange issues
  public final Set<ArtifactLocation> locations; //because locations are public we might have concurrency issues, but the likelyhood is rather small
  public final Set<ArtifactAttribute> attributes;
  public final String filename;
  
  LockfileArtifact(ArtifactHash hash, Integer size, Set<ArtifactLocation> locations, Set<ArtifactAttribute> attributes, String filename) {
    this.hash = hash;
    this.size = size;
    this.locations = locations;
    this.attributes = attributes;
    this.filename = filename;
  }

  public Artifact getArtifact() {
    return new Artifact(hash, size.longValue(), locations);
  }
}
