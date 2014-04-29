package adept.lockfile;

import java.util.Set;

import adept.artifact.models.Artifact;
import adept.artifact.models.ArtifactHash;
import adept.artifact.models.ArtifactLocation;

/**
 * Limited to this package only, use Lockfile factory methods to create lockfile instead, or LockfileGenerator 
 */
class LockfileArtifact { //TODO: equals, hashCode
  final ArtifactHash hash;
  final Integer size;  //TODO: we have to make this a Long, because we might hit some strange issues
  final Set<ArtifactLocation> locations;
  final Set<ArtifactAttribute> attributes;
  final String filename;
  
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
