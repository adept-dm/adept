package adept.lockfile;

import java.util.Set;

import adept.artifact.models.Artifact;
import adept.artifact.models.ArtifactAttribute;
import adept.artifact.models.ArtifactHash;
import adept.artifact.models.ArtifactLocation;

/**
 * Limited to this package only, use Lockfile factory methods to create lockfile
 * instead, or LockfileGenerator
 */
class LockfileArtifact { // TODO: equals, hashCode
  public final ArtifactHash hash;
  public final Integer size; // TODO: we have to make this a Long, because we
                             // might hit some strange issues but json smart
                             // does not support it
  public final Set<ArtifactLocation> locations; // because locations are public
                                                // we might have concurrency
                                                // issues, but the likelyhood is
                                                // rather small
  public final Set<ArtifactAttribute> attributes;
  /* can be null */
  public final String filename;

  LockfileArtifact(ArtifactHash hash, Integer size,
      Set<ArtifactLocation> locations, Set<ArtifactAttribute> attributes,
      String filename) {
    this.hash = hash;
    this.size = size;
    this.locations = locations;
    this.attributes = attributes;
    this.filename = filename;
  }

  public Artifact getArtifact() {
    return new Artifact(hash, size.longValue(), locations);
  }

  @Override
  public boolean equals(Object other) {
    if (other != null && other instanceof LockfileArtifact) {
      LockfileArtifact otherArtifact = (LockfileArtifact) other;
      return hash.equals(otherArtifact.hash) && size.equals(otherArtifact.size)
          && locations.equals(otherArtifact.locations)
          && attributes.equals(otherArtifact.attributes)
          && filename.equals(otherArtifact.filename);
    } else
      return false;
  }

  public int hashCode() {
    return this.getClass().getName().hashCode() + hash.hashCode()
        + size.hashCode() + locations.hashCode() + attributes.hashCode()
        + filename.hashCode();
  }
}
