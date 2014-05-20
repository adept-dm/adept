package adept.artifact.models;

import java.util.HashSet;
import java.util.Set;

public class Artifact {
  final public ArtifactHash hash;
  final public Long size;
  final public Set<ArtifactLocation> locations;

  public Artifact(ArtifactHash hash, Long size, Set<ArtifactLocation> locations) {
    this.hash = hash;
    this.size = size;
    this.locations = new HashSet<ArtifactLocation>();
    this.locations.addAll(locations); // copying since Java's collections are
                                      // mutable
  }

  @Override
  public String toString() {
    String locationsString = "";
    for (ArtifactLocation location : locations) {
      locationsString += location;
    }
    return hash + "(" + size + ")::" + locationsString;
  }

  @Override
  public boolean equals(Object other) {
    if (other instanceof Artifact) {
      Artifact otherArtifact = (Artifact) other;
      return otherArtifact.hash.equals(otherArtifact.hash)  &&
          otherArtifact.size.equals(otherArtifact.size) &&
          otherArtifact.locations.equals(otherArtifact.locations);
    } else return false;
  }

  @Override
  public int hashCode() {
    return this.getClass().getName().hashCode() + hash.value.hashCode() + locations.hashCode() + size.hashCode();
  }
}
