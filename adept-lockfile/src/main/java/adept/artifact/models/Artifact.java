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
    this.locations.addAll(locations); //copying since Java's collections are mutable
  }
  
//  @Override
//  public boolean equals(Object other) {
//    if (other instanceof Artifact)
//  }
}
