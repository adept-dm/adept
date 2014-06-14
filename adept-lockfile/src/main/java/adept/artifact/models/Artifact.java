package adept.artifact.models;

import com.fasterxml.jackson.core.JsonGenerator;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class Artifact implements JsonSerializable {
  final public ArtifactHash hash;
  final public Long size;
  final public Set<ArtifactLocation> locations;

  public Artifact(ArtifactHash hash, Long size, Set<ArtifactLocation> locations) {
    this.hash = hash;
    this.size = size;
    this.locations = new HashSet<>();
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

  @Override
  public void writeJson(JsonGenerator generator) throws IOException {
    generator.writeStringField("hash", hash.value);
    generator.writeNumberField("size", size);
    generator.writeArrayFieldStart("locations");
    for (ArtifactLocation location: locations) {
      generator.writeString(location.value);
    }
    generator.writeEndArray();
  }
}
