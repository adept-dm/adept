package adept.artifact.models;

public class ArtifactHash {
  public ArtifactHash(String value) {
    this.value = value;
  }
  final public String value;
  
  @Override
  public boolean equals(Object other) {
    if (other != null && other instanceof ArtifactHash) {
      return this.value.equals(((ArtifactHash) other).value);
    } else return false;
  }
  
  @Override
  public int hashCode() {
    return this.getClass().getName().hashCode() + value.hashCode();
  }

  @Override
  public String toString() {
    return value;
  }
}
