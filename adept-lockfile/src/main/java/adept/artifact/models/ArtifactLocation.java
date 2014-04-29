package adept.artifact.models;

import java.net.MalformedURLException;
import java.net.URL;

public class ArtifactLocation {
  public ArtifactLocation(String value) {
    this.value = value;
  }

  final public String value;

  public URL getURL() throws MalformedURLException {
    return new URL(value);
  }

  @Override
  public boolean equals(Object other) {
    if (other instanceof ArtifactLocation && other != null) {
      return this.value.equals(((ArtifactLocation) other).value);
    } else
      return false;
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
