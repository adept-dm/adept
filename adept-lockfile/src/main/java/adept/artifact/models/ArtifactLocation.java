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
}
