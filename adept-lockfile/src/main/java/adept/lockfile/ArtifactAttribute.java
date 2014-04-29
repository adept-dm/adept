package adept.lockfile;

import java.util.Set;

class ArtifactAttribute {
  final String name;
  final Set<String> values;
  
  ArtifactAttribute(String name, Set<String> values) {
    this.name = name;
    this.values = values;
  }
}
