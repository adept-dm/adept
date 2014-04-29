package adept.artifact.models;

import java.util.Set;

public class ArtifactAttribute {
  public final String name;
  public final Set<String> values;

  public ArtifactAttribute(String name, Set<String> values) {
    this.name = name;
    this.values = values;
  }

  @Override
  public boolean equals(Object other) {
    if (other instanceof ArtifactAttribute && other != null) {
      ArtifactAttribute otherAA = (ArtifactAttribute) other;

      if (name.equals(otherAA.name)) {
        if (otherAA.values == null && values == null)
          return true;

        boolean allThis = true;
        for (String value : values) {
          allThis = allThis && otherAA.values.contains(value);
        }

        if (allThis) {
          for (String otherValue : otherAA.values) {
            if (!values.contains(otherValue))
              return false;
          }
          return true;
        } else
          return false;
      } else
        return false;
    } else
      return false;
  }

  @Override
  public int hashCode() {
    return this.getClass().getName().hashCode() + name.hashCode() + values.hashCode();
  }

}
