package adept.artifact.models;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class ArtifactAttribute implements JsonSerializable {
  public final String name;
  public final Set<String> values;

  public ArtifactAttribute(String name, Set<String> values) {
    this.name = name;
    this.values = values;
  }

  public static ArtifactAttribute fromJson(JsonParser parser) throws IOException {
    String name = null;
    Set<String> values = null;
    while (parser.nextToken() != JsonToken.END_OBJECT) {
      String fieldName = parser.getCurrentName();
      // Read value, or START_OBJECT/START_ARRAY
      parser.nextToken();
      switch (fieldName) {
        case "name":
          name = parser.getValueAsString();
          break;
        case "values":
          values = new HashSet<>();
          while (parser.nextToken() != JsonToken.END_ARRAY) {
            values.add(parser.getValueAsString());
          }
          break;
      }
    }

    return new ArtifactAttribute(name, values);
  }

  public void writeJson(JsonGenerator generator) throws java.io.IOException {
    generator.writeStringField("name", name);
    generator.writeArrayFieldStart("values");
    for (String value: values) {
      generator.writeString(value);
    }
    generator.writeEndArray();
  }

  @Override
  public boolean equals(Object other) {
    if (other instanceof ArtifactAttribute) {
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
