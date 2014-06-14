package adept.lockfile;

import adept.artifact.models.JsonSerializable;
import com.fasterxml.jackson.core.JsonGenerator;

import java.io.IOException;
import java.util.Set;

public class Constraint implements JsonSerializable {
  public final String name;
  public final Set<String> values;
  
  public Constraint(String name, Set<String> values) {
    this.name = name;
    this.values = values;
  }

  public void writeJson(JsonGenerator generator) throws IOException {
    generator.writeStringField("name", name);
    generator.writeArrayFieldStart("values");
    for (String value: values) {
      generator.writeString(value);
    }
    generator.writeEndArray();
  }
}
