package adept.artifact.models;

import com.fasterxml.jackson.core.JsonGenerator;

import java.io.IOException;

public interface JsonSerializable {
    void writeJson(JsonGenerator generator) throws IOException;
}
