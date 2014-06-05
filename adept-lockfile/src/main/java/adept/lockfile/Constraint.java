package adept.lockfile;

import java.util.Set;

public class Constraint {
  public final String name;
  public final Set<String> values;
  
  public Constraint(String name, Set<String> values) {
    this.name = name;
    this.values = values;
  }
}
