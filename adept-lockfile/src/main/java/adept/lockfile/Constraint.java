package adept.lockfile;

import java.util.Set;

class Constraint {
  public final String name;
  public final Set<String> values;
  
  Constraint(String name, Set<String> values) {
    this.name = name;
    this.values = values;
  }
}
