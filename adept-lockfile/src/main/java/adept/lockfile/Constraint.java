package adept.lockfile;

import java.util.Set;

class Constraint {
  final String name;
  final Set<String> values;
  
  Constraint(String name, Set<String> values) {
    this.name = name;
    this.values = values;
  }
}
