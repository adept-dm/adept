package adept.lockfile;

import java.util.Set;

public class LockfileRequirement { //TODO: equals, hashCode
  public final Id id;
  public final Set<Constraint> constraints;
  public final Set<Id> exclusions;

  public LockfileRequirement(Id id, Set<Constraint> constraints, Set<Id> exclusions) {
    this.id = id;
    this.constraints = constraints;
    this.exclusions = exclusions;
  }
  
}
