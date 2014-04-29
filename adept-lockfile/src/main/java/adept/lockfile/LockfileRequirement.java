package adept.lockfile;

import java.util.Set;

class LockfileRequirement { //TODO: equals, hashCode
  final Id id;
  final Set<Constraint> constraints;
  final Set<Id> exclusions;
  final RepositoryName repository;
  final Set<RepositoryLocation> locations;
  final Commit commit;
  final VariantHash hash;
  
  LockfileRequirement(Id id, Set<Constraint> constraints, Set<Id> exclusions, RepositoryName repository, Set<RepositoryLocation> locations,
      Commit commit, VariantHash hash) {
    this.id = id;
    this.constraints = constraints;
    this.exclusions = exclusions;
    this.repository = repository;
    this.locations = locations;
    this.commit = commit;
    this.hash = hash;
  }
  
}
