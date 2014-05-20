package adept.lockfile;

import java.util.Set;

//TODO: rename to LockfileContext
class LockfileVariant { //TODO: equals, hashCode
  public final String info;
  public final Id id;
  public final RepositoryName repository;
  public final Set<RepositoryLocation> locations;
  public final Commit commit; //can be null
  public final VariantHash hash;

  LockfileVariant(String info, Id id, RepositoryName repository, Set<RepositoryLocation> locations, Commit commit, VariantHash hash) {
    this.info = info;
    this.id = id;
    this.repository = repository;
    this.locations = locations;
    this.commit = commit;
    this.hash = hash;
  }
}
