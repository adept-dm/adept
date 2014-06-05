package adept.lockfile;

public class VariantHash {
  VariantHash(String value) {
    this.value = value;
  }

  final public String value;

  @Override
  public int hashCode() {
    return getClass().hashCode() + value.hashCode();
  }

  @Override
  public boolean equals(Object other) {
    if (other != null && other instanceof Id) {
      Id otherId = (Id) other;
      return value.equals(otherId.value);
    } else {
      return false;
    }
  }
}
