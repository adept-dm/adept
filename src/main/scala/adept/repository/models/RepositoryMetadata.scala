package adept.repository.models

/**
 * Information that is needed load.
 * 
 * `info` can be used to make it easier to know which version this commit is pointing to.
 */
case class RepositoryMetadata(name: String, uris: Set[String], commit: Commit, info: String)
