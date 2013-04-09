# adept #

adept helps you find, declare, and download dependencies.

adept retrieves dependency metadata all at once and identifies artifacts by hashing their content.
This makes adept well-suited for discovering dependencies in addition to being predictable and fast.

adept is currently in the proposal and discussion stage and needs your involvement.
Please see the wiki for the current proposals.

Discuss existing ideas and propose new ones on the development mailing list:

  http://groups.google.com/group/adept-dev/

Although hosted on the sbt organization, adept is intended to be build tool agnostic.


## Scope ##

What adept does is to download your jars in a quick and reliable manner.
adept will also be linked to adept servers, which gives you among other things easy-to-find dependencies and browsable dependencies.

adept does not build your project or have plugins. It will be extensible through an api though.

## Workflow ##

1. pull an adept repository containing all the metainformation about the dependencies in the repository
2. find the jar files you want based in the newly created repository
3. list all the dependencies you need
4. download the jar files in bulk

## Not being lazily transitive ##

A lazy transitive dependency means that the dependencies of a dependency are calculated when the parent dependency is resolved.
With ivy, if I declare that I want the dependency for apache commons for example, ivy  will look into it's resolvers and figure out what dependencies it has and start downloading those. It will keep doing this  until all dependencies are resolved.

adept does not do that. adept has a repository containing the entire graph of the dependencies stored locally.

The pros are as follows:
- there is never a doubt of where a dependency comes from
- it makes it possible to download more quickly since we can bulk download
- offline browsing and discovery

The cons are:
- you will have more information (the entire dependency graph) in your project than earlier

## Core features ##

- downloads faster than maven's dependency manager and ivy
- never-fail caching, using hashes
- easy to get started like normal, but also easy to debug
- 100% predicable builds
- highly fault tolerant download of jar files using torrents
- easy-to-use scala api
- sbt support
- works with version control systems (i.e. text based, human readable)
- command line tool to bump versions, apply strategies and filters on adept files

## Roadmap ##

1. stabilize core features
2. advanced command line tooling
3. gradle support
4. ant support
5. easy to use java api
6. secured, enterprise version
7. gui command line tooling
8. maven support???

## Command line commands ##
To give you an idea of how adept will work, below are some examples of how to do stuff on the commandline (I think there will be modifications as we go along though). 
