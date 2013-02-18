# adept #

adept is a new dependency manager and aims to replace apache ivy.

It is not a build system like maven, gradle or sbt, but can be used inside of the build systems to load dependencies.

Have you ever been surprised by how long it takes to download dependencies using maven or ivy? I mean your adsl line should download the 10-100 mbs of data so fast, right?
Have you ever had to debug maven and ivy, having to hack your way to figure out why your classpath is wrong and why some dependencies are there or not.
Ever thought: I know what jars I want, give them to me!

adept is here to answer those thoughts!

## Scope ##

What adept does is to download your jars in a quick and reliable manner.
adept will also be linked to adept servers, which gives you among other things easy-to-find dependencies and browsable dependencies.

adept does not build your files or have plugins. It will be extensible through an api though.

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

- downloads a LOT faster than maven's dependency manager and ivy
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
2. next gen repository server: class search, source code search, better jar search (did you mean support), nicer ui
3. easy to use java api
4. a server side rest api
5. advanced command line tooling
6. gradle support
7. ant support
8. secured, enterprise version
9. gui command line tooling
10. maven support???

## Command line commands ##
To give you an idea of how adept will work, below are some examples of how to do stuff on the commandline (I think there will be modifications as we go along though). 

### Starting from scratch: ###
Initialize a new repo in current folder: ```adept init```

Get and merge data from remote repos: ```adept pull local http://adept.othercompany.com/ typesafe```
`typesafe` and `local` are alias in a known_repositories file in `.adept`

### Finding hashes in repo: ###
Outputs it self and all its deps:```adept describe play:play:2.1.0```
Imagine doing this with bash completion.

Output will look something like this: ```
play:play:2.1.0[scala-version=2.10.0]!124192313
com.typesafe.akka:akka-actor:2.1.0!1231251231
junit:junit:4.10[scope=test]!12425123412 ...```

### Publishing: ###
First add deps in current repo: ```adept describe $(echo my_deps) | adept add --scala-version=2.10.0 typesafe play:play:2.1.0 target/play.jar```
Notice: `--scala-version=2.10.0` adds the scala jars and some tags (in the []s)

This is shorthand for: ```adept describe  $(echo my_deps) scala:scala-library:2.10.0 | adept add typesafe play:play:2.1.0[scala-version=2.10.0] target/play.jar```

Push dependencies and jars to remote repo: `adept push typesafe`
This fails if you do not have the rights or are not up-to-date.

### Integrating with Ivy: ###
Adds the ivy deps to the current repo: ```adept ivy-add --settings=typesafe-settings.xml typesafe play:play:2.1.0```

