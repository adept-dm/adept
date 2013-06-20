# adept #

adept helps you find, declare, and download dependencies.

adept retrieves dependency metadata all at once and identifies artifacts by hashing their content.
This makes adept well-suited for discovering dependencies in addition to being predictable and fast.

adept is currently in the proposal and discussion stage and needs your involvement.
Please see the wiki for the current proposals.

Discuss existing ideas and propose new ones on the development mailing list:

  http://groups.google.com/group/adept-dev/

Although hosted on the sbt organization, adept is intended to be build tool agnostic.

## Vision ##
Adept should always strive to a dependency management system which is:
1. *fast*
2. *predicable*
3. *debuggable*

## Scope ##

What adept does is to download your jars in a quick and reliable manner.
adept will also be linked to adept servers, which gives you among other things easy-to-find dependencies and browsable dependencies.

adept does not build your project or have plugins. It is still flexible through its API which is extensible and composable.

## Workflow ##

1. pull an adept repository containing all the metainformation about the dependencies in the repository
2. find the jar files you want based in the newly created repository
3. list all the dependencies you need
4. download the jar files in bulk

## Core features ##

Adept introduces 2 concepts which sets it apart from the others (Maven, Ivy):

### Eagerly fetched metadata ###
Instead of looking up metadata when looking for dependencies, adept downloads all the information about  it needs once, like Git. This metadata is versioned and it is only updated when needed (when the user decides it or if the build tool sees that it has to update). This is different from Ivy and Maven which has to look for metadata about a module and its dependencies each time it finds a module it wants to download (and then look up each of the dependencies it found).

The advantages with Adept's approach are:
   a) it makes metadata resolution predicable: no more failed lookups or silently changing modules 
   b) it is faster to resolve because it does not need to resolve each dependency. It only needs to go online when it actually downloads an artifact that is not there and when you update the metadata.
   c) it is faster to download artifacts since they can be all done in parallell
   d) it is easier to debug because you have the metadata on your disk already - there is no lookup that was failing or resolver that was wrong. If something fails on the metadata it will fail again every time after that again. If dependency resolution fails you can easily figure out whether it is a metadata issue or an artifact issue since metadata and artifacts are handled completely separately.
   e) it makes it possible to search and discover dependencies offline. You can also have features like did-you-mean (imagine you have misspelled the version of something and it suggests the correct one)

The disadvantages is that there can be quite a lot of metadata. To make up for this, I have done performance benchmarking and checked if the technologies I am using (Git) works well enough for a repository with about 20 000 modules. Git compresses data and it makes the download quite small (4-5 mbs from ~40 mbs). Because of compression, I _believe_ (need more data to be sure) that even for the first time you use adept for a repository equivalent to maven-central it should be faster or equivalent to Ivy.

### Stronger metadata ###
First, adept has unique ids for each module and for each artifact. Therefore its versioning scheme is more precise. This feature makes it possible for a build to be 100% reproducible in terms of the artifacts and it makes it much easier for a build tool to exclude the dependencies it knows it does not need (such as incompatible scala-versions). You will also know if an artifact is the right one and whether it is already been downloaded or not. Thus, you never have to download an artifact twice, even if the metadata changes. It also has other attributes such as what scala version it was built with, which can be part of the dependency resolution. 


A lazy transitive dependency means that the dependencies of a dependency are calculated when the parent dependency is resolved.
With ivy, if I declare that I want the dependency for apache commons for example, ivy  will look into it's resolvers and figure out what dependencies it has and start downloading those. It will keep doing this  until all dependencies are resolved.

adept does not do that. adept has a repository containing the entire graph of the dependencies stored locally.

The pros are as follows:
- there is never a doubt of where a dependency comes from
- it makes it possible to download more quickly since we can bulk download
- offline browsing and discovery

The cons are:
- you will have more information (the entire dependency graph) in your project than earlier

## Other features ##

- downloads faster than maven's dependency manager and ivy
- unique ids for artifacts and  modules (the metadata describing a dependency, it artifacts and dependencies) and artifacts
- easy to get started like normal, but also easy to debug
- 100% predicable builds
- works with version control systems (i.e. text based, human readable)
- command line that makes 

## Roadmap ##

1. An experimental alpha release that can be used with sbt as Ivy
2. Design document
3. stabilize core features
   - add gradle support
5. ant support
6. secured, enterprise version
7. gui command line tooling
8. maven support???

## Building it ##
[![Build Status](https://travis-ci.org/freekh/adept.png?branch=master)](https://travis-ci.org/freekh/adept)


## Trying it out ##

### Command line ###
To try out the newly build  adept from the command line you can run `sbt adept-create-script`. 
This will build and create a `adept` file in the `bin` folder. Add this to your path to play around with it.

Move to you home-directory (`cd ~`) and initialize adept:
```bash
adept init
```
This will create an `.adept` directory.

Add some dependencies to your adept directory using Ivy:
```bash
adept ivy-add com.typesafe.akka:akka-actor_2.10:2.1.0
```

You should now see some files appearing in: `.adept/local`


You can also try committing by doing:
```bash
adept commit "message"
```

If you move to the `.adept/local/` directory you can see that this is a regular git repository, where you can push, pull etc etc as you want. 

Now it is time to try the adept SBT plugin.

### The SBT plugin ###

Go to the adept-sbt test project, by:
```bash
cd adept-sbt/test-project;
```

Then run:
```bash
sbt compile #compile with sbt
```
This will clone the repository here:
```
https://github.com/freekh/adept-central
```
then start downloading the dependencies it needs.

If you do:
```
sbt "show full-classpath"
```
You should see the artifacts on the classpath.
