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

## Building it ##
[![Build Status](https://travis-ci.org/freekh/adept.png?branch=master)](https://travis-ci.org/freekh/adept)

To start using adept from the command line you can run `sbt adept-create-script`. 
This will build and create a file in the `bin` folder.

## Trying it out ##

### Command line ###
Go to the adept directory and run:

```bash
sbt adept-create-script
```

This will create an `adept` script in the `bin/` folder.
Put this folder in your PATH, by following the instructions from the task.

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

Notice that there are 2 versions of Play. 
If you do:
```
sbt "show full-classpath"
```
You should see that only the latest version of Play is there.
