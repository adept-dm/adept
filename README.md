# adept #

adept is a new dependency manager and aims to replace apache ivy.

It is not a build system like maven, gradle or sbt, but can be used inside of the build systems to load dependencies.

Have you ever been surprised by how long it takes to download dependencies using maven or ivy? I mean your adsl line should download the 10-100 mbs of data so fast right?
Have you ever had to debug maven and ivy, having to hack your way to figure out why your classpath is wrong and why some dependencies are there or not.
Ever thought: I know what jars I want, give them to me!

adept is here to answer those thoughts!

## Scope ##

What adept does is to download your jars in a quick and reliable manner.
adept will also be linked to the adept webpage, which gives you among other things easy-to-find dependencies and browsable dependencies.

adept does not build your files or have plugins. It is extensible through an api though.

## Workflow ##

1. find the dependencies you want on the adept webpage
2. download the adept files (they contain information about where to find the jars)
3. setup you build system to use the adept files
4. use you build tool with adept integration to start downloading the dependencies
5. relax and take a shot of vodka (you won't have time for a beer) and observe how quickly and flawlessly it downloads your dependencies and puts them on the classpath

## Not being lazily transitive ##

A lazy transitive dependency means that the dependencies of a dependency are calculated when the parent dependency is resolved.
With ivy, if I declare that I want the dependency for apache commons for example, ivy  will look into it's resolvers and figure out what dependencies it has and start downloading those. It will keep doing this  until all dependencies are resolved.

adept does not do that. adept has descriptor files containing the entire graph of the dependencies.

The pros are as follows:
- there is never a doubt of where a dependency comes from
- it makes it possible to download more quickly since we can bulk download
- the dependency graph is what you have downloaded, it will not change

The cons are:
- you will have more information (the entire dependency graph) in your project than earlier
- instead of just adding text in your build file you have to use a tool or similar to download and find versions

## Core features ##

- downloads a LOT faster than maven's dependency manager and ivy
- never-fail caching, using hashes
- easy to get started like normal, but also easy to debug
- 100% predicable builds (when not using snapshots)
- highly fault tolerant dependency download using torrents
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
9. maven support
10. gui command line tooling

### License ###

This is *NOT* free code. 

You do NOT have the rights to use nor copy nor modify the code found here for your own or commercial use.

The reason for this is that we plan on creating a dual license where commercial-use costs something, but where it is free to modify and use as long as it is open sourced.
This is not something we do to be evil, but unfortunately we need money to make adept happen and this is the way we have chosen.
Until the time this license has been fleshed out, please email: fredrik@groosker.com and ask for guidance.

Copyright Groosker SARL 2012
