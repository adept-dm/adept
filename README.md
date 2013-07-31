# Adept #

<img src="https://raw.github.com/wiki/adept-dm/adept/images/logo_adept_hood.png"
 alt="Adept logo" title="Adept" align="right" />

Adept is a dependency management system for the JVM platform that helps you:
- discover new modules or new versions of existing modules
- publish/add/declare a new dependencies
- download dependencies quickly

Adept is different from Ivy and Maven in that it all downloads all metadata from a Git repository once and has strong meta-data support to **uniquely identify** modules and artifacts.
This makes adept well-suited for discovering dependencies in addition to being predictable and fast.

**To find out more, please check out the [Adept wiki] [wiki].**

## Overview

<img src="https://raw.github.com/wiki/adept-dm/adept/images/adept_diagram.png"
 alt="Adept diagram" title="Adept digaram" align="center" />

* **Git** is used to stored versioned meta-data
* **Repository** contains the meta-data in individual modules
* **Module** contains information needed to create the classpath such as: dependencies to other modules and the locations of artifacts
* **Build tools**, such as SBT, Gradle (not currently supported), Ant (not currently supported) uses Adept to create their classpath based on a set of modules

**For more information on the current Adept architecture, please see the [Design documentation] [design]**.


## Contributing

Adept is currently in the proposal and discussion stage and needs your involvement!

If you want to learn or learn how to develop in Scala, SBT, Gradle or Ant, Adept should be a perfect place for you to start.

Discuss existing ideas and propose new ones on the development [mailing list] [mailinglist]


## Questions or need help?

No question should be unasked: head to the [issues tracker] [issues] or start up a thread on the [mailing list] [mailinglist].

## License

Licensed under the **[Apache License, Version 2.0] [license]** (the "License");
you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.


[wiki]: https://github.com/adept-dm/adept/wiki/Home
[mailinglist]: http://groups.google.com/group/adept-dev/
[issues]: https://github.com/adept-dm/adept/issues
[design]: https://github.com/adept-dm/adept/wiki/Design
[license]: http://www.apache.org/licenses/LICENSE-2.0
