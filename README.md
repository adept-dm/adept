# Adept #
_- the predictable dependency management system where faith is not required_

<img src="https://raw.github.com/wiki/adept-dm/adept/images/logo_adept_hood.png"
 alt="Adept logo" title="Adept" align="right" />

Adept is a dependency management system for the JVM platform that helps you:
- discover new modules or new versions of existing modules
- publish/add/declare new dependencies
- download dependencies quickly

Adept differs from Ivy and Maven in that it downloads all dependency metadata for all modules in tandem from their respective Git repositories. Leveraging this meta-data, it can **uniquely identify** modules and artifacts.
This makes adept well-suited for discovering dependencies in addition to being predictable and fast.

**To find out more, please check out the [Adept wiki] [wiki].**

## Overview

<img src="https://raw.github.com/wiki/adept-dm/adept/images/adept_diagram.png"
 alt="Adept diagram" title="Adept diagram" align="center" />

* **Git** stores the versioned meta-data
* **Repositories** contain the meta-data in individual modules
* **Modules** contain the information needed to create the classpath (such as dependencies to other modules, and the locations of artifacts)
* **Build tools**, such as SBT, Gradle (not currently supported), and Ant (not currently supported), use Adept to create their classpath based on a set of modules

**For more information on the current Adept architecture, please see the [Design documentation] [design]**.

## Find out more

| Technical Docs                  | Setup Guide               | Roadmap                 | Contributing                      |
|---------------------------------|---------------------------|-------------------------|-----------------------------------|
| ![i1] [techdocs-image]          | ![i2] [setup-image]       | ![i3] [roadmap-image]   | ![i4] [contributing-image]        |
| **[Technical Docs] [techdocs]** | **[Setup Guide] [setup]** | **[Roadmap] [roadmap]** | **[Contributing] [contributing]** |

## Contributing

Adept is currently in the proposal and discussion stage! 

Do you want to take part in shaping the ultimate dependency managmenet system on the JVM for years to come? 


**Now is the time! Head over to the [Contributing] section**



## Questions or need help?

No question should be unasked: head to the [issues tracker] [issues] or start a thread on the [mailing list] [mailinglist].

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

[techdocs]: https://github.com/adept-dm/adept/wiki/Docs
[setup]: https://github.com/adept-dm/adept/wiki/Setup
[roadmap]: https://github.com/adept-dm/adept/wiki/Roadmap
[contributing]: https://github.com/adept-dm/adept/wiki/Contributing

[techdocs-image]: https://github.com/adept-dm/adept/wiki/images/techdocs.png
[setup-image]: https://github.com/adept-dm/adept/wiki/images/setup.png
[roadmap-image]: https://github.com/adept-dm/adept/wiki/images/roadmap.png
[contributing-image]: https://github.com/adept-dm/adept/wiki/images/contribute.png

