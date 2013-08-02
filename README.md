# Adept #
_- the predictable dependency management system where faith is not required_

<img src="https://raw.github.com/wiki/adept-dm/adept/images/logo_adept_hood.png"
 alt="Adept logo" title="Adept" align="right" />

Adept is a dependency management system for the JVM platform.

Adept was created because we were:
* Sick of a 'download the Internet' step in your builds that takes forever
* Sick of non-reproducible builds
* Sick of mysteriously corrupt caches
* Sick of complex publish procedures

Adept gives you speed and deterministic behavior:
* Separates metadata and artifacts
* Downloads everything in parallel because it can do all resolution up-front
* Uses git and hashes to cache **RELIABLY** and avoid extra http requests
* Publish by just pushing to a git repository

What's more, Adept has an Ivy importer so you can use it today without waiting for the world to change.

Adept is currently in active development, but you can already try [it](https://github.com/adept-dm/adept/wiki/Setup) and see for yourself!

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

