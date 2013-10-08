# Adept #
_- the predictable dependency management system where faith is not required_

<img src="https://raw.github.com/wiki/adept-dm/adept/images/logo_adept_hood.png"
 alt="Adept logo" title="Adept" align="right" />

Adept is a dependency management system for the JVM platform.

Adept was created because we:
* Had to! Today's dependency managers does not support reality anymore: one version attribute to handle resolution is not enough with multiple JVMs and _other_ non compatible libraries.
* Were sick of a 'download the Internet' step in your builds that takes forever
* Were sick of non-reproducible builds
* Were sick of mysteriously corrupt caches
* Were sick of complex publish procedures

Adept gives you speed and deterministic behavior:
* Smarter metadata gives exactly what you want or tells you what is wrong
* Separates metadata and artifacts
* Downloads everything in parallel because it can do all resolution up-front
* Uses git and hashes to cache **RELIABLY** and avoid extra http requests
* Publish by just pushing to a git repository


Note that we are changing the design of Adept!

The new spec can be found here: https://docs.google.com/document/d/1xU9m2zxva2eKhiXVYYqjmZieaWPJY0mDbEmZ_pE5P5c/edit?usp=sharing
The new sources are located on the master branch - the old ones are on mark-i.


# DEPRECATED
## Find out more

| Technical Docs                  | Setup Guide               | Roadmap                 | Contributing                      |
|:-------------------------------:|:-------------------------:|:-----------------------:|:---------------------------------:|
| ![i1] [techdocs-image]          | ![i2] [setup-image]       | ![i3] [roadmap-image]   | ![i4] [contributing-image]        |
| **[Technical Docs] [techdocs]** | **[Setup Guide] [setup]** | **[Roadmap] [roadmap]** | **[Contributing] [contributing]** |

## Contributing

Adept is currently in the proposal and discussion stage! 

Do you want to take part in shaping the ultimate dependency managmenet system on the JVM for years to come? 


**Now is the time! Head over to the [Contributing] section**

## Partners

**Do you struggle with your builds at work and think Adept could solve your issues?**

**Want to enage in Adept now and make sure your requirements are met?**

Create an [issue] [issues] and tell the world about your interest or drop me a mail directly if privacy is required: fredrik.ekholdt (at) typesafe.com !


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

[techdocs]: https://github.com/adept-dm/adept/wiki/Documentation
[setup]: https://github.com/adept-dm/adept/wiki/Setup
[roadmap]: https://github.com/adept-dm/adept/wiki/Roadmap
[contributing]: https://github.com/adept-dm/adept/wiki/Contributing

[techdocs-image]: https://github.com/adept-dm/adept/wiki/images/techdocs.png
[setup-image]: https://github.com/adept-dm/adept/wiki/images/setup.png
[roadmap-image]: https://github.com/adept-dm/adept/wiki/images/roadmap.png
[contributing-image]: https://github.com/adept-dm/adept/wiki/images/contribute.png

