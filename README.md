# Wolfram Workbench Common
[![release](http://img.shields.io/github/release/jkuczm/WWBCommon.svg)](https://github.com/jkuczm/WWBCommon/releases/latest)
[![license GPLv3](http://img.shields.io/:license-GPLv3-brightgreen.svg)](https://github.com/jkuczm/WWBCommon/blob/master/LICENSE)


Set of packages and scripts common to my projects developed using Wolfram
Workbench.


* [Features](#features)
* [Installation](#installation)
    * [Project import](#project-import)
    * [Ant runtime configuration](#ant-runtime-configuration)
    * [Example project configuration](#example-project-configuration)
        * [Simple docbuild](#simple-docbuild)
        * [Docbuild and deploy script](#docbuild-and-deploy-script)
* [Bugs and requests](#bugs-and-requests)
* [Contributing](#contributing)
* [License](#license)



## Features

* Tools "fixing" documentation:
    * making it cross version compatible,
    * fixing links in HTML version.
* Tools making source notebooks version control friendly:
    * disabling notebook cache,
    * disabling notebook cells history tracking,
    * removing existing cell change times.
* Scripts simplifying customization of documentation building and package
  deployment.



## Installation


### Project import

1. Download
   [repository archive](https://github.com/jkuczm/WWBCommon/archive/master.zip).

2. In Eclipse choose: `File` > `Inport...`.

3. Select: `General` > `Existing Projects into Workspace`, click `Next >`.

4. Choose `Select archive file` and `Browse...` for downloaded archive file.

5. Make sure WWBCommon is selected in `Projects` window and click `Finish`.


### Ant runtime configuration

To make files available to Ant scripts from all projects perform following
steps.

1. In Eclipse choose: `Window` > `Preferences`.

2. Select `Ant` > `Runtime`

3. In `Properties` tab click on `Add property...`

4. Type in:
   Name: `WWBCommonPath`,
   Value: `${workspace_loc}/WWBCommon`,
   click `OK`.

5. Click `Apply`, than `OK`.


### Example project configuration

#### Simple docbuild

1. Copy `examples/docbuild.xml` file to root directory of your project
   (or any other location and point it in:
   `Project` > `Properties` > `Mathematica` > `Paclet Settings` >
   `Documentation Build File`).

2. In copied `docbuild.xml` file change `MySuperDuperMmaApp` to name of your
   application. If you placed `docbuild.xml` somewhere else than root of your
   project adapt `basedir` attribute of `project` element.

#### Docbuild and deploy script

1. Copy `examples/buildScripts` directory to root directory of your project.

2. In copied `buildScripts/project.properties` file change `MySuperDuperMmaApp`
   to name of your application.

3. Change:
   `Project` > `Properties` > `Mathematica` > `Paclet Settings` >
   `Documentation Build File`
   setting to `buildScripts/docbuild.xml`.


## Bugs and requests

If you find any bugs or have feature request please create an
[issue on GitHub](https://github.com/jkuczm/WWBCommon/issues).



## Contributing

Feel free to fork and send pull requests.

All contributions are welcome!



## License

This project is released under
[GNU General Public License version 3](https://github.com/jkuczm/WWBCommon/blob/master/LICENSE).


### Attribution

Parts of code of this project are modifications of files from
[xTras package](https://github.com/xAct-contrib/xTras) by
[Teake Nutma](http://www.aei.mpg.de/~nutma/) used under
[GNU General Public License version 3](https://github.com/xAct-contrib/xTras/blob/master/LICENSE).

Parts of code of this project are a derivative of code written by
[Teake Nutma](http://mathematica.stackexchange.com/users/5485) and
[Simon Rochester](http://mathematica.stackexchange.com/users/8253)
in
[Creating cross-version compatible documentation with Workbench](http://mathematica.stackexchange.com/questions/28316)
thread on Mathematica Stack Exchange used under
[Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/).
