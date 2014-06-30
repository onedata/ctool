CTool

About
=====
CTool is library application that contains helper modules for OneData project.

Goals
-----
Ctool gathers code and dependencies that are common for all OneData subprojects.

Getting Started
---------------
#### 1. Dependencies
Including ctool in a project will cause it to automatically include following dependencies:

* lager
* ranch
* cowboy
* erlydtl
* n2o

#### 2. Functionalities
Ctool incorporates following functionalities:

* CT (common test) utilities
* logging system, based on lager
* common GUI utilities

#### 3. Logging system
Useful information on how to use the logging system:

* The application including ctool must contain a module called exactly 'logger_plugin', which must implement logger_plugin_behaviour. This allows for customisation of logging 

Support
-------
For more information visit project Confluence.
