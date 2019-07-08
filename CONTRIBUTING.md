# How to contribute to this project?

## Table of Contents

  * [1. Starting](#1.-Starting)
  * [2. Structure of the project](#2.-Structure-of-the-project)
  * [3. Development environment](#3.-Development-environment)
  * [4. Setting up a development environment](#4.-Setting-up-a-development-environment) 
    * [4.1. Using *devtools* package](#4.1.-Using-devtools-package)
    * [4.2. Install the package in your enviroment throught Github](#4.2.-Install-the-package-in-your-enviroment-throught-Github) 
  * [5. Version control (Git)](#5.-Version-control-\(Git\))
    * [5.1. *Push* with new *commits* on the remote server](#5.1.-Push-with-new-commits-on-the-remote-server)
    * [5.2. *Pull* ](#5.2.-Pull)
    * [5.3. *Pull* with local changes not *commited* ](#5.3.-Pull-with-local-changes-not-commited)
  * [6. Style guide](#6.-Style-guide)
    * [6.1. Source code](#6.1.-Source-code)
    * [6.2. Version control](#6.2.-Version-control)

## 1. Starting

The bdpa4R project is developed in an architecture for an R package.

In this document you will find a description of this environment and the instructions to know how to contribute correctly to this project.

## 2. Structure of the project

This project is structured as follows:

- **DESCRIPTION**: File that contains the main information of the package.
- **README.md**: Home project where there are different instructions related to configuration, installation, copyright,how to cite the package and general information.
- **NEWS.md**: Point of information where the news and improvements made in the different versions are indicated.
- **CONTRIBUTING.md**: Guide where information of the project and the process to follow to realize new functionalities are shown. 
- **./R**: Main folder where are the different scripts that implement the different functionalities of the package. 
- **./exec**: Folder that contains the support scripts of different languages that are used in the project.
- **./man**: Folder where the .RD files are located that contains the documentation of the different functionalities offered by the package.
- **./inst**: Folder where there are files to support the package.
- **./additional-material**: It is a directory that accompanies the rest of the project in which additional files that may be useful, such as the images that appear in the README.md file, will be stored.
- **./vignettes**: Folder that contains the long-form guides to the package.
   
## 3. Development environment

The tools that make up the development environment are the following:

- **RStudio**: R development environment that allows the handling and development of a package in a comfortable way supporting the integration that has the devtools package. 
- **Git and Github**: Git is the version control system used in the project. It is a distributed version control system that facilitates collaboration between developers. It is necessary that you have installed Git in your system to be able to make changes in the project and collaborate with the rest of the team.

## 4. Setting up a development environment

Start working on the project is as simple as following the following steps:

1. Install Git. If you are on Ubuntu enviroment is easily like execute `sudo apt-get install git`. It is also recommended that you install some Git viewer such as *gitk* or *qgit*.
2. Clone the Git repository using the command:
    *git clone https://github.com/miferreiro/bdpa4R.git*
3. Install RStudio for R (optional but recommended):
     1. Download the IDE from https://www.rstudio.com/products/rstudio/download/
     2. Open the project in RStudio using *File...-->Open Project* and select the RProject file.

With this, it would be enough to start working on the project. If, in addition, you want to be able to execute it locally, you must follow the following instructions.

### 4.1. Using *devtools* package

To build the package it is necessary to have installed the *devtools* package which allows to build the package in the option to install and restart through the graphic interface.

In the case that the documentation in the code has been added or modified, it will be necessary to choose the option More ...-> Document.

### 4.2. Install the package in your enviroment throught Github

To install the package using Github, it is necessary to have installed the *devtools* package and use the following command:
```R
devtools::install_github(repo = "miferreiro/bdpa4R", ref = "master")
```

## 5. Version control (Git)

The version control model that we will use initially will be very simple since we will only use two branches:

* `master`: Only *commits* will be sent to this branch when it reaches a
stable and publishable version (a *release*). These versions must be tagged with the corresponding version number. 
* `develop`: This will be the main branch of work. The *commits* that are sent must be stable.

### 5.1. *Push* with new *commits* on the remote server

If you want to do a *push* to a remote server in which there are *commits* new ones that we do not have in local, then Git shows an error in which it tells us that we should do a *pull* before being able to do *push*.

Since we are not interested in having to add a *commit* of *merge* additional, the *pull* should be done by applying an *rebase*. For this the command must be used:
```
git pull --rebase
```
This command will initiate a *rebase* process between the local branch and the remote branch. That is, local *commit* not *pushed* will happen to have the last *remote* commit as a parent.

### 5.2. *Pull*

Before doing a *pull* you should always check the integration server
keep going.

### 5.3. *Pull* with local changes not *commited*
In case we are in the middle of a *commit* (the necessary changes have not been completed to perform a *commit*) and we wish to download new *commits* from the central server, we can do it using the commands:
```
git stash
git pull --rebase
git stash pop
```

## 6. Style guide

An important element to be able to collaborate is that there is uniformity in the code and other elements that are part of the development. This section serves as a small style guide that should be respected when working on the project.

### 6.1. Source code

In order to standardize the source code, the following rules must be respected:

* **Language**: All code (including documentation) must be developed in English. 
* **Code format**: The code should be formatted, preferably, following the [Google's R Style Guide](https://google.github.io/styleguide/javaguide.html).
* **Comments**: The commented code must be **completely** avoided and, in the as much as possible, the comments in the code. 
* **Documentation**: Document the function or developed class oriented to the help of the package.

### 6.2. Version control

One of the bases of development that we will use in this project is the **integrate as soon as possible**. For this, the following rules must be followed:
* **Content of the *commits***: The *commits* must be complete in the
sense that they should not break the construction.
* **Format**: The format of the *commits* must respect the following rules:
  * Written in English.
  * Limit the line size to 80 columns. 
  * First descriptive line of what the *commit*.
  * It must be written in the third person of this (e.g. *Adds...*, *Improves...*, *Modifies...*, etc.).
  * The title should not have a point at the end.
  * If it is an error, a descriptive phrase is placed.
  * Body's *commit* descriptive. With an empty line separating the first line, a text should be written to clearly explain the work done in the *commit*.
* ***Commit* frequency**: The *commits* should be done in small steps so that the frequency is high. For this it is advisable to develop in an orderly manner, attacking specific parts.
* ***Push* frequency**: Whenever a commit is done it should be done a *push*. The only exception to this rule is that we are doing local tests to evaluate a possible solution. In such a case, it is advisable that this be done in an independent branch to avoid sending *commits* accidentally to the *develop* remote branch.
