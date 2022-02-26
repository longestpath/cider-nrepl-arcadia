# Cider NRepl Arcadia Unity

This is the result of exploring NRepl, and being really excited by Arcadia in Unity.

While the miracle repl is very nice, my workflow is more cuited around NRepl and specifically Cider, so this is an attempt to start from the Arcadia NRepl code and port it over to something that can be used a bit more like nrepl/nrepl.

[NOTE: This is currently experimetnal, but seems promising. Unity has a bunch of really interesting limitations on what can be executed where, so this may end up being some middleware that changes how eval works :shrug: Enjoy!]

## Basic Install

I'm using this as a submodule under `Assets/dependencies/cider-nrepl-arcadia-unity` and symlinking it into `game` so I can develop it while I work on my arcadia project. Process described in this [StackOverflow post](https://stackoverflow.com/questions/7597748/linking-a-single-file-from-another-git-repository).

Here's the relevant info:
```
$ git submodule add /url/submodule/<reponame>
$ ln -s <reponame>/path/to/<linked_file>
$ git add .gitmodules <linked_file>
$ git commit -m "add a symbolic link to <linked_file> with the respective submodule"
```
