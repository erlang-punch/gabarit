# gabarit

![GitHub top language](https://img.shields.io/github/languages/top/erlang-punch/gabarit)
![GitHub last commit (branch)](https://img.shields.io/github/last-commit/erlang-punch/gabarit/master)
![GitHub contributors](https://img.shields.io/github/contributors/erlang-punch/gabarit)
![GitHub all releases](https://img.shields.io/github/downloads/erlang-punch/gabarit/total)
![GitHub repo size](https://img.shields.io/github/repo-size/erlang-punch/gabarit)

Gabarit, a template engine manager for Erlang/OTP.

**!!!This is a draft, please don't use it in production!!!**

Gabarit was created to offer an easy way to deal with template engines
from the Erlang community but also to deal with multi-template engines
running on the same cluster.

Gabarit uses bbmustache template engine as default template engine. If
another template is being used, gabarit will not automatically fetch
required dependencies, you will be in charge to add them manually.

Gabarit compiles an erlang module including file or directory
templates using merl. 

## Features

 - [x] templates versionned during compilation
 - [x] templates compiled with timestamp
 - [ ] hardcoded templates limits
 - [ ] templates compilation limit during runtime
 - [ ] automatic or dynamic reload
 - [ ] flexible rendering template service
 - [ ] rollback template based on version
 - [ ] export feature
 - [ ] template edition facility (text editor integration)
 - [ ] namespace support
   - [ ] default value and options based on namespace
   - [ ] partial templating
 - [ ] project manager support
   - [ ] rebar3 plugin
   - [ ] erlang.mk plugin
 - [ ] distributed feature and replication:
   - [ ] cluster template synchronization
   - [ ] template signature
 - [ ] callback module for:
   - [ ] bbmustache (default)
   - [ ] beard
   - [ ] dactyl
   - [ ] EEx (from elixir)
   - [ ] elk
   - [ ] erlydtl
   - [ ] etcher
   - [ ] jaderl
   - [ ] roni
   - [ ] sgte
   - [ ] swirl
   - [ ] templaterl
   - [ ] walrus

## Build

```sh
rebar3 compile
```

## Test

```sh
rebar3 ct
```

## Usage

All templates are by default stored in `priv/gabarit` directory, if
you add a new template outside of this directory, you will need to
load it manually.

```sh
mkdir priv/gabarit

# unique template
touch priv/templates/template.html
echo "<html>{{placeholder}}</html>" >> /tmp/templates/template.html

# multi-templates
mkdir priv/templates/templates
echo "<html>{{placeholder}}</html>" >> priv/templates/templates/index.html
echo "<html>{{placeholder}}</html>" >> priv/templates/templates/index2.html
```

```erlang
{ok, 'gabarit@/template.html' = Module} = gabarit:new("template.html").
%
{ok, Content} = Module:template().

% template printing
ok = Module:format(#{}).
ok = Module:format(#{}, []).

% rendering
{ok, Template} = Module:render(#{ "placeholder" => "test" }).
{ok, Template} = Module:render(#{ "placeholder" => "test" }, []).
```

Template directory support, recursive support enabled by default.

```erlang
{ok, ['gabarit@/templates/index.html'
     ,'gabarit@/templates/index2.html'
	 ] = Module} = gabarit:new("templates", [{recursive, true}]).
```

Function lists.

```erlang
% create a new template based on path
gabarit:new("path").

% export all compiled templates
% {ok, [{{TemplateName, Version, Timestamp}, {Filename, Source, Binary}}]}.
gabarit:export().

% create a new template based on path with
% custom template engine. If the file or the
% directory exists, it will create a new module 
% called: 'gabarit@/path'.
gabarit:new("path", [{engine, bbmustach}]).

% equivalent to
gabarit:new("path", [{'render/2', {bbmustache, render}}
                    ,{'render/3', {bbmustache, render}}
					]).

% create a template from data-structure. it will generate a 
% new module called: 'gabarit$name'.
gabarit:new_template("name", "template {{content}}.", []).

% list available templates
gabarit:get().

% set a new template engine for one template
gabarit:set(Template, 'render/2', {bbmustache, render}).

% compile and load all templates
gabarit:reload().

% compile and load one template
gabarit:reload("path").

% check the limits
gabarit:limits().
gabarit:limit(Limit).

% wrapper around 'gabarit@/path':format(#{}, []).
gabarit:format("path", #{}, []).

% wrapper around 'gabarit@/path':render(#{}, []).
gabarit:render("path", #{}, []).

% edit or create a new template by opening
% a text editor
gabarit:edit("path").
```

## Ideas

This project is side project created to play and improve template
management on Erlang/OTP system. Here list of ideas we would like to
work on.

### In Memory Compressed Templates

Templates can be really huge, and compressing them can save lot of
place, even more if they are using in a loaded module in the BEAM.

### Limitations

Erlang modules are compiled using atoms, atoms are limited and not
garbage collected. That means it could crash a production system if
you want to dynamically.

### Partial Templates

Sometimes, only a small part of the template needs to be dynamic, the
other part will stay static. Partial template will create a partial
template module with already replaced values in it.

### Development Environment

In development environment, templates can easily be upgraded and
updated on the fly, with local version. When one or more templates are
validated by developers, they can be exported with a fixed version.

### Production Environment

In production environment, templates are locked and can't be updated
on the fly, except during an explicit update/upgrade of the
application.

### Distributed Templates

In distributed environment, templates must be available on each nodes
requiring this module, even if the distribution does not have access
to them locally. We should probably also provide a RPC function.

### Custom Template Engine

A custom template engine with an AST representation and many way to
modify them, but without logic in it (avoid using if, for, while
expression). It should be developed in pure Erlang.

### Structured and Unstructured Template Engine Support

It could be great to support both structured and unstructured template
engines to generate, for example, XML, HTML, JSON but also Mardown and
raw textual files. 

### What About Binary Template Engine?

We are used to use textual templates, but rarely binary templates. In
fact, having binary template could be an extension of pattern matching
in Erlang, when one specific binary pattern can be replaced, extended,
removed on some part in data. I don't think it should be part of this
project, but could be a great idea to implement. What kind of usage?
Network package payload modification on the fly, data fuzzing and so
on. Here a code example of what I have in mind:

```erlang
% A mapping is a document containing address on a binary map with
% default instructions when the pattern needs to be changed.
Identifier = "identifier".
Mapping = [{Identifier, {From, To}}
          ,{Identifier2, {From, {absolute, To}}, Opts2}
		  ,{Identifier3, {From, {relative, To}}, Opts3}
		  ,{Identifier4, {From, To}, [reversed]}
		  % set an elf_magic identifier to replace the magic
		  % number used by an executable binary file using
		  % ELF. It will be a fixed size.
		  ,{"elf_magic", {16#00, 16#05}, [fixed]}
		  ].
{ok, Cat} = file:read_file("/bin/cat").
{ok, Module} = gabarit_binary:compile(Cat, Mapping).

% returns the data
Module:data().

Module:analyze().
Module:analyze(fun(X) -> 

% print the default mapping used for this file
Module:mapping().

% replace a pattern by a term
Module:replace(Identifier, Term).

% delete a pattern and resize the payload
Module:delete(Identifier).

% nullify a pattern (replace by 0x00).
Module:nullify(Identifier).

% randomize a pattern (replace with random payload)
Module:randomize(Identifier).

% clone a module with new identifier and/or already
% replaced values
Module:clone([{Identifier, {replace, Term, Opts}}]
            ,[{"newid", {From, To}, Opts}]).

% manual replacement
% {replace, {From, To}, Term, Opts}
% {nullify, {From, To}, Opts}
% {delete, {From, To}, Opts}
% {randomize, {From, To}, Opts}
Module:apply(Command, Opts).
Module:apply([Command, Command], Opts).

% some function to apply on the loaded payload
Module:map(fun(Integer) -> Integer end, Size).
Module:reduce(fun(Integer, BinAcc) -> <<Integer, BinAcc/binary>> end, Size).

```
