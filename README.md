# gabarit

Gabarit, a template engine manager for Erlang/OTP.

Gabarit was created to offer an easy way to deal with template engines
from the Erlang community but also to deal with multi-template engines
running on the same cluster.

Gabarit uses bbmustache template engine as default template engine. If
another template is being used, gabarit will not automatically fetch
required dependencies, you will be in charge to add them manually.

Gabarit compiles an erlang module including file or directory
templates using merl. 

## Features

 - [ ] templates versionned during compilation
 - [ ] templates compiled with timestamp
 - [ ] hardcoded templates limits
 - [ ] templates compilation limit during runtime
 - [ ] automatic or dynamic reload
 - [ ] flexible rendering template service
 - [ ] rollback template based on version
 - [ ] export feature
 - [ ] template edition facility (text editor integration)
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

## Limitations

Erlang modules are compiled using atoms, atoms are limited and not
garbage collected. That means it could crash a production system if
you want to dynamically
