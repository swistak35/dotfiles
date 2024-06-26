compdef zeus

local -a _1st_arguments
_1st_arguments=(
'console:Lets you interact with your Rails application from the command line. (alias = c)'
'cucumber:Runs cucumber.'
'dbconsole:Figures out which database you are using and drops you into whichever command line interface.'
'destroy:Figures out what generate did, and undoes it. (alias = d)'
'generate:Uses templates to create a whole lot of things. (alias = g)'
'rake:Execute rake tasks.'
'runner:Runs Ruby code in the context of Rails non-interactively. (alias = r)'
'server:Launches a small web server named WEBrick which comes bundled with Ruby. (alias = s)'
'start:Preloads the zeus environment'
'test:Runs RSpec tests. (alias = rspec, testrb)'
'version:Shows the version number.'
)

local expl
local -a pkgs installed_pkgs

_arguments \
	'*:: :->subcmds' && return 0

if (( CURRENT == 1 )); then
	_describe -t commands "zeus subcommand" _1st_arguments
	return
fi

_files
