-include("include/ar.hrl").

-record(result, {
	messages = [],
	assignments = [],
	spawns = [],
	output = [],
	cursor = undefined
}).