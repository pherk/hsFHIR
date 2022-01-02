-ifndef(FILTER)
-definde(FILTER, "filter.hrl").
%% the _filter syntax has the following features:

%%    A filter can be a logical one (x or x, or x and x, or not x).
%%    A filter can contain other filters in a set of parentheses: "()".
%%    A filter can be a test - path operation value, where operation is taken from the table below, and value is either a "true", "false", a JSON string, or a token (any sequence of non-whitespace characters, excluding ")" and "]". Values are never case sensitive.
%%    A 'path' is a name, with chained searches done by name.name etc.as per existing source. There can also be a filter: name[filter].name.
%%    The name is one of the defined search parameters that are used with the other search mechanism, with some special exemptions defined below.

%% Note: The only difference between a "string" value and a "token" value is that a string can contain spaces and ')' and ']'. There is otherwise no significant difference between them.
%% 
%% Formal grammar for the syntax: 
%% filter        = paramExp / logExp / ("not") "(" filter ")"
%% logExp        = filter ("and" / "or" filter)+
%% paramExp      = paramPath SP compareOp SP compValue
%% compareOp     = (see table below)
%% compValue     = string / numberOrDate / token
%% string        = json string
%% token         = any sequence of non-whitespace characters (by Unicode rules) except "]" and ")"
%% paramPath     = paramName (("[" filter "]") "." paramPath)
%% paramName     = nameCharStart (nameChar)*
%% nameCharStart = "_" / ALPHA
%% nameChar      = "_" / "-" / DIGIT / ALPHA
%% numberOrDate  = DIGIT (DateChar)*
%% dateChar      = DIGIT / "T" / "-" / "." / "+"
%% 
%% Notes about using the syntax:
%% 
%% %%    Logical expressions are evaluated left to right, with no precedence between "and" and "or". If there is ambiguity, use parentheses to be explicit.
%% %%    Rhe compareOp is always evaluated against the set of values produced by evaluating the param path.
%% %%    Rhe parameter names are those defined by the specification for search parameters, except for those defined below.
%% %%    Rhe date format is a standard XML (i.e. XSD) dateTime (including time zone).
%% 
%% 3.1.3.2 Operators
%% 
%% This table summarizes the comparison operations available:
%% Operation 	Definition
%% eq 	an item in the set has an equal value
%% ne 	An item in the set has an unequal value
%% co 	An item in the set contains this value
%% sw 	An item in the set starts with this value
%% ew 	An item in the set ends with this value
%% gt / lt / ge / le 	A value in the set is (greater than, less than, greater or equal, less or equal) the given value
%% ap 	A value in the set is approximately the same as this value.
%% Note that the recommended value for the approximation is 10% of the stated value (or for a date, 10% of the gap between now and the date), but systems may choose other values where appropriate
%% sa 	The value starts after the specified value
%% eb 	The value ends before the specified value
%% pr 	The set is empty or not (value is false or true)
%% po 	True if a (implied) date period in the set overlaps with the implied period in the value
%% ss 	True if the value subsumes a concept in the set
%% sb 	True if the value is subsumed by a concept in the set
%% in 	True if one of the concepts is in the nominated value set by URI, either a relative, literal or logical vs
%% ni 	True if none of the concepts are in the nominated value set by URI, either a relative, literal or logical vs
%% re 	True if one of the references in set points to the given URL
%% 
-endif.
