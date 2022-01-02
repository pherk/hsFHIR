-ifndef(primitives).
-define(primitives, true).

-type decimal() :: float().

-type uri() :: binary().

-type url() :: binary().

-type canonical() :: binary().
%% Regex: (\s*([0-9a-zA-Z\+\=]){4}\s*)+

-type  base64Binary() :: binary().
%% Regex: ([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\.[0-9]+)?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00))

-type  instant() :: binary().
%% Regex: ([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1]))?)?

-type  date() :: binary().
%% Regex: ([0-9]([0-9]([0-9][1-9]|[1-9]0)|[1-9]00)|[1-9]000)(-(0[1-9]|1[0-2])(-(0[1-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\.[0-9]+)?(Z|(\+|-)((0[0-9]|1[0-3]):[0-5][0-9]|14:00)))?)?)?

-type dateTime() :: date() | yearMonth() | year().

-type  yearMonth() :: binary().

-type  year() :: binary().

-type  dow() :: binary().
%% Regex: ([01][0-9]|2[0-3]):[0-5][0-9]:([0-5][0-9]|60)(\.[0-9]+)?

-type  time() :: binary().
%% Regex: [^\s]+(\s[^\s]+)*

-type  code() :: binary().
%% Regex: urn:oid:[0-2](\.(0|[1-9][0-9]*))+

-type  oid() :: binary().
%% Regex: [A-Za-z0-9\-\.]{1,64}

-type  id() :: binary().
%% Regex: \s*(\S|\s)*

-type  markdown() :: binary().
%% Regex: +?[1-9][0-9]*

-type  positiveInt() :: non_neg_integer().
%% Regex: [0]|([1-9][0-9]*)

-type  unsignedInt() :: non_neg_integer().

-type  uuid() :: binary().

-type  ucum() :: binary().


-endif.
