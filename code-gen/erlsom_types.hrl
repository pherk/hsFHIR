-ifndef(ERLSOM_ANY_ATTRIB_TYPES).
-define(ERLSOM_ANY_ATTRIB_TYPES, true).
-type anyAttrib()  :: {{string(),    %% name of the attribute
                        string()},   %% namespace
                        string()}.    %% value
 
-type anyAttribs() :: [anyAttrib()] | undefined.
-endif.

