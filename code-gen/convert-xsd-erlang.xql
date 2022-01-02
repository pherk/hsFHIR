xquery version "3.1";

declare namespace fhir="http://hl7.org/fhir";
declare namespace xs="http://www.w3.org/2001/XMLSchema";

declare variable $local:whitelist := 
    (
          "Condition"
        , "Encounter"
        , "Patient"
        , "Task"
        );
        
declare function local:rep($e)
{
    switch($e/@minOccurs) 
    case '0' return if ($e/@maxOccurs = '1')
                then "optional"
                else "list"
    case '1' return if ($e/@maxOccurs = '1')
                then "required"
                else "non_empty_list"
    default return "optional"
    
};

declare function local:type($e,$xsd)
{
    let $ct := $xsd/xs:schema/xs:complexType[@name=$e/@type]
    return
        if ($ct/xs:complexContent/xs:extension/xs:attribute/@name='value')
        then let $type := replace($ct/xs:complexContent/xs:extension/xs:attribute/@type,'-','_')
            let $ttoks := tokenize($type,'_')
            let $primitive := $ttoks[2] = 'primitive'
            return if ($primitive)
                then
                    concat('{primitive, <<"', $ttoks[1], '">>}')
                else
                    concat('{code, <<"', lower-case($type), '">>}')
        else if ($ct/xs:complexContent/xs:extension/@base='BackboneElement')
        then
                concat('{bbelement, <<"', $e/@type, '">>}')
        else if ($e/@type='ResourceContainer')
        then
                concat('{container, <<"', $e/@type, '">>}')
        else if ($e/@type=('Extension','ModifierExtension'))
        then
                concat('{extension, <<"', $e/@type, '">>}')
        else if ($e/@type=('ContactDetail','DataRequirement','Expression','ParameterDefinition','ProductShelfLife','RelatedArtifact','TriggerDefinition','UsageContext'))
        then
                concat('{metadata, <<"', $e/@type, '">>}')
        else
                concat('{complex, <<"', $e/@type, '">>}')
};

declare function local:props($props,$xsd, $nl)
{
    if (count($props) > 0)
    then 
        (
          '            [',
            string-join(
                for $p in $props
                return
                    if (local-name($p)='element')
                    then
                        concat('            {<<"', 
                            $p/@name, '">>, {', local:type($p,$xsd), ', ',local:rep($p), '}}')
                    else if (local-name($p)='choice')
                    then 
                        let $rep := local:rep($p)
                        for $e in $p/xs:element
                        return
                            concat('            {<<"',
                                $e/@name, '">>, {', local:type($e,$xsd), ', ',$rep, '}}')
                    else ()
                , concat(',',$nl))
        , "            ],")
    else 
       "            [],"
};

declare function local:restrictions($props,$xsd, $nl)
{
    let $relems := $props[local-name(.)='choice']
    return
    if (count($relems) > 0)
    then 
        (
          '            [',
            string-join(
                for $r in $relems
                return
                    concat('            {',
                        string-join((
                            for $e in $r/xs:element
                            return
                                concat('<<"', $e/@name, '">>')
                        )
                        , ', ')
                        , '}')
                , concat(', ',$nl))
        , "            ]")
    else 
       "            []"
};
declare function local:attrs($attrs, $nl)
{
    if (count($attrs)>0)
    then
        string-join(('            [', for $a in $attrs
                                      let $type := tokenize($a/@type,'-primitive')[1]
                                      return concat('{<<"',$a/@name, '">>, <<"', $type, '">>}'), '],'), "")
    else
        "            [],"
};

let $nl := codepoints-to-string(10)
let $xsd := doc("/db/apps/nabu/FHIR-XSD-4.0/fhir-single.xsd")
let $ctypes := $xsd/xs:schema/xs:complexType[xs:complexContent/xs:extension/@base=('Element','Resource')] |
                $xsd/xs:schema/xs:complexType[xs:complexContent/xs:extension/@base='DomainResource'][@name=$local:whitelist] |
                $xsd/xs:schema/xs:complexType[xs:complexContent/xs:extension/@base='BackboneElement'][starts-with(@name,$local:whitelist)]
let $stypes := $xsd/xs:schema/xs:simpleType
return
<output>
{
    string-join((
         "-ifndef(fhir_xsd)."
        , "%%%"
        , "%%%","%%% FHIR XSD schema converted to erlang map and types"
        , concat("%%% @version : ", $xsd/xs:schema/@version)
        , "%%% do not edit or know what you are doing ;-)"
        , "%%% generated with convert-xsd-erlang.xql script"
        , "%%% comments, known deficits:"
        , "%%% - attributes exists only in Element (id) and Extension (url)"
        , "%%% - xs:choice are all converted as simple or (no restriction on parallel use)"
        , '%%% - Narrative has to be manually fixed (xhtml:div ref, no type)'
        , "%%% - simple elements are defined in XSD as a type which refers to a type with value attribute plus code-list simple type"
        , "%%%   these are converted to a union type, that means these type are not in the big map"
        , "%%%"
        , ""
        , "%%%"
        , "%%% complex types"
        , "%%%"
        , "-define(fhir_xsd, "
        , "    #{"
        ),$nl)
}
{
for $type at $pos in $ctypes
return
    let $ct := $type
    let $annots := $ct/xs:annotation/xs:documentation/string()
    let $props := $ct/xs:complexContent//xs:sequence/xs:element | $ct/xs:sequence/xs:element |$ct/xs:complexContent//xs:sequence/xs:choice
    let $attr  := $ct/xs:complexContent//xs:attribute | $ct/xs:attribute
    return
        if ($attr/@name='value' or $ct/xs:choice)
        then ()
        else
        string-join((
                concat($nl,"%%",$nl,"%% ", $ct/@name)
            ,   for $annot in $annots
                return
                    concat("%% ", $annot)
            ,   "%%"
            ,   if ($pos = 1) (: index in for loop wrong :)
                then concat('      <<"', $ct/@name, '">> => {<<"', $ct/xs:complexContent/xs:extension/@base, '">>,')
                else concat('    , <<"', $ct/@name, '">> => {<<"', $ct/xs:complexContent/xs:extension/@base, '">>,')
            ,   local:props($props,$xsd, $nl)
            ,   local:attrs($attr, $nl)
            ,   local:restrictions($props, $xsd, $nl)
            ,   '}'
            ), $nl)
}
{
    string-join((
          "        })."
        , ""
        , "%%%"
        , "%%% complex types with xs:choice as root"
        , "%%%"
        ),$nl)
}
{
for $type at $pos in $ctypes
return
    let $ct := $type
    let $annots := $ct/xs:annotation/xs:documentation/string()
    let $elems := $ct/xs:choice/xs:element
    return
        if ($ct/xs:choice)
        then
        string-join((
                concat($nl,"%%",$nl,"%% ", $ct/@name)
            ,   for $annot in $annots
                return
                    concat("%% ", $annot)
            ,   "%%"
            ,   (
                  concat("%% ", $ct/@name, $nl)
                , concat('-type ', replace(lower-case($ct/@name),'-', '_'), '() :: ',$nl)
                , concat('            ',
                    string-join((for $e in $elems
                    return 
                    concat("'", $e/@ref, "'")), concat($nl, '           | ')), '.', $nl)
                )
            ), $nl)
        else ()
}
{
    string-join((
          "        })."
        , ""
        , "%%%"
        , "%%% simple types"
        , "%%%"
        ),$nl)
}
{
for $type in $stypes
return
    let $st := $type
    let $elems := $st//xs:enumeration
    let $union := $st//xs:union
    return
        if (count($elems) > 0)
        then
            (
              concat("%% ", $st/@name, $nl)
            , concat('-type ', replace(lower-case($st/@name),'-', '_'), '() :: ',$nl)
            , concat('            ',
                string-join((for $e in $elems
                    return 
                    concat("'",$e/@value,"'")), concat($nl, '           | ')), '.', $nl)
            )
        else if ($union)
        then
            (
              concat("%% ", $st/@name, $nl)
            , concat('-type ', replace($st/@name,'-','_'), '() :: ',$nl)
            , concat('            ',
                string-join((for $e in tokenize($union/@memberTypes,' ')
                    return 
                        concat("'",$e,"'")), concat($nl, '           | ')), '.', $nl)
            )
        else ()
}
{
    string-join((
          "-endif."
        ),$nl)
}
</output>
