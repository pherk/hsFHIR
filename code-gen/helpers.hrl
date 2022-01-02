-ifndef(helpers).

-type prop_info() :: {binary(), prop_type(), occurs()}.

-type prop_type() ::
       single()
     | cluster()
     | metadata()
     | special().

-type single() ::
       base64Binary
     | binary
     | boolean
     | canonical
     | code
     | date
     | dateTime
     | decimal
     | dow
     | id
     | instant
     | markdown
     | oid
     | positiveInt
     | time
     | ucum
     | unsignedInt
     | uuid
     | uri
     | url.

-type cluster() ::
       address
     | annotation
     | attachment
     | codeableConcept
     | coding
     | contactPoint
     | humanName
     | identifier
     | period
     | quantity
     | range
     | ratio
     | repeat.

-type metadata() ::
       contactDetail
     | relatedArtefact.

-type special() ::
       extension
     | meta
     | narrative
     | reference.

-type occurs() ::
       optional
     | required
     | non_empty_list
     | list.

-endif.
