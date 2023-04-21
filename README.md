# hsFHIR

# Possible Improvements

- allow recursive imports of extensions and ResourceContainer which can be broken by hs-boot files (see GHC 5.8.10).
- naming of constants to eliminate conflicts (xsd2hs.erl)

# steps to add new Resource

1. new module in Data/FHIR/Resources
    - data definitions
    - from/to JSON/XML
2. add resource in
    - src/Data/FHIR/Datatypes/ResourceTypes.h
    - src/Data/FHIR/IdMeta.hs
    - src/Data/FHIR/Interface.hs
    - src/Data/FHIR/Resources.hs
    - src/Data/FHIR/Resources/ResourceContainer.hs
3. add resource in cabal  
4. resolve possible export conflicts from new modules

# Excerpts from www.hl7.org/fhir/http.html

# 3.1.0.11.1 Batch Processing Rules

For a batch, there SHALL be no interdependencies between the different entries in the Bundle that cause change on the server. The success or failure of one change SHOULD not alter the success or failure or resulting content of another change. Servers SHOULD validate that this is the case. Note that it is considered that servers execute the batch in the same order as that specified below for transactions, though the order of execution should not matter given the previous rule.

References within a Bundle.entry.resource to another Bundle.entry.resource that is being created within the batch are considered to be non-conformant.

When processing the batch, the HTTP response code is 200 Ok if the batch was processed correctly, regardless of the success of the operations within the Batch. To determine the status of the operations, look inside the returned Bundle. A response code on an entry of other than 2xx (200, 202 etc) indicates that processing the request in the entry failed. 

# 3.1.0.11.2 Transaction Processing Rules

For a transaction, servers SHALL either accept all actions and return a 200 OK, along with a response bundle (see below), or reject all resources and return an HTTP 400 or 500 type response. It is not an error if the submitted bundle has no resources in it. The outcome of processing the transaction SHALL NOT depend on the order of the resources in the transaction. A resource can only appear in a transaction once (by identity).

Because of the rules that a transaction is atomic where all actions pass or fail together and the order of the entries doesn't matter, there is a particular order in which to process the actions:

    Process any DELETE interactions
    Process any POST interactions
    Process any PUT or PATCH interactions
    Process any GET or HEAD interactions
    Resolve any conditional references

If any resource identities (including resolved identities from conditional update/delete) overlap in steps 1-3, then the transaction SHALL fail.

A transaction may include references from one resource to another in the bundle, including circular references where resources refer to each other. When the server assigns a new id to any resource in the bundle which has a POST method as part of the processing rules above, it SHALL also update any references to that resource in the same bundle as they are processed (see about Ids in a bundle). References to resources that are not part of the bundle are left untouched. Version-specific references should remain as version-specific references after the references have been updated. Note that when building a transaction, a client can use arbitrarily chosen version references since they will all be re-assigned anyway. Servers SHALL replace all matching links in the bundle, whether they are found in the resource ids, resource references, elements of type uri, url, oid, uuid, and <a href="" & <img src="" in the narrative. Elements of type canonical are not replaced.

When processing a "POST" (create), the full URL is treated as the id of the resource on the source, and is ignored; the server generates an id for the resource. For updates, the server performs a mapping between the fullUrl specified and the local URL the server knows that instance as, if possible. If the server does not have a mapping for the fullUrl, the server ignores the base URL and attempts an update assuming the base is the same as the server base. This allows the same transaction bundle to be sent to multiple systems without changing the fullUrls for each target.

When processing a batch or transaction, a server MAY choose to honor existing logical ids (e.g. Observation/1234 remains as Observation/1234 on the server), but since this is only safe in controlled circumstances, servers may choose to assign new ids to all submitted resources, irrespective of any claimed logical id in the resource, or fullUrl on entries in the batch/transaction.

Conditional References

When constructing the bundle, the client might not know the logical id of a resource, but it may know identifying information - e.g. an identifier. This situation arises commonly when building transactions from v2 messages. The client could resolve that identifier to a logical id using a search, but that would mean that the resolution to a logical id does not occur within the same transaction as the commit (as well as significantly complicating the client). Because of this, in a transaction (and only in a transaction), references to resources may be replaced by a search URI that describes how to find the correct reference:

~~
 <Bundle xmlns="http://hl7.org/fhir">
   <id value="20160113160203" />
   <type value="transaction" />
   <entry>
     <fullUrl value="urn:uuid:c72aa430-2ddc-456e-7a09-dea8264671d8" />
     <resource>
       <Observation>
         <subject>
            <reference value="Patient?identifier=12345"/>
         </subject>
         <--! rest of resource omitted -->
       </Observation>
     </resource>
     <request>
       <method value="POST" />
     </request>
   </entry>
 <Bundle>
~~~

The search URI is relative to the server's [base] path, and always starts with a resource type: [type]?parameters.... Only filtering parameters are allowed; none of the parameters that control the return of resources are relevant.

When processing transactions, servers SHALL:

    check all references for search URIs
    For search URIs, use the search to locate matching resources
    if there are no matches, or multiple matches, the transaction fails, and an error is returned to the user
    if there is a single match, the server replaces the search URI with a reference to the matching resource

# id resolution in batch/transaction

2.36.4 Resource URL & Uniqueness rules in a bundle

Except for transactions and batches, each entry in a Bundle must have a fullUrl which is the identity of the resource in the entry. Note that this is not a versioned reference to the resource, but its identity. Where a resource is not assigned a persistent identity that can be used in the Bundle, a UUID should be used (urn:uuid:...).

For transactions and batches, entries MAY not have fullURLs when the entry.request.method = POST, and the resource has no identity. Note that even in this case, there may still be a fullURL in a transaction on a POST so that relationships between resources can be represented (see Transactions).

A given version of a resource SHALL only appear once in each Bundle. There might, however, be multiple versions of a single resource present in a single bundle. This would be expected in Bundles of type history, and also might be necessitated by closely tracking Provenance.

Note that the meaning of an unversioned reference to a resource that appears multiple times is potentially ambiguous, though processors may have additional informaton to help resolve this (e.g. change order in a history bundle).

When processing batches and transactions, it is at server discretion how to behave if multiple versions of a single resource are present.

2.36.4.1 Resolving references in Bundles

The Bundle resource is a packaging construct that has one of more entries that are other kinds of resources. Those resources themselves have references to other resources - e.g. an Observation that refers to a Patient. The referenced resources may also be found in the Bundle. For example, the system that constructed the Bundle may have included both the Observation and the Patient. The content of the references between resources doesn't change because of the bundle.

This section documents a method that resolves references correctly within a bundle. Note that this method does not define any new semantics; resolution is based on the way resource identity and resource references work.

Applications reading a Bundle should always look for a resource by its identity in the bundle first before trying to access it by its URL externally.

How to resolve a reference in a Bundle:

    If the reference is not an absolute reference, convert it to an absolute URL:
        if the reference has the format [type]/[id], and
        if the fullUrl for the bundle entry containing the resource is a RESTful one (see the RESTful URL regex)
            extract the [root] from the fullUrl, and append the reference (type/id) to it
            then try to resolve within the bundle as for a RESTful URL reference.
            If no resolution is possible, then the reference has no defined meaning within this specification
        else no resolution is possible and the reference has no defined meaning within this specification
    else
        Look for an entry with a fullUrl that matches the URI in the reference
        if no match is found, and the URI is a URL that can be resolved (e.g. if an http: URL), try accessing it directly)

Note, in addition, that a reference may be by identifier, and if it is, and there is no URL, it may be resolved by scanning the ids in the bundle. Note also that transactions may contain conditional references that must be resolved by the server before processing the matches.

If the reference is version specific (either relative or absolute), then remove the version from the URL before matching fullUrl, and then match the version based on Resource.meta.versionId. Note that the rules for resolving references in contained resources are the same as those for resolving resources in the resource that contains the contained resource.

If multiple matches are found, it is ambiguous which is correct. Applications MAY return an error or take some other action as they deem appropriate.

There is an example Bundle that demonstrates these rules. 
