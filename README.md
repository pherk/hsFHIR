# hsFHIR

# steps to add new Resource

1. new module in Data/FHIR/Resources
    - data definitions
    - from/to JSON/XML
2. add resource in
    - src/Data/FHIR/Datatypes/ResourceTypes.h
    - src/Data/FHIR/Interface.hs
    - src/Data/FHIR/Resources.hs
    - src/Data/FHIR/Resources/ResourceContainer.hs
3. add resource in cabal  
