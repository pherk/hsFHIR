-ifndef(complexcodes).

-type quantityComparator ::
     '<'
   | '<::'
   | '>::'
   | '>'.

-type identifierUse ::
     'usual'
   | 'official'
   | 'temp'
   | 'secondary'.

-type humanNameUse ::
     'usual'
   | 'official'
   | 'temp'
   | 'nickname'
   | 'anonymous'
   | 'old'
   | 'maiden'.

-type addressUse ::
     'home'
   | 'work'
   | 'temp'
   | 'old'.

-type contactPointUse ::
     'home'
   | 'work'
   | 'temp'
   | 'old'
   | 'mobile'.

-type contactPointSystem ::
     'phone'
   | 'fax'
   | 'email'
   | 'pager'
   | 'url'
   | 'sms'
   | 'other'.

-type ucum_time ::
     's'
   | 'min'
   | 'h'
   | 'd'
   | 'wk'
   | 'mo'
   | 'a'.

-type timeCode ::
     'BID'
   | 'TID'
   | 'QID'
   | 'AM'
   | 'PM'
   | 'QD'
   | 'QOD'
   | 'Q4H'
   | 'Q6H'.

-endif.
