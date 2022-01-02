%%
%% ICalendar
%%
    , <<"Schedule.Agenda.Event.RRule">> => {<<"BackboneElement">>,
            [
         {<<"frequency">>, {{code, <<"rrule_freq">>}, optional}},
         {<<"byweekno ">>, {{code, <<"rrule_byweekno">>}, optional}},
         {<<"byday    ">>, [{<<"byDayNo">>, {{primitive, integer}, optional}},
                            {<<"byDayWeekDay">>, {{code, <<"weekday">>}, optional}}]},
         {<<"count    ">>, {{primitive, non_neg_integer}, optional}}
            ],
            [],
            []
} 

    , <<"Schedule.Agenda.Event">> => {<<"BackboneElement">>,
            [
         {<<"name">>, {{primitive, <<"string">>}, required}},
         {<<"description">>, {{primitive, <<"string">>}, optional}},
         {<<"type">>, {{code, <<"event_type">>}, optional}},
         {<<"location">>, {{special, <<"Reference">>}, optional}},
         {<<"period">>, {{complex, <<"Period">>}, required}},
         {<<"note">>, {{primitive, <<"string">>}, optional},
         {<<"rrule">>, {{bbelement, <<"Schedule.Agenda.Event.RRule">>}, optional}},
         {<<"rdate">>, {{bbelement, <<"Schedule.Agenda.Event.RDdate">>}, list}}, 
         {<<"exdate">>, {{bbelement, <<"Schedule.Agenda.Event.ExDate">>}, list}}
            ],
            [],
            []
} 

    , <<"Schedule.Agenda">> => {<<"BackboneElement">>,
            [
         {<<"period">>, {{complex, <<"Period">>}, required}},
         {<<"overbookable">>,{{primitive, <<"boolean">>}, required}},
         {<<"parallelPerHour">>, {{code, <<"agenda_parallelPerHour">>}, optional}},
         {<<"blocking">>,{{primitive, <<"boolean">>}, required}},
         {<<"note">>, {{primitive, <<"string">>}, optional},
         {<<"event">>, {{bbelement, <<"Schedule.Agenda.Event">>}, list}}
            ],
            [],
            []
} 

    , <<"Schedule.Timing">> => {<<"BackboneElement">>,
            [
         {<<"pre">>, {{code, <<"timing_duration">>}, required}}, 
         {<<"exam">>, {{code, <<"timing_duration">>}, required}},
         {<<"post">>, {{code, <<"timing_duration">>}, required}},
         {<<"overbookable">>, {{primitive, <<"boolean">>}, required}},
         {<<"parallelPerHour">>, {{code, <<"agenda_parallelPerHour">>}, optional}},
         {<<"blocking">>,{{primitive, <<"boolean">>}, optional}},
         {<<"prio">>, {{code, <<"priority">>}, required}}
            ],
            [],
            []
} 

    , <<"Schedule.CSS">> => {<<"BackboneElement">>,
            [
         {<<"className">>, {{primitive, <<"string">>}, required},
         {<<"backgroundColor">>, {{primitive, <<"string">>}, required},
         {<<"textColor">>, {{primitive, <<"string">>}, required},
         {<<"editable">>,{{primitive, <<"boolean">>}, required}},
            ],
            [],
            []
} 

    , <<"Schedule">> => {<<"DomainResource">>,
            [
         {<<"'identifier'">>, {{complex, <<"Identifier">>}, list}},
         {<<"active">>,{{primitive, <<"boolean">>}, required}},
         {<<"type">>, {{primitive, <<"string">>}, required},
         {<<"name">>, {{primitive, <<"string">>}, required},
         {<<"description">>, {{primitive, <<"string">>}, optional},
         {<<"fc">>, {{bbelement, <<"Schedule.CSS">>}, required}}, 
         {<<"ff">>,{{primitive, <<"boolean">>}, required}},
         {<<"timing">>, {{bbelement, <<"Schedule.Timing">>}, required}},
         {<<"location">>, {{special, <<"Reference">>}, optional}},
         {<<"agenda">>, {{bbelement, <<"Schedule.Agenda">>}, list}},
            ],
            [],
            []
} 

    , <<"ICalendar.Schedule">> => {<<"BackboneElement">>,
            [
         {<<"global">>, {{special, <<"Reference">>}, required}},
         {<<"timing">>, {{bbelement, <<"Schedule.Timing">>}, required}},
         {<<"agenda">>, {{bbelement, <<"Schedule.Agenda">>}, list}},
            ],
            [],
            []
} 

    , <<"ICalendar">> => {<<"DomainResource">>,
            [
         {<<"'identifier'">>, {{complex, <<"Identifier">>}, list}},
         {<<"active">>,{{primitive, <<"boolean">>}, required}},
         {<<"owner">>, {{special, <<"Reference">>}, required}},
         {<<"cutype">>, {{complex, <<"Coding">>}, required}},
         {<<"caltype">>, {{complex, <<"Coding">>}, required}},
         {<<"summary">>, {{primitive, <<"string">>}, optional},
         {<<"description">>, {{primitive, <<"string">>}, optional},
         {<<"timezone">>, {code, <<"ICalendar_timezone">>}, required}},
         {<<"location">>, {{special, <<"Reference">>}, optional}},
         {<<"schedule">>, {{bbelement, <<"ICalendar.Schedule">>}, list}},
            ],
            [],
            []
} 


