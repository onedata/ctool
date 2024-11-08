%%%-------------------------------------------------------------------
%%% @author Bartosz Walkowicz
%%% @copyright (C) 2023 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Eunit tests of automation related modules upgrades.
%%% @end
%%%-------------------------------------------------------------------
-module(automation_upgrade_tests).
-author("Bartosz Walkowicz").

-ifdef(TEST).

-include("test/test_utils.hrl").
-include("automation/automation.hrl").
-include("onedata_file.hrl").


upgrade_db_data_spec_test_() ->
    U = fun(JsonData) ->
        persistent_record:from_string(json_utils:encode(JsonData), atm_data_spec)
    end,

    [
        ?_assertEqual(
            #atm_array_data_spec{
                item_data_spec = #atm_number_data_spec{
                    integers_only = false,
                    allowed_values = undefined
                }
            },
            U(#{
                <<"_data">> => #{
                    <<"type">> => <<"array">>,
                    <<"valueConstraints">> => #{
                        <<"itemDataSpec">> => #{
                            <<"_data">> => #{
                                <<"type">> => <<"number">>,
                                <<"valueConstraints">> => #{
                                    <<"allowedValues">> => null,
                                    <<"integersOnly">> => false
                                }
                            },
                            <<"_version">> => 1
                        }
                    }
                },
                <<"_version">> => 1
            })
        ),

        ?_assertEqual(
            #atm_boolean_data_spec{},
            U(#{
                <<"_data">> => #{
                    <<"type">> => <<"boolean">>,
                    <<"valueConstraints">> => #{}
                },
                <<"_version">> => 1
            })
        ),

        ?_assertEqual(
            #atm_dataset_data_spec{},
            U(#{
                <<"_data">> => #{
                    <<"type">> => <<"dataset">>,
                    <<"valueConstraints">> => #{}
                },
                <<"_version">> => 1
            })
        ),

        % On the API level, the version without the attributes field or with older values is no
        % longer accepted. However, to ensure that older schemas can be loaded from the DB,
        % it is still retained there (Attributes default to all possible values).
        ?_assertEqual(
            #atm_file_data_spec{
                file_type = ?REGULAR_FILE_TYPE,
                %% @TODO VFS-12091 include all attrs after atm versioning is introduced
                attributes = lists:usort(?API_FILE_ATTRS -- [?attr_creation_time])
            },
            U(#{
                <<"_data">> => #{
                    <<"type">> => <<"file">>,
                    <<"valueConstraints">> => #{
                        <<"fileType">> => <<"REG">>
                    }
                },
                <<"_version">> => 1
            })
        ),
        ?_assertEqual(
            #atm_file_data_spec{
                file_type = ?REGULAR_FILE_TYPE,
                %% @TODO VFS-12091 include all attrs after atm versioning is introduced
                attributes = lists:usort(?API_FILE_ATTRS -- [?attr_creation_time])
            },
            U(#{
                <<"_data">> => #{
                    <<"type">> => <<"file">>,
                    <<"valueConstraints">> => #{
                        <<"fileType">> => <<"REG">>,
                        <<"attrbiutes">> => [<<"file_id">>, <<"storage_user_id">>]
                    }
                },
                <<"_version">> => 1
            })
        ),

        ?_assertEqual(
            #atm_number_data_spec{
                integers_only = true,
                allowed_values = undefined
            },
            U(#{
                <<"_data">> => #{
                    <<"type">> => <<"number">>,
                    <<"valueConstraints">> => #{
                        <<"allowedValues">> => null,
                        <<"integersOnly">> => true
                    }
                },
                <<"_version">> => 1
            })
        ),

        ?_assertEqual(
            #atm_object_data_spec{},
            U(#{
                <<"_data">> => #{
                    <<"type">> => <<"object">>,
                    <<"valueConstraints">> => #{}
                },
                <<"_version">> => 1
            })
        ),

        ?_assertEqual(
            #atm_range_data_spec{},
            U(#{
                <<"_data">> => #{
                    <<"type">> => <<"range">>,
                    <<"valueConstraints">> => #{}
                },
                <<"_version">> => 1
            })
        ),

        ?_assertEqual(
            #atm_string_data_spec{allowed_values = [<<"a">>, <<"b">>]},
            U(#{
                <<"_data">> => #{
                    <<"type">> => <<"string">>,
                    <<"valueConstraints">> => #{
                        <<"allowedValues">> => [<<"a">>,<<"b">>]
                    }
                },
                <<"_version">> => 1
            })
        ),

        ?_assertEqual(
            #atm_time_series_measurement_data_spec{specs = [
                #atm_time_series_measurement_spec{
                    name_matcher_type = has_prefix,
                    name_matcher = <<"latency">>,
                    unit = milliseconds
                }
            ]},
            U(#{
                <<"_data">> => #{
                    <<"type">> => <<"timeSeriesMeasurement">>,
                    <<"valueConstraints">> => #{
                        <<"specs">> => [
                            #{
                                <<"_data">> => #{
                                    <<"nameMatcher">> => <<"latency">>,
                                    <<"nameMatcherType">> => <<"hasPrefix">>,
                                    <<"unit">> => <<"milliseconds">>
                                },
                                <<"_version">> => 1
                            }
                        ]
                    }
                },
                <<"_version">> => 1
            })
        )
    ].


load_deprecated_json_data_spec_test_() ->
    U = fun(JsonData) ->
        jsonable_record:from_json(JsonData, atm_data_spec)
    end,

    [
        ?_assertEqual(
            #atm_array_data_spec{
                item_data_spec = #atm_number_data_spec{
                    integers_only = false,
                    allowed_values = undefined
                }
            },
            U(#{
                <<"type">> => <<"array">>,
                <<"valueConstraints">> => #{
                    <<"itemDataSpec">> => #{
                        <<"type">> => <<"number">>,
                        <<"valueConstraints">> => #{
                            <<"allowedValues">> => null,
                            <<"integersOnly">> => false
                        }
                    }
                }
            })
        ),

        ?_assertEqual(
            #atm_boolean_data_spec{},
            U(#{
                <<"type">> => <<"boolean">>,
                <<"valueConstraints">> => #{}
            })
        ),

        ?_assertEqual(
            #atm_dataset_data_spec{},
            U(#{
                <<"type">> => <<"dataset">>,
                <<"valueConstraints">> => #{}
            })
        ),

        % On the API level, the version without the attributes field or with older values is no
        % longer accepted. However, to ensure that older schemas can be loaded from the DB,
        % it is still retained there (see upgrade_db_data_spec_test_)
        ?_assertException(
            error, {badkey,<<"attributes">>},
            U(#{
                <<"type">> => <<"file">>,
                <<"valueConstraints">> => #{
                    <<"fileType">> => <<"REG">>
                }
            })
        ),
        ?_assertException(
            throw, ?ERROR_BAD_VALUE_NOT_ALLOWED(<<"attributes">>, _),
            U(#{
                <<"type">> => <<"file">>,
                <<"valueConstraints">> => #{
                    <<"fileType">> => <<"REG">>,
                    <<"attributes">> => [<<"file_id">>]
                }
            })
        ),

        ?_assertEqual(
            #atm_number_data_spec{
                integers_only = true,
                allowed_values = undefined
            },
            U(#{
                <<"type">> => <<"number">>,
                <<"valueConstraints">> => #{
                    <<"allowedValues">> => null,
                    <<"integersOnly">> => true
                }
            })
        ),

        ?_assertEqual(
            #atm_object_data_spec{},
            U(#{
                <<"type">> => <<"object">>,
                <<"valueConstraints">> => #{}
            })
        ),

        ?_assertEqual(
            #atm_range_data_spec{},
            U(#{
                <<"type">> => <<"range">>,
                <<"valueConstraints">> => #{}
            })
        ),

        ?_assertEqual(
            #atm_string_data_spec{allowed_values = [<<"a">>, <<"b">>]},
            U(#{
                <<"type">> => <<"string">>,
                <<"valueConstraints">> => #{
                    <<"allowedValues">> => [<<"a">>,<<"b">>]
                }
            })
        ),

        ?_assertEqual(
            #atm_time_series_measurement_data_spec{specs = [
                #atm_time_series_measurement_spec{
                    name_matcher_type = has_prefix,
                    name_matcher = <<"latency">>,
                    unit = milliseconds
                }
            ]},
            U(#{
                <<"type">> => <<"timeSeriesMeasurement">>,
                <<"valueConstraints">> => #{
                    <<"specs">> => [
                        #{
                            <<"nameMatcher">> => <<"latency">>,
                            <<"nameMatcherType">> => <<"hasPrefix">>,
                            <<"unit">> => <<"milliseconds">>
                        }
                    ]
                }
            })
        )
    ].


-endif.