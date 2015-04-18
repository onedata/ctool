%% ===================================================================
%% @author Lukasz Opiola
%% @copyright (C): 2014 ACK CYFRONET AGH
%% This software is released under the MIT license
%% cited in 'LICENSE.txt'.
%% @end
%% ===================================================================
%% @doc: This module handles DNS protocol requests (TCP and UDP) and allows using
%% custom query handlers.
%% @end
%% ===================================================================
-module(dns_server).

-include("logging.hrl").
-include_lib("kernel/src/inet_dns.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

% Local name of the process waiting for dns udp messages
-define(DNS_UDP_LISTENER, dns_udp).

% Module name of dns tcp ranch listener
-define(DNS_TCP_LISTENER, dns_tcp).

% Timeout to wait for DNS listeners to start. After it, they are assumed to have failed to start.
-define(LISTENERS_START_TIMEOUT, 10000).

% Maximum size of UDP DNS reply (without EDNS) as in RFC1035.
-define(NOEDNS_UDP_SIZE, 512).

% Allowed query types
-type dns_query_type() :: ?S_A | ?S_NS | ?S_CNAME | ?S_SOA | ?S_WKS | ?S_PTR | ?S_HINFO | ?S_MINFO | ?S_MX | ?S_TXT.

% Atoms for reply codes for cenvenience
-type reply_type() :: ok | serv_fail | nx_domain | not_impl | refused | form_error | bad_version.

% Records used internally to pass query results from handler to server, which then puts them in response.
-type answer_record() :: {answer, Type :: dns_query_type(), Data :: term()}.
-type authority_record() :: {authority, Type :: dns_query_type(), Data :: term()}.
-type additional_record() :: {additional, Type :: dns_query_type(), Data :: term()}.
-type authoritative_answer_flag() :: {aa, Flag :: boolean()}.
-type dns_query_handler_reponse() :: [answer_record() | authority_record () | additional_record() | authoritative_answer_flag()].

-export_type([reply_type/0, dns_query_handler_reponse/0]).

%% Data types that should be returned for specific types of queries:
%% -----------------------------------------
%% Type A, identified by macro ?S_A (RFC1035 3.4.1)
%% Data :: {A :: byte(), B :: byte(), C :: byte(), D :: byte()}
%% IPv4 address(es) - IP address(es) of servers:
%% {A, B, C, D}: Bytes of IPv4 address

%% -----------------------------------------
%% Type NS, identified by macro ?S_NS (RFC1035 3.3.11)
%% Data :: string()
%% A <domain-name> which specifies a host which should be authoritative for the specified class and domain.

%% -----------------------------------------
%% Type CNAME, identified by macro ?S_CNAME (RFC1035 3.3.1)
%% Data :: string()
%% A <domain-name> which specifies the canonical or primary name for the owner.  The owner name is an alias.

%% -----------------------------------------
%% Type SOA, identified by macro ?S_SOA (RFC1035 3.3.13)
%% Data :: {MName :: string() , RName:: string(), Serial :: integer(), Refresh :: integer(), Retry :: integer(), Expiry :: integer(), Minimum :: integer()}
%% MName: The <domain-name> of the name server that was the original or primary source of data for this zone.
%% RName: A <domain-name> which specifies the mailbox of the person responsible for this zone.
%% Serial: The unsigned 32 bit version number of the original copy of the zone.  Zone transfers preserve this value.
%%    This value wraps and should be compared using sequence space arithmetic.
%% Refresh: A 32 bit time interval before the zone should be refreshed.
%% Retry: A 32 bit time interval that should elapse before a failed refresh should be retried.
%% Expiry: A 32 bit time value that specifies the upper limit on the time interval that can elapse before the zone
%%   is no longer authoritative.
%% Minimum: The unsigned 32 bit minimum TTL field that should be exported with any RR from this zone.

%% -----------------------------------------
%% Type WKS, identified by macro ?S_WKS (RFC1035 3.4.2)
%% Data :: {{A :: byte(), B :: byte(), C :: byte(), D :: byte()}, Proto :: string(), BitMap :: string()}
%% {A, B, C, D}: Bytes of IPv4 address
%% Proto: An 8 bit IP protocol number
%% BitMap: A variable length bit map. The bit map must be a multiple of 8 bits long.
%%   A positive bit means, that a port of number equal to its position in bitmap is open.
%%   BitMap representation as string in erlang is problematic. Example of usage:
%%   BitMap = [2#11111111, 2#00000000, 2#00000011]
%%   above bitmap will inform the client, that ports: 0 1 2 3 4 5 6 7 22 23 are open.

%% -----------------------------------------
%% Type PTR, identified by macro ?S_PTR (RFC1035 3.3.12)
%% Data :: string()
%% <domain-name> which points to some location in the domain name space.

%% -----------------------------------------
%% Type HINFO, identified by macro ?S_HINFO (RFC1035 3.3.2)
%% Data :: {CPU :: string(), OS :: string()}
%% CPU: A <character-string> which specifies the CPU type.
%% OS: A <character-string> which specifies the operatings system type.

%% -----------------------------------------
%% Type MINFO, identified by macro ?S_MINFO (RFC1035 3.3.7)
%% Data :: {RM :: string(), EM :: string()}
%% RM: A <domain-name> which specifies a mailbox which is responsible for the mailing list or mailbox.  If this
%%   domain name names the root, the owner of the MINFO RR is responsible for itself.  Note that many existing mailing
%%   lists use a mailbox X-request for the RMAILBX field of mailing list X, e.g., Msgroup-request for Msgroup.  This
%%   field provides a more general mechanism.
%% EM: A <domain-name> which specifies a mailbox which is to receive error messages related to the mailing list or
%%   mailbox specified by the owner of the MINFO RR (similar to the ERRORS-TO: field which has been proposed).  If
%%   this domain name names the root, errors should be returned to the sender of the message.
%% -----------------------------------------

%% Type MX, identified by macro ?S_MX (RFC1035 3.3.9)
%% Data :: {Pref :: integer(), Exch :: string()}
%% Pref: A 16 bit integer which specifies the preference given to this RR among others at the same owner.
%%   Lower values are preferred.
%% Exch: A <domain-name> which specifies a host willing to act as a mail exchange for the owner name.
%% -----------------------------------------

%% Type TXT, identified by macro ?S_TXT (RFC1035 3.3.14)
%% text data - one or more <character-string>s:
%% TXT RRs are used to hold descriptive text. The semantics of the text depends on the domain where it is found.
%% NOTE: Should return list of strings for every record, for example:
%% [["string_1_1", "string_1_2"], ["string_2_1", "string_2_2"]] - two records, each with two strings.
%% -----------------------------------------

%% ====================================================================
%% API
%% ====================================================================
-export([start/7, stop/1, handle_query/2, start_listening/5]).

% Functions useful in qury handler modules
-export([answer_record/4, authority_record/4, additional_record/4,
    authoritative_answer_flag/1, validate_query/1]).

% Server configuration (in runtime)
-export([set_handler_module/1, get_handler_module/0, set_max_edns_udp_size/1,
    get_max_edns_udp_size/0]).

%% start/7
%% ====================================================================
%% @doc Starts a DNS server. The server will listen on chosen port (UDP and TCP).
%% QueryHandlerModule must conform to dns_query_handler_behaviour.
%% The server is started in a new process.
%% OnFailureFun is evalueated by the process if the server fails to start.
%% @end
%% ====================================================================
-spec start(SupervisorName :: atom(), DNSPort :: integer(), QueryHandlerModule :: atom(), EdnsMaxUdpSize :: integer(),
    TCPNumAcceptors :: integer(), TCPTimeout :: integer(), OnFailureFun :: function()) -> ok | {error, Reason :: term()}.
%% ====================================================================
start(SupervisorName, DNSPort, QueryHandlerModule, EdnsMaxUdpSize, TCPNumAcceptors, TCPTimeout, OnFailureFun) ->
    set_handler_module(QueryHandlerModule),
    set_max_edns_udp_size(EdnsMaxUdpSize),
    % Listeners start is done in another process.
    % This is because this function is often called during the init of the supervisor process
    % that supervises the listeners - which causes a deadlock.
    case proc_lib:start(?MODULE, start_listening, [SupervisorName, DNSPort, TCPNumAcceptors, TCPTimeout, OnFailureFun], ?LISTENERS_START_TIMEOUT) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% stop/1
%% ====================================================================
%% @doc Stops the DNS server and cleans up.
%% @end
%% ====================================================================
-spec stop(SupervisorName :: atom()) -> ok.
%% ====================================================================
stop(SupervisorName) ->
    % Cleaning up must be done in another process. This is because this function is often called during the termination
    % of the supervisor process that supervises the listeners - which causes a deadlock.
    spawn(fun() -> clear_children_and_listeners(SupervisorName) end),
    ok.

%% handle_query/2
%% ====================================================================
%% @doc Handles a DNS request. This function is called from dns_upd_handler or dns_tcp_handler after
%% they have received a request. After evaluation, the response is sent back to the client.
%% @end
%% ====================================================================
-spec handle_query(Packet :: binary(), Transport :: udp | tcp) -> {ok, Packet :: binary()} | {error, term()}.
%% ====================================================================
handle_query(Packet, Transport) ->
    case inet_dns:decode(Packet) of
        {ok, #dns_rec{qdlist = QDList, anlist = _AnList, nslist = _NSList, arlist = ARList} = DNSRecWithAdditionalSection} ->
            % Detach OPT RR from the DNS query record an proceed with processing it - the OPT RR will be added during answer generation
            DNSRec = DNSRecWithAdditionalSection#dns_rec{arlist = []},
            OPTRR = case ARList of
                        [] -> undefined;
                        [#dns_rr_opt{} = OptRR] -> OptRR
                    end,
            case validate_query(DNSRec) of
                ok ->
                    HandlerModule = get_handler_module(),
                    [#dns_query{domain = Domain, type = Type, class = Class}] = QDList,
                    case call_handler_module(HandlerModule, string:to_lower(Domain), Type) of
                        Reply when is_atom(Reply) -> % Reply :: reply_type()
                            generate_answer(set_reply_code(DNSRec, Reply), OPTRR, Transport);
                        {Reply, ResponseList} ->
                            DNSRecUpdatedHeader = set_reply_code(DNSRec, Reply),
                            %% If there was an OPT RR, it will be concated at the end of the process
                            NewRec = lists:foldl(
                                fun(CurrentRecord, #dns_rec{header = CurrHeader, anlist = CurrAnList, nslist = CurrNSList, arlist = CurrArList} = CurrRec) ->
                                    case CurrentRecord of
                                        {aa, Flag} ->
                                            CurrRec#dns_rec{header = inet_dns:make_header(CurrHeader, aa, Flag)};
                                        {Section, CurrDomain, CurrTTL, CurrType, CurrData} ->
                                            RR = inet_dns:make_rr([
                                                {data, CurrData},
                                                {domain, CurrDomain},
                                                {type, CurrType},
                                                {ttl, CurrTTL},
                                                {class, Class}]),
                                            case Section of
                                                answer ->
                                                    CurrRec#dns_rec{anlist = CurrAnList ++ [RR]};
                                                authority ->
                                                    CurrRec#dns_rec{nslist = CurrNSList ++ [RR]};
                                                additional ->
                                                    CurrRec#dns_rec{arlist = CurrArList ++ [RR]}
                                            end
                                    end
                                end, DNSRecUpdatedHeader#dns_rec{}, ResponseList),
                            generate_answer(NewRec#dns_rec{arlist = NewRec#dns_rec.arlist}, OPTRR, Transport)
                    end;
                Error ->
                    generate_answer(set_reply_code(DNSRec, Error), OPTRR, Transport)
            end;
        _ ->
            {error, uprocessable}
    end.

%% validate_query/1
%% ====================================================================
%% @doc Checks if a query is valid.
%% @end
%% ====================================================================
-spec validate_query(#dns_rec{}) -> ok | form_error.
%% ====================================================================
validate_query(DNSRec) ->
    case DNSRec of
        #dns_rec{qdlist = [#dns_query{class = Class}], anlist = [], nslist = [], arlist = []}
            when Class =:= ?C_IN orelse Class =:= in ->
            % The record includes a question section and no OPT RR section - ok
            ok;
        #dns_rec{qdlist = [#dns_query{class = Class}], anlist = [], nslist = [], arlist = [#dns_rr_opt{version = Version}]}
            when Class =:= ?C_IN orelse Class =:= in ->
            % The record includes a question section and an OPT RR section - check EDNS version
            case Version of
                0 -> ok;
                _ -> bad_version
            end;
        #dns_rec{} ->
            % Other - not ok
            form_error
    end.

%% call_handler_module/3
%% ====================================================================
%% @doc Calls the handler module to handle the query or returns not_impl if
%% this kind of query is not accepted by the server.
%% @end
%% ====================================================================
-spec call_handler_module(HandlerModule :: module(), Domain :: string(), Type :: atom()) ->
    term().
%% ====================================================================
call_handler_module(HandlerModule, Domain, Type) ->
    case type_to_fun(Type) of
        not_impl ->
            not_impl;
        Fun ->
            erlang:apply(HandlerModule, Fun, [Domain])
    end.

%% type_to_fun/1
%% ====================================================================
%% @doc Returns a function that should be called to handle a query of given type.
%% Those functions are defined in dns_query_handler_behaviour.
%% @end
%% ====================================================================
-spec type_to_fun(QueryType :: atom()) -> atom().
%% ====================================================================
type_to_fun(?S_A) -> handle_a;
type_to_fun(?S_NS) -> handle_ns;
type_to_fun(?S_CNAME) -> handle_cname;
type_to_fun(?S_SOA) -> handle_soa;
type_to_fun(?S_WKS) -> handle_wks;
type_to_fun(?S_PTR) -> handle_ptr;
type_to_fun(?S_HINFO) -> handle_hinfo;
type_to_fun(?S_MINFO) -> handle_minfo;
type_to_fun(?S_MX) -> handle_mx;
type_to_fun(?S_TXT) -> handle_txt;
type_to_fun(_) -> not_impl.

%% generate_answer/3
%% ====================================================================
%% @doc Encodes a DNS record and returns a tuple accepted by dns_xxx_handler modules.
%% Modifies flags in header according to server's capabilities.
%% If there was an OPT RR record in request, it modifies it properly and concates to the ADDITIONAL section.
%% @end
%% ====================================================================
-spec generate_answer(DNSRec :: #dns_rec{}, OPTRR :: #dns_rr_opt{}, Transport :: atom()) -> {ok, binary()}.
%% ====================================================================
generate_answer(DNSRec, OPTRR, Transport) ->
    % Update the header - no recursion available, qr=true -> it's a response
    Header2 = inet_dns:make_header(DNSRec#dns_rec.header, ra, false),
    NewHeader = inet_dns:make_header(Header2, qr, true),
    DNSRecUpdatedHeader = DNSRec#dns_rec{header = NewHeader},
    % Check if OPT RR is present. If so, retrieve client's max upd payload size and set the value to server's max udp.
    {NewDnsRec, ClientMaxUDP} = case OPTRR of
                                    undefined ->
                                        {DNSRecUpdatedHeader, undefined};
                                    #dns_rr_opt{udp_payload_size = ClMaxUDP} = RROPT ->
                                        NewRROPT = RROPT#dns_rr_opt{udp_payload_size = get_max_edns_udp_size()},
                                        {DNSRecUpdatedHeader#dns_rec{arlist = DNSRecUpdatedHeader#dns_rec.arlist ++ [NewRROPT]}, ClMaxUDP}
                                end,
    case Transport of
        udp -> {ok, encode_udp(NewDnsRec, ClientMaxUDP)};
        tcp -> {ok, inet_dns:encode(NewDnsRec)}
    end.

%% set_reply_code/2
%% ====================================================================
%% @doc Encodes a DNS record and returns a tuple accepted by dns_xxx_handler modules.
%% Modifies flags in header according to server's capabilities.
%% If there was an OPT RR record in request, it modifies it properly and concates to the ADDITIONAL section.
%% @end
%% ====================================================================
-spec set_reply_code(DNSRec :: #dns_rec{}, ReplyType :: term()) -> #dns_rec{}.
%% ====================================================================
set_reply_code(#dns_rec{header = Header} = DNSRec, ReplyType) ->
    ReplyCode = case ReplyType of
                    nx_domain -> ?NXDOMAIN;
                    not_impl -> ?NOTIMP;
                    refused -> ?REFUSED;
                    form_error -> ?FORMERR;
                    bad_version -> ?BADVERS;
                    ok -> ?NOERROR;
                    _ -> ?SERVFAIL
                end,
    DNSRec#dns_rec{header = inet_dns:make_header(Header, rcode, ReplyCode)}.

%% encode_udp/2
%% ====================================================================
%% @doc Encodes a DNS record and truncates it if required.
%% @end
%% ====================================================================
-spec encode_udp(DNSRec :: #dns_rec{}, ClientMaxUDP :: integer() | undefined) -> binary().
%% ====================================================================
encode_udp(#dns_rec{} = DNSRec, ClientMaxUDP) ->
    TruncationSize = case ClientMaxUDP of
                         undefined ->
                             ?NOEDNS_UDP_SIZE;
                         Value ->
                             % If the client advertised a value, accept it but don't exceed the range [512, MAX_UDP_SIZE]
                             max(?NOEDNS_UDP_SIZE, min(get_max_edns_udp_size(), Value))
                     end,
    Packet = inet_dns:encode(DNSRec),
    case size(Packet) > TruncationSize of
        true ->
            NumBits = (8 * TruncationSize),
            % Truncate the packet
            <<TruncatedPacket:NumBits, _/binary>> = Packet,
            % Set the TC (truncation) flag, which is the 23th bit in header
            <<Head:22, _TC:1, LastBit:1, Tail/binary>> = <<TruncatedPacket:NumBits>>,
            NewPacket = <<Head:22, 1:1, LastBit:1, Tail/binary>>,
            NewPacket;
        false ->
            Packet
    end.

%% start_listening/5
%% ====================================================================
%% @doc Starts dns listeners and terminates dns_worker process in case of error.
%% OnFailureFun is evalueated if the server fails to start.
%% @end
%% ====================================================================
-spec start_listening(SupervisorName :: atom(), DNSPort :: integer(),
    TCPNumAcceptors :: integer(), TCPTimeout :: integer(), OnFailureFun :: function()) -> ok.
%% ====================================================================
start_listening(SupervisorName, DNSPort, TCPNumAcceptors, TCPTimeout, OnFailureFun) ->
    try
        proc_lib:init_ack(ok),
        UDPChild = {?DNS_UDP_LISTENER, {dns_udp_handler, start_link, [DNSPort]}, permanent, 5000, worker, [dns_udp_handler]},
        TCPOptions = [{packet, 2}, {dns_tcp_timeout, TCPTimeout}, {keepalive, true}],

        % Start the UDP listener
        {ok, Pid} = supervisor:start_child(SupervisorName, UDPChild),
        % Start the TCP listener. In case of an error, stop the UDP listener
        try
            {ok, _} = ranch:start_listener(?DNS_TCP_LISTENER, TCPNumAcceptors, ranch_tcp, [{port, DNSPort}],
                dns_tcp_handler, TCPOptions)
        catch
            _:RanchError ->
                supervisor:terminate_child(SupervisorName, Pid),
                supervisor:delete_child(SupervisorName, Pid),
                ?error("Error while starting DNS TCP, ~p", [RanchError]),
                throw(RanchError)
        end,
        ?info("DNS server started successfully.")
    catch
        _:Reason ->
            ?error("DNS Error during starting listeners, ~p", [Reason]),
            OnFailureFun()
    end,
    ok.

%% authoritative_answer_flag/1
%% ====================================================================
%% @doc Convenience function used from a dns query handler. Creates a term that will cause
%% the AA (authoritative answer) flag to be set to desired value in response.
%% The term should be put in list returned from handle_xxx function.
%% @end
%% ====================================================================
-spec authoritative_answer_flag(Flag) -> {aa, Flag} when
    Flag :: boolean().
%% ====================================================================
authoritative_answer_flag(Flag) ->
    {aa, Flag}.

%% answer_record/3
%% ====================================================================
%% @doc Convenience function used from a dns query handler. Creates a term that will end up
%% as a record in ANSWER section of DNS reponse.
%% The term should be put in list returned from handle_xxx function.
%% @end
%% ====================================================================
-spec answer_record(Domain, TTL, Type, Data) ->
    {answer, Domain, TTL, Type, Data} when
    Domain :: string(), TTL :: integer(), Type :: dns_query_type(), Data :: term().
%% ====================================================================
answer_record(Domain, TTL, Type, Data) ->
    {answer, Domain, TTL, Type, Data}.

%% authority_record/3
%% ====================================================================
%% @doc Convenience function used from a dns query handler. Creates a term that will end up
%% as a record in AUTHORITY section of DNS reponse.
%% The term should be put in list returned from handle_xxx function.
%% @end
%% ====================================================================
-spec authority_record(Domain, TTL, Type, Data) ->
    {authority, Domain, TTL, Type, Data} when
    Domain :: string(), TTL :: integer(), Type :: dns_query_type(), Data :: term().
%% ====================================================================
authority_record(Domain, TTL, Type, Data) ->
    {authority, Domain, TTL, Type, Data}.

%% additional_record/3
%% ====================================================================
%% @doc Convenience function used from a dns query handler. Creates a term that will end up
%% as a record in ADDITIONAL section of DNS reponse.
%% The term should be put in list returned from handle_xxx function.
%% @end
%% ====================================================================
-spec additional_record(Domain, TTL, Type, Data) ->
    {additional, Domain, TTL, Type, Data} when
    Domain :: string(), TTL :: integer(), Type :: dns_query_type(), Data :: term().
%% ====================================================================
additional_record(Domain, TTL, Type, Data) ->
    {additional, Domain, TTL, Type, Data}.

%% set_handler_module/1
%% ====================================================================
%% @doc Saves query handler module in application env.
%% @end
%% ====================================================================
-spec set_handler_module(Module :: atom()) -> ok.
%% ====================================================================
set_handler_module(Module) ->
    ok = application:set_env(ctool, query_handler_module, Module).

%% get_handler_module/0
%% ====================================================================
%% @doc Retrieves query handler module from application env.
%% @end
%% ====================================================================
-spec get_handler_module() -> module().
%% ====================================================================
get_handler_module() ->
    {ok, Module} = application:get_env(ctool, query_handler_module),
    Module.

%% set_max_udp_size/1
%% ====================================================================
%% @doc Saves DNS response TTL in application env.
%% @end
%% ====================================================================
-spec set_max_edns_udp_size(Size :: integer()) -> ok.
%% ====================================================================
set_max_edns_udp_size(Size) ->
    ok = application:set_env(ctool, edns_max_udp_size, Size).

%% get_max_udp_size/0
%% ====================================================================
%% @doc Retrieves DNS response TTL from application env.
%% @end
%% ====================================================================
-spec get_max_edns_udp_size() -> integer().
%% ====================================================================
get_max_edns_udp_size() ->
    {ok, Size} = application:get_env(ctool, edns_max_udp_size),
    Size.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% clear_children_and_listeners/1
%% ====================================================================
%% @doc Terminates listeners and created children, if they exist.
%% @end
%% ====================================================================
-spec clear_children_and_listeners(SupervisorName :: atom()) -> ok.
%% ====================================================================
clear_children_and_listeners(SupervisorName) ->
    try
        SupChildren = supervisor:which_children(SupervisorName),
        case lists:keyfind(?DNS_UDP_LISTENER, 1, SupChildren) of
            {?DNS_UDP_LISTENER, _, _, _} ->
                ok = supervisor:terminate_child(SupervisorName, ?DNS_UDP_LISTENER),
                supervisor:delete_child(SupervisorName, ?DNS_UDP_LISTENER),
                ?debug("DNS UDP child has exited");
            _ ->
                ok
        end
    catch
        _:Error1 ->
            ?error_stacktrace("Error stopping dns udp listener, status ~p", [Error1])
    end,

    try
        ok = ranch:stop_listener(?DNS_TCP_LISTENER),
        ?debug("dns_tcp_listener has exited")
    catch
        _:Error2 ->
            ?error_stacktrace("Error stopping dns tcp listener, status ~p", [Error2])
    end,
    ok.