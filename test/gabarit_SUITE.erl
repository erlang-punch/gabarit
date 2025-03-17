%%%-------------------------------------------------------------------
%%% @author Maartz <maartz@protonmail.com>
%%%
%%% @doc Test suite for the gabarit template management system.
%%%
%%% This test suite verifies the core functionality of the gabarit template
%%% management system, including:
%%%  - Basic template operations (add, get, update)
%%%  - Namespace management and limits
%%%  - Template versioning and version access
%%%  - Content compression for large templates
%%%  - Enforcement of namespace template limits
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gabarit_SUITE).

-export([
    all/0,
    suite/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    common/1,
    namespace_test/1,
    template_versioning_test/1,
    template_compression_test/1,
    template_limit_test/1,
    default_namespace_test/1,
    list_all_templates_test/1,
    namespace_options_test/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Common Test Callbacks
%%====================================================================

%% @doc Returns the test suite properties.
%% @end
suite() ->
    [{timetrap, {seconds, 30}}].

%% @doc Initializes the test suite environment.
%%      Starts the gabarit application without configuration.
%% @end
init_per_suite(Config) ->
    application:start(gabarit),
    Config.

%% @doc Cleans up after test suite completion.
%%      Stops the gabarit application.
%% @end
end_per_suite(_Config) ->
    application:stop(gabarit),
    ok.

%% @doc Initialization before each test group.
%% @end
init_per_group(_GroupName, Config) ->
    Config.

%% @doc Cleanup after each test group.
%% @end
end_per_group(_GroupName, _Config) ->
    ok.

%% @doc Initialization before each test case.
%% @end
init_per_testcase(_TestCase, Config) ->
    Config.

%% @doc Cleanup after each test case.
%% @end
end_per_testcase(_TestCase, _Config) ->
    ok.

%% @doc Returns test groups and their associated properties.
%% @end
groups() ->
    [].

%% @doc Returns the list of test cases to run.
%% @end
all() ->
    [
        common,
        namespace_test,
        template_versioning_test,
        template_compression_test,
        template_limit_test,
        default_namespace_test,
        list_all_templates_test,
        namespace_options_test
    ].

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Tests basic template operations.
%%      Verifies that templates can be created and retrieved.
%% @end
common(_Config) ->
    ok = gabarit:create_namespace("test_default", #{limit => 10}),

    OriginalContent = <<"<body>{{content}}</body>">>,
    {ok, TemplateId} = gabarit:add_template("test_default", "test_template", OriginalContent),

    {ok, RetrievedContent} = gabarit:get_template(TemplateId),
    ?assertEqual(OriginalContent, RetrievedContent),
    ok.

%% @doc Tests namespace management functionality.
%%      Verifies that namespaces can be created and listed.
%% @end
namespace_test(_Config) ->
    ok = gabarit:create_namespace("test_ns", #{limit => 5}),

    {ok, Namespaces} = gabarit:list_namespaces(),
    ?assert(lists:keymember("test_ns", 1, Namespaces)),

    % Verify the correct limit was set
    {value, {"test_ns", 5}} = lists:keysearch("test_ns", 1, Namespaces),
    ok.

%% @doc Tests template versioning functionality.
%%      Verifies that multiple versions of a template can be
%%      created, accessed, and compared.
%% @end
template_versioning_test(_Config) ->
    ok = gabarit:create_namespace("version_ns", #{limit => 10}),

    OriginalContent = <<"<body>{{content}}</body>">>,
    {ok, TemplateId} = gabarit:add_template("version_ns", "test_template", OriginalContent),

    {ok, Content1} = gabarit:get_template(TemplateId),
    ?assertEqual(OriginalContent, Content1),

    {ok, Version1Content} = gabarit:get_template_version(TemplateId, 1),
    ?assertEqual(OriginalContent, Version1Content),

    NewContent = <<"<html><body>{{content}}</body></html>">>,
    {ok, _} = gabarit:update_template(TemplateId, NewContent),

    {ok, Content2} = gabarit:get_template(TemplateId),
    ?assertEqual(NewContent, Content2),

    {ok, Version1Content2} = gabarit:get_template_version(TemplateId, 1),
    ?assertEqual(OriginalContent, Version1Content2),

    {ok, Version2Content} = gabarit:get_template_version(TemplateId, 2),
    ?assertEqual(NewContent, Version2Content),

    ok.

%% @doc Tests content compression functionality.
%%      Verifies that large template content is properly compressed
%%      during storage and decompressed during retrieval.
%% @end
template_compression_test(_Config) ->
    ok = gabarit:create_namespace("compress_ns", #{limit => 10}),

    LargeContent = list_to_binary(lists:duplicate(20000, $a)),
    {ok, CompressTemplateId} = gabarit:add_template("compress_ns", "large_template", LargeContent),

    {ok, RetrievedContent} = gabarit:get_template(CompressTemplateId),
    ?assertEqual(LargeContent, RetrievedContent),

    ok.

%% @doc Tests namespace template limits.
%%      Verifies that the system enforces the maximum number of templates
%%      allowed per namespace.
%% @end
template_limit_test(_Config) ->
    ok = gabarit:create_namespace("limit_ns", #{limit => 2}),

    {ok, _} = gabarit:add_template("limit_ns", "template1", <<"content1">>),
    {ok, _} = gabarit:add_template("limit_ns", "template2", <<"content2">>),

    ?assertEqual({error, namespace_limit_reached},
                gabarit:add_template("limit_ns", "template3", <<"content3">>)),

    ok.

%% @doc Tests the default namespace functionality.
%%      Verifies that the default namespace exists and can be used
%%      without explicit creation.
%% @end
default_namespace_test(_Config) ->
    ok = gabarit:reset_namespace(gabarit, 10),

    {ok, Namespaces} = gabarit:list_namespaces(),
    ?assert(lists:keymember(gabarit, 1, Namespaces)),

    {value, {gabarit, Limit}} = lists:keysearch(gabarit, 1, Namespaces),
    ?assert(Limit >= 10),

    TemplateContent = <<"<div>Default namespace test</div>">>,
    {ok, TemplateId} = gabarit:add_template(gabarit, "default_test", TemplateContent),

    {ok, RetrievedContent} = gabarit:get_template(TemplateId),
    ?assertEqual(TemplateContent, RetrievedContent),

    ok.

%% @doc Tests listing templates across all namespaces.
%%      Verifies that templates from different namespaces are included
%%      in the global template list.
%% @end
list_all_templates_test(_Config) ->
    NS1 = "ns1_test_" ++ integer_to_list(erlang:system_time(millisecond)),
    NS2 = "ns2_test_" ++ integer_to_list(erlang:system_time(millisecond) + 1),

    ok = gabarit:create_namespace(NS1, #{limit => 5}),
    ok = gabarit:create_namespace(NS2, #{limit => 5}),

    {ok, Ns1TemplateId} = gabarit:add_template(NS1, "ns1_template", <<"NS1 Content">>),
    {ok, Ns2TemplateId} = gabarit:add_template(NS2, "ns2_template", <<"NS2 Content">>),
    {ok, AllTemplates} = gabarit:list_templates(),

    ?assert(lists:keymember(Ns1TemplateId, 1, AllTemplates)),
    ?assert(lists:keymember(Ns2TemplateId, 1, AllTemplates)),

    ok.

%% @doc Tests namespace creation with options.
%%      Verifies that namespaces can be created with default options
%%      and with custom option maps.
%% @end
namespace_options_test(_Config) ->
    DefaultOptionsNS = "default_options_ns_" ++ integer_to_list(erlang:system_time(millisecond)),
    CustomOptionsNS = "custom_options_ns_" ++ integer_to_list(erlang:system_time(millisecond) + 1),

    ok = gabarit:create_namespace(DefaultOptionsNS),
    {ok, Namespaces1} = gabarit:list_namespaces(),
    {value, {DefaultOptionsNS, DefaultLimit}} = lists:keysearch(DefaultOptionsNS, 1, Namespaces1),
    ct:log("Default limit: ~p", [DefaultLimit]),
    ct:log("Namespaces: ~p", [Namespaces1]),
    ct:log("DefaultOptionsNS: ~p", [DefaultOptionsNS]),
    ?assertEqual(infinity, DefaultLimit),

    ok = gabarit:create_namespace(CustomOptionsNS, #{limit => 15}),
    {ok, Namespaces2} = gabarit:list_namespaces(),
    {value, {CustomOptionsNS, CustomLimit}} = lists:keysearch(CustomOptionsNS, 1, Namespaces2),
    ?assertEqual(15, CustomLimit),

    ok.
