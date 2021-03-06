-module(arak_tests).

-compile({parse_transform, arak}).
-compile(export_all).


-record(rec, {f1 = 1, f2 = 2}).
-record(rec1, {f1 = 3, f2 = 4}).

direct_access_after(X = #rec{}) ->
    X.f1.

direct_access_before(X = #rec{}) ->
    X#rec.f1.

direct_access_nested(X = #rec{}) ->
    %% X shadowed.
    fun(X = #rec1{}) ->
            X.f1
        end.

access_nested(X = #rec{}) ->
    fun(X = #rec1{}) ->
            X.f1
        end.

access_lc(Xs, F2) ->
   [ X.f1 || X=#rec{} <- Xs, X.f2 =:= F2].

access_lc_before(Xs, F2) ->
    [ X#rec.f1 || X=#rec{} <- Xs, X#rec.f2 =:= F2].

shadowed_access_lc(Xs, F2) ->
    [ X#rec.f1 || X=#rec1{} <- [#rec1{}], X=#rec{} <- Xs, X#rec.f2 =:= F2].

leave_scope(X = #rec{}) ->
    %% New scope
    F = fun(X = #rec1{}) -> X end,
    %% Leave the scope
    F(#rec1{}),
    X.f1.

difficult_match(Y = X = #rec{}) ->
    X.f1.

difficult_match2(X = Y = #rec{}) ->
    X.f1.

difficult_match3([X|_] = [#rec{}|_]) ->
    X.f1.

difficult_match4([_, X|_] = [_, #rec{}|_]) ->
    X.f1.

difficult_match5({_, X} = {_, #rec{}}) ->
    X.f1.

few_clauses(X = #rec{}) -> X.f1;
few_clauses(X = #rec1{}) -> X.f2.

case_case(X) ->
    case X of
        #rec{}  -> X.f1;
        #rec1{} -> X.f1
    end.

case_case2(X) ->
    case X of
        Y = #rec{}  -> Y.f1;
        Z = #rec1{} -> Z.f1
    end.

wtf(#rec1{} = #rec{}) -> ok.


case_bug(L) ->
    case L of
        %% Matched by key (it is rare case).
        [#rec1{f1 = F1}] -> F1
    end.

case_bug2(Rec) ->
    %% Get something before this date.
    case [Rec] of
        List ->
            case hd(List) of
                #rec1{f1 = F1} -> F1
            end
    end.



-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

direct_access_test_() ->
    [ ?_assertEqual(direct_access_after(#rec{}), 1) ].

access_nested_test_() ->
    [ {"The parameter of the HOF is analyzed."
      ,?_assertEqual((access_nested(#rec{}))(#rec1{}), 3)} ].

access_lc_test_() ->
    [ ?_assertEqual(access_lc([#rec{}], 2), [1]) 
    , {"LC with few generators."
       ,?_assertEqual(shadowed_access_lc([#rec{}], 2), [1])} ].

leave_scope_test_() ->
    [ ?_assertEqual(leave_scope(#rec{}), 1) ].

difficult_match_test_() ->
    [ ?_assertEqual(difficult_match(#rec{}), 1) 
    , ?_assertEqual(difficult_match2(#rec{}), 1) 
    , ?_assertEqual(difficult_match3([#rec{}]), 1)
    , ?_assertEqual(difficult_match4([skip, #rec{}]), 1) 
    , ?_assertEqual(difficult_match5({skip, #rec{}}), 1) 
    ].

case_case_test_() ->
    [ ?_assertEqual(case_case(#rec{}), 1) 
    , ?_assertEqual(case_case(#rec1{}), 3) 
    ].

-endif.
