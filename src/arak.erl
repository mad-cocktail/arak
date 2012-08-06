-module(arak).
-export([parse_transform/2]).

-record(var, {name, value, level}).
-record(state, {
        %% Store the current nesting level
        level = 0, 
        %% Store binded variables
        variables = [], 
        %% Store a list with numbers of levels on which new scope is created.
        scope_levels = []
       }).



%% type(Node) =:= match_expr
flatten_expressions(Node) ->
    %% "Pattern = Body"
    Pattern = erl_syntax:match_expr_pattern(Node),
    Body    = erl_syntax:match_expr_body(Node),
    case erl_syntax:type(Body) of
        match_expr -> 
            [Pattern|flatten_expressions(Body)];
        _ -> 
            [Pattern, Body]
    end.




    
%% @doc Return a HOF, which can repeat a path to elem, marked with MF.
%% MF is a match function.
find_path(MF, St, Node) ->
    try
        PathF = path(MF, St, Node),
        retrieve_state(PathF, Node)
    catch throw:not_found ->
        undefined
    end.

none() ->
    fun(Node) -> Node end.

%% Eval F1 and pass its result to F2.
two(F1, F2) ->
    fun(Node) -> F2(F1(Node)) end.

tail([_|T]) -> T.
head([H|_]) -> H.

list(_MF, _St, []) ->
    throw(not_found);
list(MF, St, [H|T]) ->
    try
        two(fun head/1, path(MF, St, H))
    catch throw:not_found ->
        two(fun tail/1, list(MF, St, T))
    end.

%% @doc Last function returns `{State, MatchedNode}'.
pass_state(St, F) -> fun(Node) -> {St, F(Node)} end.

%% @doc Retrieve the state and hide it.
retrieve_state(F, TreeNode) -> 
    {St, _MatchedNode} = F(TreeNode),
    %% We cannot rebuild the path fun, so lets just wrap it.
    {St, fun(Node) -> {_, Match} = F(Node), Match end}.

%% MF = MatchFunction
path(MF, St, Node) ->
    case MF(Node, St) of
        matched -> pass_state(St, none());
        St2 ->
            case erl_syntax:type(Node) of
                list ->
                    HeadF = fun erl_syntax:list_head/1,
                    TailF = fun erl_syntax:list_tail/1,
                    try 
                        two(HeadF, path(MF, St2, HeadF(Node)))
                    catch throw:not_found ->
                        Tail = TailF(Node),
                        case erl_syntax:type(Tail) of
                            nil  -> throw(not_found);
                            _    -> two(TailF, path(MF, St2, Tail))
                        end
                    end;
                parentheses ->
                    ParentF = fun erl_syntax:parentheses_body/1,
                    two(ParentF, path(MF, St2, ParentF(Node)));
%               record ->
                tuple ->
                    ElemsF = fun erl_syntax:tuple_elements/1,
                    two(ElemsF, list(MF, St2, ElemsF(Node)));
                _ ->
                    throw(not_found)
            end
    end.




-spec bindings_list_to_variables(BindingsList, Level) -> Vars when
    BindingsList :: [[Node]],
    Node :: erl_syntax:syntaxTree(),
    Level :: non_neg_integer(),
    Vars :: [#var{}].

bindings_list_to_variables(BindingsList, Level) ->
    [Var || Bindings <- BindingsList,
            Var <- bindings_to_variables(Bindings, Level)].


bindings_to_variables(Bindings, Level) ->
    TypesNodes = [{erl_syntax:type(Node), Node} || Node <- Bindings],
    RecNodes = [RecNode || {record_expr, RecNode} <- TypesNodes],
    VarNodes = [VarNode || {variable,    VarNode} <- TypesNodes],
    %% If #rec{} = #rec1{}, the error will be here!
    RecNode = head(RecNodes),
    [error_logger:info_msg("These records newer matches: ~p~n", 
                          [RecNodes]) || length(RecNodes) > 1],
    RecNameTree = erl_syntax:record_expr_type(RecNode),
    RecName = erl_syntax:atom_value(RecNameTree),
    [ #var{ name = erl_syntax:variable_name(VarNode),
           value = RecName,
           level = Level } || VarNode <- VarNodes ].


%% @doc Return a list of a lists of matched valiables.
-spec record_expression_bindings_list(MatchExNode) -> Res when
    MatchExNode :: Node,
    Res :: [[Node|false]],
    Node :: erl_syntax:syntaxTree().

record_expression_bindings_list(MatchExNode) ->
    %% get a list of HOFs to retrieve record_expr nodes
    Nodes = flatten_expressions(MatchExNode),
    IsMatched = fun is_record_expr/1,
    PathFuns = get_all_record_expr_paths(Nodes, IsMatched, [], []),
    [[valid_node(PathFun, Node) || Node <- Nodes]
                                || PathFun <- PathFuns].
    
valid_node(PathFun, Node) ->
    try
        PathFun(Node)
    catch _:_ ->
        false
    end.

is_record_expr(Node) ->
    erl_syntax:type(Node) =:= record_expr.

get_all_record_expr_paths([], _IsMatched, _PrevStates, PathFs) ->
    lists:reverse(PathFs);
get_all_record_expr_paths([H|T], IsMatched, PrevStates, PathFs) ->
    MF = fun(Node, State) -> 
            case IsMatched(Node) of
                true -> 
                    %% Is it matched already?
                    case lists:member(State, PrevStates) of
                        false -> matched;
                        true -> State + 1
                    end;
                false -> State + 1
            end
        end,
    %% Use an integer (counter) as a state. The initial value is 0.
    case find_path(MF, 0, H) of
        undefined ->
            get_all_record_expr_paths(T, IsMatched, PrevStates, PathFs);
        {NewState, PathF} ->
            %% Restart for the current node
            get_all_record_expr_paths([H|T], IsMatched, [NewState|PrevStates],
                                      [PathF|PathFs])
    end.


                     





parse_transform(Forms, _Options) ->
    F = fun visitor/2,
    Acc = #state{},
    X = [element(1, preorder(F, Acc, Tree)) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.

%% @doc Mark this level as a level, that creates a scope.
add_scope_level(Acc=#state{scope_levels = Lvls, level = Lvl}) ->
    Acc#state{scope_levels = [Lvl|Lvls]}.


%% @doc This function will be passed to `mapfoldl' of the AST.
visitor(level_up, Acc=#state{level = Lvl}) ->
%   io:format(user, "~n ~B up~n", [Lvl + 1]),
    Acc#state{level = Lvl + 1};

%% This level is a new scope.
visitor(level_down, Acc=#state{level = Lvl, scope_levels = [Lvl|Lvls], 
                               variables = Vars}) ->
%   io:format(user, " ~B down and clean\t~p~2n", [Lvl, Vars]),
    %% Decrease the scope number;
    %% Delete the current level from scope numbers;
    %% Delete binded variables on this level.
    IsOnCurrentLvlOrHigher = fun(#var{level = VarLvl}) -> VarLvl >= Lvl end,
    Vars2 = lists:dropwhile(IsOnCurrentLvlOrHigher, Vars),
    Acc#state{level = Lvl - 1, scope_levels = Lvls, variables = Vars2};

%% This level is not a new scope.
visitor(level_down, Acc=#state{level = Lvl}) ->
%   io:format(user, " ~B down~2n", [Lvl]),
    Acc#state{level = Lvl - 1};

visitor(Node, Acc=#state{variables = Vars, level = Lvl}) ->
%   io:format(user, ":\t~p\n", [Node]),
    Old = {Node, Acc},
    case erl_syntax:type(Node) of
    fun_expr ->
        {Node, add_scope_level(Acc)};
    funtion  ->
        {Node, add_scope_level(Acc)};
    list_comp ->
        {Node, add_scope_level(Acc)};

    match_expr -> 
        %% Bind variables.
        Bindings = record_expression_bindings_list(Node),
%       io:format(user, "|:\t~p~n", [Bindings]),
        NewVars = bindings_list_to_variables(Bindings, Lvl),
        {Node, Acc#state{variables = NewVars ++ Vars}};

    record_access ->
        %% Insert the record name.
        %% "Argument#Type.Field"
        Argument = erl_syntax:record_access_argument(Node),
        Type     = erl_syntax:record_access_type(Node),
        case {Type, erl_syntax:type(Argument)} of
        {none, variable} ->
            Field   = erl_syntax:record_access_field(Node),
            VarName = erl_syntax:variable_name(Argument),
%           io:format(user, "!:\t~p\t~p~n", [Field, VarName]),
            case lists:keyfind(VarName, #var.name, Vars) of
            #var{value = RecName} ->
                NewType = erl_syntax:atom(RecName),
                NewNode = erl_syntax:record_access(Argument, NewType, Field),
                {NewNode, Acc};
            false -> %% Unknown "Var#Field".
                Old
            end;
        _ -> %% Not a "Var#Field".
            Old
        end;

    _X -> %% Not interesting type.
        Old
    end.



%% @doc nested tree mapfoldl.
-spec preorder(F, Acc, Form) -> {Form, Acc} when
    F :: fun((Form, Acc) -> {Form, Acc}),
    Form :: erl_syntax:syntaxTree(),
    Acc :: term().

preorder(F, Acc, Form) ->
%   io:format(user, "$\t~p\n", [Form]),
    {Form1, Acc1} = F(Form, F(level_up, Acc)),
    {Tree1, Acc2} = preorder2(F, Acc1, Form1),
    Form2 = erl_syntax:revert(Tree1),
    {Form2, F(level_down, Acc2)}.

preorder2(F, Acc, Form) ->
    FF = preorder_foldl_hof2(F),
    case {erl_syntax:type(Form), erl_syntax:subtrees(Form)} of
    {list_comp, [Template|Body]} ->
        %% Body = [E1, ..., En], than "[Template || E1, ..., En]".
        %% Analyze the body before the template for list comprehensions.
        {Groups,    Acc1} = lists:mapfoldl(FF, Acc, Body),
        {Template2, Acc2} = FF(Template, Acc1),
        Groups2 = erl_syntax:update_tree(Form, [Template2|Groups]),
        {Groups2,   Acc2};

    {_, []} ->
        {Form, Acc};
    {_, List} ->
        {Groups, Acc1} = lists:mapfoldl(FF, Acc, List),
        Groups2 = erl_syntax:update_tree(Form, Groups),
        {Groups2, Acc1}
    end.

preorder_foldl_hof(F) ->
    fun(Group, Acc) -> preorder(F, Acc, Group) end.

preorder_foldl_hof2(F) ->
    fun(Group, Acc) -> 
        FF = preorder_foldl_hof(F),
        lists:mapfoldl(FF, Acc, Group)
        end.

