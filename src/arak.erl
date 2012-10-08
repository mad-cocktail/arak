-module(arak).
-export([parse_transform/2]).

-record(var, {name, value, level}).
-record(state, {
        %% Store the current nesting level
        level = 0, 
        %% Store binded variables
        variables = [], 
        %% Store case arguments
        case_arguments = [], 
        %% Store a list with numbers of levels on which new scope is created.
        scope_levels = [],
        scope_level_types = []
       }).

-type path_tokens() :: [fun() | head | tail].

parse_transform(Forms, _Options) ->
    F = fun visitor/2,
    Acc = #state{},
    X = [element(1, preorder_level_foldmap(F, Acc, Tree)) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.



%% ===============================================================
%% Helpers
%% ===============================================================

node_type(Node) ->
    erl_syntax:type(Node).


%% @doc Copy positional information from `From' node to `To' node.       
-spec copy_pos(From, To) -> To when                            
    From :: erl_syntax:syntaxTree(),                           
    To :: From.                                                
                                                               
copy_pos(From, To) ->                                          
    Pos = erl_syntax:get_pos(From),                            
    erl_syntax:set_pos(To, Pos).                               


%% @doc Form a list of explessions from a Tree.
%%
%% Example:
%% Tree: E1 = E2 = E3 = E4;
%% List: [E1, E2, E3, E4].
flatten_expressions(Node) ->
    %% "Pattern = Body"
    match_expr = node_type(Node),
    Pattern = erl_syntax:match_expr_pattern(Node),
    Body    = erl_syntax:match_expr_body(Node),
    flatten_expressions(Pattern, Body).


flatten_expressions(Pattern, Body) ->
    case node_type(Body) of
        match_expr -> 
            [Pattern|flatten_expressions(Body)];
        _ -> 
            [Pattern, Body]
    end.


%% ===============================================================
%% Find path and helpers for it
%% ===============================================================
    
%% @doc Return a HOF PathFun, which can repeat a path to the node of
%% the tree Node, marked with MF.
%% MF is a match function.
%% Returns `undefined', if MF returns `false' for all nodes of the tree.
-spec find_path(MF, State, Node) -> {PathTokens, State} | undefined when
    MF :: fun((Node, State) -> matched | State),
    State :: term(),
    PathTokens :: path_tokens(). 

find_path(MF, St, Node) ->
    try
        path(MF, St, Node)
    catch throw:not_found ->
        undefined
    end.

%% @doc Add a path token on recursion return stage.
push_decision(Decision, {Decisions, State}) -> 
    {[Decision|Decisions], State}.


%% @doc Retrieve a node path.
%% Calls `MatchFunction(_, State)' for each node.
%%
%% @see find_path/2
%% @private
-spec path(MatchFunction, State, Node) -> {PathTokens, State} when
    MatchFunction :: fun(),
    State :: term(),
    Node :: erl_syntax:syntaxTree(),
    PathTokens :: path_tokens().

path(MF, St, Node) ->
    case MF(Node, St) of
        matched -> {[], St};
        %% Not matched, get the new state.
        St2 ->
            %% Gen a next node, save path.
            case node_type(Node) of
                list ->
                    HeadF = fun erl_syntax:list_head/1,
                    TailF = fun erl_syntax:list_tail/1,
                    try 
                        push_decision(HeadF, path(MF, St2, HeadF(Node)))
                    catch throw:not_found ->
                        Tail = TailF(Node),
                        case node_type(Tail) of
                            nil  -> throw(not_found);
                            _    -> push_decision(TailF, path(MF, St2, Tail))
                        end
                    end;
                parentheses ->
                    ParentF = fun erl_syntax:parentheses_body/1,
                    push_decision(ParentF, path(MF, St2, ParentF(Node)));
%               record ->
                tuple ->
                    ElemsF = fun erl_syntax:tuple_elements/1,
                    push_decision(ElemsF, list(MF, St2, ElemsF(Node)));
                _ ->
                    throw(not_found)
            end
    end.


list(_MF, _St, NodeList) ->
    list_cycle(_MF, _St, NodeList, 1).

%% Find the position of the matched element.
list_cycle(_MF, _St, [], _Pos) ->
    throw(not_found);
list_cycle(MF, St, [H|T], Pos) ->
    try
        push_decision({list_element, Pos}, path(MF, St, H))
    catch throw:not_found ->
        list_cycle(MF, St, T, Pos+1)
    end.


%% ===============================================================
%% Apply path
%% ===============================================================

%% @doc Return a sub-node of `Node' using a saved path.
-spec apply_path(PathTokens, Node) -> Node when
    PathTokens :: path_tokens(),
    Node :: erl_syntax:syntaxTree().
apply_path(PathTokens, Node) ->
    lists:foldl(fun next_node/2, Node, PathTokens).


next_node({list_element, Pos}, List) ->
    lists:nth(Pos, List);
%% `Decision' is a function, for example, fun erl_syntax:list_head/1.
next_node(Decision, Node) ->
    Decision(Node).



%% ===============================================================
%% Variable bindings
%% ===============================================================

%% @doc Convert bindings to the list of `#var{}'.
%% Binding is an expression `Var = Expr'.
%% `BindingsList' is a list of bindings for each variable.
%%
%% @see record_expression_bindings_list/1
-spec bindings_list_to_variables(BindingsList, Level) -> Vars when
    BindingsList :: [[Node]],
    Node :: erl_syntax:syntaxTree(),
    Level :: non_neg_integer(),
    Vars :: [#var{}].

bindings_list_to_variables(BindingsList, Level) ->
    [Var || Bindings <- BindingsList,
            Var <- bindings_to_variables(Bindings, Level)].


bindings_to_variables(Bindings, Level) ->
    TypesNodes = [{node_type(Node), Node} || Node <- Bindings],
    RecNodes = [RecNode || {record_expr, RecNode} <- TypesNodes],
    VarNodes = [VarNode || {variable,    VarNode} <- TypesNodes],
    %% If #rec{} = #rec1{}, the error will be here!
    RecNode = hd(RecNodes),
    [error_logger:info_msg("These records newer matches: ~p~n", 
                          [RecNodes]) || length(RecNodes) > 1],
    RecNameTree = erl_syntax:record_expr_type(RecNode),
    RecName = erl_syntax:atom_value(RecNameTree),
    [ #var{ name = erl_syntax:variable_name(VarNode),
           value = RecName,
           level = Level } || VarNode <- VarNodes ].


%% @doc Return a list of a lists of matched valiables.
-spec record_expression_bindings_list(Nodes) -> Res when
    Nodes :: [Node],
    Res :: [[Node|undefined]],
    Node :: erl_syntax:syntaxTree().

record_expression_bindings_list(Nodes) ->
    IsMatched = fun is_record_expr/1,
    %% Get a list of paths of record_expr nodes.
    Paths = match_all_nodes(Nodes, IsMatched),
    %% Get paths.
    [try_apply_path_for_nodes(Path, Nodes) || Path <- Paths].
    
%% @doc It is a helper for previous function.
try_apply_path_for_nodes(Path, [Node|Nodes]) ->
    try 
        [apply_path(Path, Node)|try_apply_path_for_nodes(Path, Nodes)]
    catch error:Reason ->
        error_logger:info_msg("Maybe error: ~p~n", [Reason]),
        try_apply_path_for_nodes(Path, Nodes)
    end;

try_apply_path_for_nodes(_Path, []) ->
    [].


is_record_expr(Node) ->
    node_type(Node) =:= record_expr.


%% @doc Return all sub-nodes of nodes from the list `Nodes', for those 
%%      `IsMatched' returns `true'.
-spec match_all_nodes(Nodes, IsMatched) -> [PathTokens] when
    Nodes :: [Node],
    Node :: erl_syntax:syntaxTree(),
    IsMatched :: fun((Node) -> boolean()),
    PathTokens :: path_tokens().

match_all_nodes(Nodes, IsMatched) ->
    match_all_nodes(Nodes, IsMatched, [], []).


match_all_nodes([], _IsMatched, _PrevStates, Paths) ->
    lists:reverse(Paths);
match_all_nodes([H|T], IsMatched, PrevStates, Paths) ->
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
            match_all_nodes(T, IsMatched, PrevStates, Paths);
        {Path, NewState} ->
            %% Restart for the current node
            match_all_nodes([H|T], IsMatched, [NewState|PrevStates],
                            [Path|Paths])
    end.



%% ===============================================================
%% Visitor
%% ===============================================================

%% @doc Mark this level as a level, that creates a scope.
add_scope_level(Type, Acc=#state{scope_levels = Lvls, level = Lvl, 
                                 scope_level_types = Types}) ->
    Acc#state{scope_levels = [Lvl|Lvls], scope_level_types = [Type|Types]}.


%% @doc Mark this level as a level, that holds clauses of the case `Node'.
add_case_argument(Node, Acc=#state{level = Lvl, case_arguments = CaseArgs}) ->
    Argument = erl_syntax:case_expr_argument(Node),
    CaseArg = #var{value = Argument, level = Lvl},
    Acc#state{case_arguments = [CaseArg|CaseArgs]}.


%% @doc This function will be passed to `mapfoldl' of the AST.
visitor(level_up, Acc=#state{level = Lvl}) ->
%   io:format(user, "~n ~B up~n", [Lvl + 1]),
    Acc#state{level = Lvl + 1};

%% This level is a new scope.
visitor(level_down, Acc=#state{level = Lvl, scope_levels = [Lvl|Lvls], 
                               scope_level_types = [_|ScopeLevelTypes],
                               variables = Vars, case_arguments = CaseArgs}) ->
%   io:format(user, " ~B down and clean\t~p~2n", [Lvl, Vars]),
    %% Decrease the scope number;
    %% Delete the current level from scope numbers;
    %% Delete binded variables on this level.
    IsOnCurrentLvlOrHigher = fun(#var{level = VarLvl}) -> VarLvl >= Lvl end,
    Vars2     = lists:dropwhile(IsOnCurrentLvlOrHigher, Vars),
    CaseArgs2 = lists:dropwhile(IsOnCurrentLvlOrHigher, CaseArgs),
    Acc#state{level = Lvl - 1, scope_levels = Lvls, variables = Vars2,
              case_arguments = CaseArgs2, scope_level_types = ScopeLevelTypes};

%% This level is not a new scope.
visitor(level_down, Acc=#state{level = Lvl}) ->
%   io:format(user, " ~B down~2n", [Lvl]),
    Acc#state{level = Lvl - 1};

visitor(Node, Acc=#state{variables = Vars, level = Lvl}) ->
%   io:format(user, ":\t~p\n", [Node]),
    Old = {Node, Acc},
    NodeType = node_type(Node),
    case NodeType of
    fun_expr ->
        {Node, add_scope_level(NodeType, Acc)};
    funtion  ->
        {Node, add_scope_level(NodeType, Acc)};
    list_comp ->
        {Node, add_scope_level(NodeType, Acc)};
    case_expr ->
            {Node, add_case_argument(Node, add_scope_level(NodeType, Acc))};
    clause ->
            #state{ case_arguments = CaseArgs, 
                   scope_level_types = ScopeLevelTypes } = Acc,
            case ScopeLevelTypes of
            [case_expr|_] ->
                [#var{value = Argument}] = CaseArgs,
                %% The clause is a case clause.
                [Pattern] = erl_syntax:clause_patterns(Node),
%               io:format(user, "*: ~p ~p~n", [Argument, Pattern]),
                %% Handle a case argument and a pattern as a match expression.
                Nodes = flatten_expressions(Argument, Pattern),
                Bindings = record_expression_bindings_list(Nodes),
                NewVars = bindings_list_to_variables(Bindings, Lvl),
                %% Here is a trick: use the case_expr node type for
                %% simplify the pattern matching.
                Acc1 = add_scope_level(case_expr, Acc),
                {Node, Acc1#state{variables = NewVars ++ Vars}};
            _ -> 
                %% It is an if or fun clause
                {Node, add_scope_level(clause, Acc)}
            end;

    match_expr -> 
        %% Bind variables.
        Nodes = flatten_expressions(Node),
        Bindings = record_expression_bindings_list(Nodes),
%       io:format(user, "|:\t~p~n", [Bindings]),
        NewVars = bindings_list_to_variables(Bindings, Lvl),
        {Node, Acc#state{variables = NewVars ++ Vars}};

    record_access ->
        %% Insert the record name.
        %% "Argument#Type.Field"
        Argument = erl_syntax:record_access_argument(Node),
        Type     = erl_syntax:record_access_type(Node),
        case {Type, node_type(Argument)} of
        {none, variable} ->
            Field   = erl_syntax:record_access_field(Node),
            VarName = erl_syntax:variable_name(Argument),
%           io:format(user, "!:\t~p\t~p~n", [Field, VarName]),
            case lists:keyfind(VarName, #var.name, Vars) of
            #var{value = RecName} ->
                NewType = erl_syntax:atom(RecName),
                NewNode = erl_syntax:record_access(Argument, NewType, Field),
                {copy_pos(Node, NewNode), Acc};
            false -> %% Unknown "Var#Field".
                Old
            end;
        _ -> %% Not a "Var#Field".
            Old
        end;

    _Type -> %% Not interesting type.
%           io:format(user, "!:\t~p\t~p~n", [_Type, Node]),
        Old
    end.



%% ===============================================================
%% Fold
%% ===============================================================


%% @doc Nested tree mapfoldl.
%% It is like a simple tree fold, but `F' will be called as:
%% `F(level_up)'    Before the first node on the level was folded.
%% `F(level_down)'  After the last node on the level was folded.
%%
%% This functions passes the body of the list comprehension <b>before</b>
%% the template of the list comprehension.
-spec preorder_level_foldmap(F, Acc, Form) -> {Form, Acc} when
    F :: fun((Form, Acc) -> {Form, Acc}),
    Form :: erl_syntax:syntaxTree(),
    Acc :: term().

preorder_level_foldmap(F, Acc, Form) ->
%   io:format(user, "$\t~p\n", [Form]),
    {Form1, Acc1} = F(Form, F(level_up, Acc)),
    {Tree1, Acc2} = preorder_level_foldmap2(F, Acc1, Form1),
    Form2 = erl_syntax:revert(Tree1),
    {Form2, F(level_down, Acc2)}.

preorder_level_foldmap2(F, Acc, Form) ->
    FF = preorder_level_foldmap_group_handler(F),
    case {node_type(Form), erl_syntax:subtrees(Form)} of
    {_, []} ->
        {Form, Acc};

    {list_comp, [Template|Body]} ->
        %% Body = [E1, ..., En], than "[Template || E1, ..., En]".
        %% Analyze the body before the template for list comprehensions.
        {Groups,    Acc1} = lists:mapfoldl(FF, Acc, Body),
        {Template2, Acc2} = FF(Template, Acc1),
        Groups2 = erl_syntax:update_tree(Form, [Template2|Groups]),
        {Groups2,   Acc2};

    {_, List} ->
        {Groups, Acc1} = lists:mapfoldl(FF, Acc, List),
        Groups2 = erl_syntax:update_tree(Form, Groups),
        {Groups2, Acc1}
    end.

preorder_level_foldmap_group_handler(F) ->
    fun(Group, Acc) -> 
        FF = preorder_level_foldmap_visitor(F),
        lists:mapfoldl(FF, Acc, Group)
        end.


preorder_level_foldmap_visitor(F) ->
    fun(Group, Acc) -> preorder_level_foldmap(F, Acc, Group) end.
