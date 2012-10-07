Assigned Record Access
======================

It is a tiny parse transform.

**License**: MIT

**Author**: Uvarov Michael (freeakk@gmail.com)

.. image:: https://secure.travis-ci.org/mad-cocktail/arak.png?branch=master
    :alt: Build Status
    :target: http://travis-ci.org/mad-cocktail/arak

Allows to use a short access syntax (dot-syntax) for getting values from
records.

Example 1
---------

In this exampe, the call of ``access(#rec{}).`` returns ``1``.

Before:

.. code-block:: erlang

    -record(rec, {f1 = 1, f2 = 2}).

    access(X = #rec{}) ->
        X.f1.

Other variant:

.. code-block:: erlang

    -record(rec, {f1 = 1, f2 = 2}).

    access(#rec{f1 = F1}) ->
        F1.

After:

.. code-block:: erlang

    -compile({parse_transform, arak}).
    -record(rec, {f1 = 1, f2 = 2}).

    access(X = #rec{}) ->
        X.f1.

Illegal form:

.. code-block:: erlang

    access(X) ->
        X.f1.

Example 2
---------

Not only match syntax can be used for getting the type of the record
variable. This example shows, how a case syntax can be used instead.
Binded variables can be used only inside clauses (they are not exported
as in Erlang).

.. code-block:: erlang

    -compile({parse_transform, arak}).
    -record(rec,  {f1, f2}).
    -record(rec1, {f1, f2}).

    bad_case(X) ->
        case X of
             #rec{}  -> X.f1;
             #rec1{} -> X.f1
        end.

Illegal form:

.. code-block:: erlang

    bad_case(X) ->
        case X of
            #rec{}  -> ok;
            #rec1{} -> ok
        end,
        X.f1.


Example 3
---------

We can get mnemosyne-like fields combining this parse transform with QLC.

.. code-block:: erlang

    -compile({parse_transform, arak}).
    -include_lib("stdlib/include/qlc.hrl").

    revision_ids(RepId, undefined, Hashes) when RepId =/= undefined ->
        Q = qlc:q([R || R=#g_revision{} <- mnesia:table(g_revision),
                        B=#g_branch{}   <- mnesia:table(g_branch),
                        Hash <- Hashes,
                        B.id  =:= R.branch,
                        Hash  =:= R.commit_hash,
                        RepId =:= B.repository]),
        qlc:e(Q).


