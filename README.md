Assigned Record Access
======================

It is a tiny parse transform.

__License__: MIT

__Author__: Uvarov Michael ([`freeakk@gmail.com`](mailto:freeakk@gmail.com))

[![Build Status](https://secure.travis-ci.org/freeakk/arak.png?branch=master)](http://travis-ci.org/freeakk/arak)


Allows to use a short access syntax (dot-syntax) for getting values from
records.

Example 1
---------

In this exampe, the call of `access(#rec{}).` returns `1`.

Before:

```erlang
-record(rec, {f1 = 1, f2 = 2}).

access(X = #rec{}) ->
    X.f1.
```

Other variant:

```erlang
-record(rec, {f1 = 1, f2 = 2}).

access(#rec{f1 = F1}) ->
    F1.
```

After:

```erlang
-compile({parse_transform, arak}).
-record(rec, {f1 = 1, f2 = 2}).

access(X = #rec{}) ->
    X.f1.
```

Illegal form:

```erlang
access(X) ->
    X.f1.
```


Example 2
---------

Not only match syntax can be used for getting the type of the record variable.
This example shows, how a case syntax can be used instead.
Binded variables can be used only inside clauses (they are not exported as in
Erlang).

```erlang
-compile({parse_transform, arak}).
-record(rec,  {f1, f2}).
-record(rec1, {f1, f2}).

bad_case(X) ->
    case X of
         #rec{}  -> X.f1;
         #rec1{} -> X.f1
    end.
```

Illegal form:

```erlang
bad_case(X) ->
    case X of
        #rec{}  -> ok;
        #rec1{} -> ok
    end,
    X.f1.
```
