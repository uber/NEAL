# Writing a custom Reporter

Reporters are the components who take the all the violations found in the files analysed and output them in an appropriate format. As an example, NEAL ships with two builtin formatters: [CLI][1] and [ARC][2], the former to pretty print the errors in the command line and the latter to integrate with [Arcanist][3] (Phabricator's command line interface).

The interface for creating a reporter currently consists of a single method:
```ocaml
module type REPORTER = sig
  val name : string
  val report : severity -> Ctx.ctx -> fix -> unit
end
```

And the definition for the `sevirity` and `fix` types is as follows:
```ocaml
type severity =
  | Error
  | Warning
  | Other

type fix =
  | Suggestion of string

```

## Using a custom reporter

* Custom reporters must be compiled to a `.cmxs` file, which is OCaml's dynamic library format (as described [here][4]);
* They must have `.reporter` suffix before the `.cmxs` extension, e.g. `arc.reporter.cmxs`;
* You must tell NEAL the path to your reporter (using the `-f`/`--reporter` [configuration flag][5]).

After that NEAL should already be report violations using your new reporter.

## Sample reporter (ARC)

As example, here's the simplest reporter so far, the ARC reporter mentioned above:

```ocaml
module Arc = struct
  let name = "arc"

  let report severity ctx (R.Suggestion fix) =
    let line = Loc.get_line ctx in
    let severity' = R.string_of_severity severity in
    Printf.printf "%s:%d %s\n" severity' line fix
end
```

[1]: https://github.com/uber/NEAL/blob/master/src/reporters/cli/cli_reporter.ml
[2]: https://github.com/uber/NEAL/blob/master/src/reporters/arc/arc_reporter.ml

[3]: https://secure.phabricator.com/book/phabricator/article/arcanist/
[4]: ../components#plugins
[5]: ../configuration.md#flags
