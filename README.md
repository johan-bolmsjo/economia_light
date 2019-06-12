# economia_light

Minimalist debt tracker able to resolve debts among a closed group of individuals.

The basic idea is that shared spendings are accounted by who spent the money and
whom are to share the debt. Each individual in the group have a running balance,
positive or negative. A debt can be cleared by payment to anyone in the group.

## Background Story

This tool was originally a Python script hacked together in a couple of hours
after a trip to clear debts among the participants. This program although still
simple is much more sophisticated with a proper parser.

I'm trying to learn OCaml and thought that rewriting this tool was a good
exercise to improve my skills. It's a simple enough program to be a good first
"real" program when learning a new programming language and OCaml is supposedly
good for parsing stuff.

## WARNING

This software is not fully tested! I used it as a learning tool and is not
really interested in making sure it's rock solid.

* Parser lacks automated tests
* Better reporting with dates on transactions
* Perhaps some more input validations should be added

## Usage

    usage: economia_light INPUT_FILE

## Build

A working OCaml installation is required, see https://ocaml.org/.

Make sure the following packages are installed on your system (using opam).

* alcotest
* dune

Execute the following commands to install `economia_light`:

    dune build && dune install --prefix ~/.local

Optionally run the tests:

    dune runtest

## Example

There is an example file in `example/expenses.txt` that will give a feeling for
how to use the tool.

    economia_light example/expenses.txt
