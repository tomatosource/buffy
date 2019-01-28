# Buffy ⚰️🔪

## Purpose

To identify dead code in go programs. This includes unused exported functions/attrs so cannot be used on libs etc. This will be fixed soon.

*PLEASE NOTE* This is very much a work in progress and has way too much stuff hardcoded just to get it to work, and sometimes it does!

## Installing

```
go get github.com/tomatosource/buffy
```

## Running

Go to the root of the application you want to find dead code in (currently has `"./..."` hardcoded in as path, will be fixed soon), and run `buffy`.

## Reading the output

Output will look something like

```
08:32 (master) ~/gits/go/src/github.com/spaceship-fspl/srv.saver.financier
$ buffy
macquarie.go:42:2 const ReversalDebitDR is unused
macquarie.go:40:2 const MiscellaneousDebitADR is unused
macquarie.go:41:2 const PeriodicalPaymentDebitDR is unused
macquarie.go:45:2 const DepositWithReferenceCR is unused
macquarie.go:38:2 const ServiceChargeDR is unused
```

One unused thing per line - `${position} ${type} ${name} is unused`, unused objects may be

 - function
 - field
 - var
 - const
 - type
 - identifier


## Another note

A lot of this code (the majority) is lifted from `unused`, now known as `staticcheck`, I plan on rewriting it verrrry soon to be a little leaner/more understandable for our application.
