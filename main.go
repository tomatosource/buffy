package main

import (
	"fmt"
	"go/build"
	"go/parser"
	"sort"

	"golang.org/x/tools/go/loader"
)

func main() {
	ctx := build.Default
	conf := &loader.Config{
		Build:      &ctx,
		ParserMode: parser.ParseComments,
		ImportPkgs: map[string]bool{"./.": true},
	}
	p, err := conf.Load()
	if err != nil {
		panic(err)
	}

	var unused []string
	for _, u := range Check(p) {
		unused = append(unused, u.String())
	}
	sort.Strings(unused)
	for _, u := range unused {
		fmt.Println(u)
	}
}
