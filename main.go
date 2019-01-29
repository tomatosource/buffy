package main

import (
	"fmt"
	"go/build"
	"go/parser"
	"go/types"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/kisielk/gotool"
	"golang.org/x/tools/go/loader"
)

func main() {
	p, err := loadProgram()
	if err != nil {
		panic(err)
	} else {
		checkProgram(p)
	}
}

func loadProgram() (*loader.Program, error) {
	paths := gotool.ImportPaths([]string{"./..."})
	goFiles, err := resolveRelative(paths)
	if err != nil {
		return nil, err
	}
	ctx := build.Default
	hadError := false
	conf := &loader.Config{
		Build:      &ctx,
		ParserMode: parser.ParseComments,
		ImportPkgs: map[string]bool{},
		TypeChecker: types.Config{
			Error: func(err error) {
				// Only print the first error found
				if hadError {
					return
				}
				hadError = true
				fmt.Fprintln(os.Stderr, err)
			},
		},
	}
	if goFiles {
		conf.CreateFromFilenames("adhoc", paths...)
	} else {
		for _, path := range paths {
			conf.ImportPkgs[path] = true
		}
	}
	lprog, err := conf.Load()
	if err != nil {
		return nil, err
	}
	return lprog, nil
}

func resolveRelative(importPaths []string) (goFiles bool, err error) {
	if len(importPaths) == 0 {
		return false, nil
	}
	if strings.HasSuffix(importPaths[0], ".go") {
		// User is specifying a package in terms of .go files, don't resolve
		return true, nil
	}
	wd, err := os.Getwd()
	if err != nil {
		return false, err
	}
	ctx := build.Default
	for i, path := range importPaths {
		bpkg, err := ctx.Import(path, wd, build.FindOnly)
		if err != nil {
			return false, fmt.Errorf("can't load package %q: %v", path, err)
		}
		importPaths[i] = bpkg.ImportPath
	}
	return false, nil
}

func checkProgram(p *loader.Program) {
	c := NewChecker()
	unused := c.Check(p)
	prettyPrintUnused(unused)
}

func prettyPrintUnused(unused []Unused) {
	for _, u := range unused {
		name := u.Obj.Name()
		if sig, ok := u.Obj.Type().(*types.Signature); ok && sig.Recv() != nil {
			switch sig.Recv().Type().(type) {
			case *types.Named, *types.Pointer:
				typ := types.TypeString(sig.Recv().Type(), func(*types.Package) string { return "" })
				if len(typ) > 0 && typ[0] == '*' {
					name = fmt.Sprintf("(%s).%s", typ, u.Obj.Name())
				} else if len(typ) > 0 {
					name = fmt.Sprintf("%s.%s", typ, u.Obj.Name())
				}
			}
		}
		fmt.Printf(
			"%s %s %s is unused\n",
			relPath(u.Position.String()),
			typString(u.Obj), name,
		)
	}
}

func relPath(p string) string {
	dir, err := filepath.Abs(filepath.Dir(os.Args[0]))
	if err != nil {
		log.Fatal(err)
	}
	return strings.TrimPrefix(p, dir+"/")
}
