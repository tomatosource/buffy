package main

import (
	"fmt"
	"go/types"
	"log"
	"os"
	"path/filepath"
	"strings"
)

func (u *Unused) String() string {
	name := u.Obj.Name()
	if sig, ok := u.Obj.Type().(*types.Signature); ok && sig.Recv() != nil {
		switch sig.Recv().Type().(type) {
		case *types.Named, *types.Pointer:
			typ := types.TypeString(sig.Recv().Type(), func(*types.Package) string {
				return ""
			})
			if len(typ) > 0 && typ[0] == '*' {
				name = fmt.Sprintf("(%s).%s", typ, u.Obj.Name())
			} else if len(typ) > 0 {
				name = fmt.Sprintf("%s.%s", typ, u.Obj.Name())
			}
		}
	}
	dir, err := filepath.Abs(filepath.Dir(os.Args[0]))
	if err != nil {
		log.Fatal(err)
	}

	return fmt.Sprintf(
		"%s %s %s is unused",
		strings.TrimPrefix(u.Position.String(), dir+"/"),
		typString(u.Obj), name,
	)
}

func typString(obj types.Object) string {
	switch obj := obj.(type) {
	case *types.Func:
		return "func"
	case *types.Var:
		if obj.IsField() {
			return "field"
		}
		return "var"
	case *types.Const:
		return "const"
	case *types.TypeName:
		return "type"
	default:
		return "identifier"
	}
}

func (g *graph) markUsedBy(obj, usedBy interface{}) {
	objNode := g.getNode(obj)
	usedByNode := g.getNode(usedBy)
	if objNode.obj == usedByNode.obj {
		return
	}
	usedByNode.uses[objNode] = struct{}{}
}

func (g *graph) getNode(obj interface{}) *graphNode {
	for {
		if pt, ok := obj.(*types.Pointer); ok {
			obj = pt.Elem()
		} else {
			break
		}
	}
	_, ok := g.nodes[obj]
	if !ok {
		g.addObj(obj)
	}

	return g.nodes[obj]
}

func (g *graph) addObj(obj interface{}) {
	if pt, ok := obj.(*types.Pointer); ok {
		obj = pt.Elem()
	}
	node := &graphNode{obj: obj, uses: make(map[*graphNode]struct{}), n: labelCounter}
	g.nodes[obj] = node
	labelCounter++

	if obj, ok := obj.(*types.Struct); ok {
		n := obj.NumFields()
		for i := 0; i < n; i++ {
			field := obj.Field(i)
			g.markUsedBy(obj, field)
		}
	}
}
