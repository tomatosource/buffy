package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"path/filepath"
	"strings"

	"golang.org/x/tools/go/loader"
	"golang.org/x/tools/go/types/typeutil"
)

type Checker struct {
	graph        *graph
	msCache      typeutil.MethodSetCache
	lprog        *loader.Program
	topmostCache map[*types.Scope]*types.Scope
	interfaces   []*types.Interface
}

type graph struct {
	roots []*graphNode
	nodes map[interface{}]*graphNode
}

type graphNode struct {
	obj   interface{}
	uses  map[*graphNode]struct{}
	used  bool
	quiet bool
}

type Unused struct {
	Obj      types.Object
	Position token.Position
}

func Check(p *loader.Program) []Unused {
	c := &Checker{
		graph: &graph{
			nodes: make(map[interface{}]*graphNode),
		},
		topmostCache: make(map[*types.Scope]*types.Scope),
		lprog:        p,
	}
	return c.Check()
}

func (c *Checker) Check() []Unused {
	var unused []Unused
	c.findExportedInterfaces()
	for _, pkg := range c.lprog.InitialPackages() {
		c.processDefs(pkg)
		c.processUses(pkg)
		c.processTypes(pkg)
		c.processSelections(pkg)
		c.processAST(pkg)
	}

	for _, node := range c.graph.nodes {
		obj, ok := node.obj.(types.Object)
		if !ok {
			continue
		}
		typNode, ok := c.graph.nodes[obj.Type()]
		if !ok {
			continue
		}
		node.uses[typNode] = struct{}{}
	}

	roots := map[*graphNode]struct{}{}
	for _, root := range c.graph.roots {
		roots[root] = struct{}{}
	}
	markNodesUsed(roots)
	c.markNodesQuiet()

	for _, node := range c.graph.nodes {
		if node.used || node.quiet {
			continue
		}
		obj, ok := node.obj.(types.Object)
		if !ok {
			continue
		}
		found := false
		for _, pkg := range c.lprog.InitialPackages() {
			if pkg.Pkg == obj.Pkg() {
				found = true
				break
			}
		}
		if !found {
			continue
		}

		pos := c.lprog.Fset.Position(obj.Pos())
		if pos.Filename == "" || filepath.Base(pos.Filename) == "C" {
			continue
		}
		unused = append(unused, Unused{Obj: obj, Position: pos})
	}
	return unused
}

func (c *Checker) markFields(typ types.Type) {
	structType, ok := typ.Underlying().(*types.Struct)
	if !ok {
		return
	}
	n := structType.NumFields()
	for i := 0; i < n; i++ {
		field := structType.Field(i)
		c.graph.markUsedBy(field, typ)
	}
}

// isNoCopyType reports whether a type represents the NoCopy sentinel
// type. The NoCopy type is a named struct with no fields and exactly
// one method `func Lock()` that is empty.
//
// FIXME(dh): currently we're not checking that the function body is
// empty.
func isNoCopyType(typ types.Type) bool {
	st, ok := typ.Underlying().(*types.Struct)
	if !ok {
		return false
	}
	if st.NumFields() != 0 {
		return false
	}

	named, ok := typ.(*types.Named)
	if !ok {
		return false
	}
	if named.NumMethods() != 1 {
		return false
	}
	meth := named.Method(0)
	if meth.Name() != "Lock" {
		return false
	}
	sig := meth.Type().(*types.Signature)
	if sig.Params().Len() != 0 || sig.Results().Len() != 0 {
		return false
	}
	return true
}

func (c *Checker) useNoCopyFields(typ types.Type) {
	if st, ok := typ.Underlying().(*types.Struct); ok {
		n := st.NumFields()
		for i := 0; i < n; i++ {
			field := st.Field(i)
			if isNoCopyType(field.Type()) {
				c.graph.markUsedBy(field, typ)
			}
		}
	}
}

func (c *Checker) useExportedFields(typ types.Type) {
	if st, ok := typ.Underlying().(*types.Struct); ok {
		n := st.NumFields()
		for i := 0; i < n; i++ {
			field := st.Field(i)
			if field.Exported() {
				c.graph.markUsedBy(field, typ)
			}
		}
	}
}

func (c *Checker) processDefs(pkg *loader.PackageInfo) {
	for _, obj := range pkg.Defs {
		if obj == nil {
			continue
		}
		c.graph.getNode(obj)

		if obj, ok := obj.(*types.TypeName); ok {
			c.graph.markUsedBy(obj.Type().Underlying(), obj.Type())
			c.graph.markUsedBy(obj.Type(), obj) // TODO is this needed?
			c.graph.markUsedBy(obj, obj.Type())

			// We mark all exported fields as used. For normal
			// operation, we have to. The user may use these fields
			// without us knowing.
			//
			// TODO(dh): In whole-program mode, however, we mark them
			// as used because of reflection (such as JSON
			// marshaling). Strictly speaking, we would only need to
			// mark them used if an instance of the type was
			// accessible via an interface value.
			c.useExportedFields(obj.Type())
		}

		switch obj := obj.(type) {
		case *types.Var, *types.Const, *types.Func, *types.TypeName:
			if obj.Exported() {
				// Exported variables and constants use their types,
				// even if there's no expression using them in the
				// checked program.
				//
				// Also operates on funcs and type names, but that's
				// irrelevant/redundant.
				c.graph.markUsedBy(obj.Type(), obj)
			}
			if obj.Name() == "_" {
				node := c.graph.getNode(obj)
				node.quiet = true
				scope := c.topmostScope(pkg.Pkg.Scope().Innermost(obj.Pos()), pkg.Pkg)
				if scope == pkg.Pkg.Scope() {
					c.graph.roots = append(c.graph.roots, node)
				} else {
					c.graph.markUsedBy(obj, scope)
				}
			} else {
				// Variables declared in functions are used. This is
				// done so that arguments and return parameters are
				// always marked as used.
				if _, ok := obj.(*types.Var); ok {
					if obj.Parent() != obj.Pkg().Scope() && obj.Parent() != nil {
						c.graph.markUsedBy(obj, c.topmostScope(obj.Parent(), obj.Pkg()))
						c.graph.markUsedBy(obj.Type(), obj)
					}
				}
			}
		}

		if fn, ok := obj.(*types.Func); ok {
			// A function uses its signature
			c.graph.markUsedBy(fn, fn.Type())

			// A function uses its return types
			sig := fn.Type().(*types.Signature)
			res := sig.Results()
			n := res.Len()
			for i := 0; i < n; i++ {
				c.graph.markUsedBy(res.At(i).Type(), fn)
			}
		}

		if obj, ok := obj.(interface {
			Scope() *types.Scope
			Pkg() *types.Package
		}); ok {
			scope := obj.Scope()
			c.graph.markUsedBy(c.topmostScope(scope, obj.Pkg()), obj)
		}

		if c.isRoot(obj) {
			node := c.graph.getNode(obj)
			c.graph.roots = append(c.graph.roots, node)
			if obj, ok := obj.(*types.PkgName); ok {
				scope := obj.Pkg().Scope()
				c.graph.markUsedBy(scope, obj)
			}
		}
	}
}

func (c *Checker) processUses(pkg *loader.PackageInfo) {
	for ident, usedObj := range pkg.Uses {
		if _, ok := usedObj.(*types.PkgName); ok {
			continue
		}
		pos := ident.Pos()
		scope := pkg.Pkg.Scope().Innermost(pos)
		scope = c.topmostScope(scope, pkg.Pkg)
		if scope != pkg.Pkg.Scope() {
			c.graph.markUsedBy(usedObj, scope)
		}

		switch usedObj.(type) {
		case *types.Var, *types.Const:
			c.graph.markUsedBy(usedObj.Type(), usedObj)
		}
	}
}

func (c *Checker) findExportedInterfaces() {
	c.interfaces = []*types.Interface{types.Universe.Lookup("error").Type().(*types.Named).Underlying().(*types.Interface)}
	var pkgs []*loader.PackageInfo
	for _, pkg := range c.lprog.AllPackages {
		pkgs = append(pkgs, pkg)
	}

	for _, pkg := range pkgs {
		for _, tv := range pkg.Types {
			iface, ok := tv.Type.(*types.Interface)
			if !ok {
				continue
			}
			if iface.NumMethods() == 0 {
				continue
			}
			c.interfaces = append(c.interfaces, iface)
		}
	}
}

func (c *Checker) processTypes(pkg *loader.PackageInfo) {
	named := map[*types.Named]*types.Pointer{}
	var interfaces []*types.Interface
	for _, tv := range pkg.Types {
		if typ, ok := tv.Type.(interface {
			Elem() types.Type
		}); ok {
			c.graph.markUsedBy(typ.Elem(), typ)
		}

		switch obj := tv.Type.(type) {
		case *types.Named:
			named[obj] = types.NewPointer(obj)
			c.graph.markUsedBy(obj, obj.Underlying())
			c.graph.markUsedBy(obj.Underlying(), obj)
		case *types.Interface:
			if obj.NumMethods() > 0 {
				interfaces = append(interfaces, obj)
			}
		case *types.Struct:
			c.useNoCopyFields(obj)
			if pkg.Pkg.Name() != "main" {
				c.useExportedFields(obj)
			}
		}
	}

	// Pretend that all types are meant to implement as many
	// interfaces as possible.
	//
	// TODO(dh): For normal operations, that's the best we can do, as
	// we have no idea what external users will do with our types. In
	// whole-program mode, we could be more conservative, in two ways:
	// 1) Only consider interfaces if a type has been assigned to one
	// 2) Use SSA and flow analysis and determine the exact set of
	// interfaces that is relevant.
	fn := func(iface *types.Interface) {
		for obj, objPtr := range named {
			if !types.Implements(obj, iface) && !types.Implements(objPtr, iface) {
				continue
			}
			ifaceMethods := make(map[string]struct{}, iface.NumMethods())
			n := iface.NumMethods()
			for i := 0; i < n; i++ {
				meth := iface.Method(i)
				ifaceMethods[meth.Name()] = struct{}{}
			}
			for _, obj := range []types.Type{obj, objPtr} {
				ms := c.msCache.MethodSet(obj)
				n := ms.Len()
				for i := 0; i < n; i++ {
					sel := ms.At(i)
					meth := sel.Obj().(*types.Func)
					_, found := ifaceMethods[meth.Name()]
					if !found {
						continue
					}
					c.graph.markUsedBy(meth.Type().(*types.Signature).Recv().Type(), obj) // embedded receiver
					if len(sel.Index()) > 1 {
						f := getField(obj, sel.Index()[0])
						c.graph.markUsedBy(f, obj) // embedded receiver
					}
					c.graph.markUsedBy(meth, obj)
				}
			}
		}
	}

	for _, iface := range interfaces {
		fn(iface)
	}
	for _, iface := range c.interfaces {
		fn(iface)
	}
}

func (c *Checker) processSelections(pkg *loader.PackageInfo) {
	fn := func(expr *ast.SelectorExpr, sel *types.Selection, offset int) {
		scope := pkg.Pkg.Scope().Innermost(expr.Pos())
		c.graph.markUsedBy(expr.X, c.topmostScope(scope, pkg.Pkg))
		c.graph.markUsedBy(sel.Obj(), expr.X)
		if len(sel.Index()) > 1 {
			typ := sel.Recv()
			indices := sel.Index()
			for _, idx := range indices[:len(indices)-offset] {
				obj := getField(typ, idx)
				typ = obj.Type()
				c.graph.markUsedBy(obj, expr.X)
			}
		}
	}

	for expr, sel := range pkg.Selections {
		switch sel.Kind() {
		case types.FieldVal:
			fn(expr, sel, 0)
		case types.MethodVal:
			fn(expr, sel, 1)
		}
	}
}

func dereferenceType(typ types.Type) types.Type {
	if typ, ok := typ.(*types.Pointer); ok {
		return typ.Elem()
	}
	return typ
}

// processConversion marks fields as used if they're part of a type conversion.
func (c *Checker) processConversion(pkg *loader.PackageInfo, node ast.Node) {
	if node, ok := node.(*ast.CallExpr); ok {
		callTyp := pkg.TypeOf(node.Fun)
		var typDst *types.Struct
		var ok bool
		switch typ := callTyp.(type) {
		case *types.Named:
			typDst, ok = typ.Underlying().(*types.Struct)
		case *types.Pointer:
			typDst, ok = typ.Elem().Underlying().(*types.Struct)
		default:
			return
		}
		if !ok {
			return
		}

		if typ, ok := pkg.TypeOf(node.Args[0]).(*types.Basic); ok && typ.Kind() == types.UnsafePointer {
			// This is an unsafe conversion. Assume that all the
			// fields are relevant (they are, because of memory
			// layout)
			n := typDst.NumFields()
			for i := 0; i < n; i++ {
				c.graph.markUsedBy(typDst.Field(i), typDst)
			}
			return
		}

		typSrc, ok := dereferenceType(pkg.TypeOf(node.Args[0])).Underlying().(*types.Struct)
		if !ok {
			return
		}

		// When we convert from type t1 to t2, were t1 and t2 are
		// structs, all fields are relevant, as otherwise the
		// conversion would fail.
		//
		// We mark t2's fields as used by t1's fields, and vice
		// versa. That way, if no code actually refers to a field
		// in either type, it's still correctly marked as unused.
		// If a field is used in either struct, it's implicitly
		// relevant in the other one, too.
		//
		// It works in a similar way for conversions between types
		// of two packages, only that the extra information in the
		// graph is redundant unless we're in whole program mode.
		n := typDst.NumFields()
		for i := 0; i < n; i++ {
			fDst := typDst.Field(i)
			fSrc := typSrc.Field(i)
			c.graph.markUsedBy(fDst, fSrc)
			c.graph.markUsedBy(fSrc, fDst)
		}
	}
}

// processCompositeLiteral marks fields as used if the struct is used
// in a composite literal.
func (c *Checker) processCompositeLiteral(pkg *loader.PackageInfo, node ast.Node) {
	// XXX how does this actually work? wouldn't it match t{}?
	if node, ok := node.(*ast.CompositeLit); ok {
		typ := pkg.TypeOf(node)
		if _, ok := typ.(*types.Named); ok {
			typ = typ.Underlying()
		}
		if _, ok := typ.(*types.Struct); !ok {
			return
		}

		if isBasicStruct(node.Elts) {
			c.markFields(typ)
		}
	}
}

// processCgoExported marks functions as used if they're being
// exported to cgo.
func (c *Checker) processCgoExported(pkg *loader.PackageInfo, node ast.Node) {
	if node, ok := node.(*ast.FuncDecl); ok {
		if node.Doc == nil {
			return
		}
		for _, cmt := range node.Doc.List {
			if !strings.HasPrefix(cmt.Text, "//go:cgo_export_") {
				return
			}
			obj := pkg.ObjectOf(node.Name)
			c.graph.roots = append(c.graph.roots, c.graph.getNode(obj))
		}
	}
}

func (c *Checker) processVariableDeclaration(pkg *loader.PackageInfo, node ast.Node) {
	if decl, ok := node.(*ast.GenDecl); ok {
		for _, spec := range decl.Specs {
			spec, ok := spec.(*ast.ValueSpec)
			if !ok {
				continue
			}
			for i, name := range spec.Names {
				if i >= len(spec.Values) {
					break
				}
				value := spec.Values[i]
				fn := func(node ast.Node) bool {
					if node3, ok := node.(*ast.Ident); ok {
						obj := pkg.ObjectOf(node3)
						if _, ok := obj.(*types.PkgName); ok {
							return true
						}
						c.graph.markUsedBy(obj, pkg.ObjectOf(name))
					}
					return true
				}
				ast.Inspect(value, fn)
			}
		}
	}
}

func (c *Checker) processArrayConstants(pkg *loader.PackageInfo, node ast.Node) {
	if decl, ok := node.(*ast.ArrayType); ok {
		ident, ok := decl.Len.(*ast.Ident)
		if !ok {
			return
		}
		c.graph.markUsedBy(pkg.ObjectOf(ident), pkg.TypeOf(decl))
	}
}

func (c *Checker) processKnownReflectMethodCallers(pkg *loader.PackageInfo, node ast.Node) {
	call, ok := node.(*ast.CallExpr)
	if !ok {
		return
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	if !ok {
		return
	}
	if types.TypeString(pkg.TypeOf(sel.X), nil) != "*net/rpc.Server" {
		x, ok := sel.X.(*ast.Ident)
		if !ok {
			return
		}
		pkgname, ok := pkg.ObjectOf(x).(*types.PkgName)
		if !ok {
			return
		}
		if pkgname.Imported().Path() != "net/rpc" {
			return
		}
	}

	var arg ast.Expr
	switch sel.Sel.Name {
	case "Register":
		if len(call.Args) != 1 {
			return
		}
		arg = call.Args[0]
	case "RegisterName":
		if len(call.Args) != 2 {
			return
		}
		arg = call.Args[1]
	}
	typ := pkg.TypeOf(arg)
	ms := types.NewMethodSet(typ)
	for i := 0; i < ms.Len(); i++ {
		c.graph.markUsedBy(ms.At(i).Obj(), typ)
	}
}

func (c *Checker) processAST(pkg *loader.PackageInfo) {
	fn := func(node ast.Node) bool {
		c.processConversion(pkg, node)
		c.processKnownReflectMethodCallers(pkg, node)
		c.processCompositeLiteral(pkg, node)
		c.processCgoExported(pkg, node)
		c.processVariableDeclaration(pkg, node)
		c.processArrayConstants(pkg, node)
		return true
	}
	for _, file := range pkg.Files {
		ast.Inspect(file, fn)
	}
}

func isBasicStruct(elts []ast.Expr) bool {
	for _, elt := range elts {
		if _, ok := elt.(*ast.KeyValueExpr); !ok {
			return true
		}
	}
	return false
}

func isPkgScope(obj types.Object) bool {
	return obj.Parent() == obj.Pkg().Scope()
}

func isMain(obj types.Object) bool {
	if obj.Pkg().Name() != "main" {
		return false
	}
	if obj.Name() != "main" {
		return false
	}
	if !isPkgScope(obj) {
		return false
	}
	if !isFunction(obj) {
		return false
	}
	if isMethod(obj) {
		return false
	}
	return true
}

func isFunction(obj types.Object) bool {
	_, ok := obj.(*types.Func)
	return ok
}

func isMethod(obj types.Object) bool {
	if !isFunction(obj) {
		return false
	}
	return obj.(*types.Func).Type().(*types.Signature).Recv() != nil
}

func (c *Checker) checkFlags(v interface{}) bool {
	return true
}

func (c *Checker) isRoot(obj types.Object) bool {
	// - in local mode, main, init, tests, and non-test, non-main exported are roots
	// - in global mode (not yet implemented), main, init and tests are roots

	if _, ok := obj.(*types.PkgName); ok {
		return true
	}

	if isMain(obj) || (isFunction(obj) && !isMethod(obj) && obj.Name() == "init") {
		return true
	}
	if obj.Exported() {
		f := c.lprog.Fset.Position(obj.Pos()).Filename
		if strings.HasSuffix(f, "_test.go") {
			return strings.HasPrefix(obj.Name(), "Test") ||
				strings.HasPrefix(obj.Name(), "Benchmark") ||
				strings.HasPrefix(obj.Name(), "Example")
		}

		// Package-level are used, except in package main
		if isPkgScope(obj) && obj.Pkg().Name() != "main" {
			return true
		}
	}
	return false
}

func markNodesUsed(nodes map[*graphNode]struct{}) {
	for node := range nodes {
		wasUsed := node.used
		node.used = true
		if !wasUsed {
			markNodesUsed(node.uses)
		}
	}
}

func (c *Checker) markNodesQuiet() {
	for _, node := range c.graph.nodes {
		if node.used {
			continue
		}
		if obj, ok := node.obj.(types.Object); ok && !c.checkFlags(obj) {
			node.quiet = true
			continue
		}
		c.markObjQuiet(node.obj)
	}
}

func (c *Checker) markObjQuiet(obj interface{}) {
	switch obj := obj.(type) {
	case *types.Named:
		n := obj.NumMethods()
		for i := 0; i < n; i++ {
			meth := obj.Method(i)
			node := c.graph.getNode(meth)
			node.quiet = true
			c.markObjQuiet(meth.Scope())
		}
	case *types.Struct:
		n := obj.NumFields()
		for i := 0; i < n; i++ {
			field := obj.Field(i)
			c.graph.nodes[field].quiet = true
		}
	case *types.Func:
		c.markObjQuiet(obj.Scope())
	case *types.Scope:
		if obj == nil {
			return
		}
		if obj.Parent() == types.Universe {
			return
		}
		for _, name := range obj.Names() {
			v := obj.Lookup(name)
			if n, ok := c.graph.nodes[v]; ok {
				n.quiet = true
			}
		}
		n := obj.NumChildren()
		for i := 0; i < n; i++ {
			c.markObjQuiet(obj.Child(i))
		}
	}
}

func getField(typ types.Type, idx int) *types.Var {
	switch obj := typ.(type) {
	case *types.Pointer:
		return getField(obj.Elem(), idx)
	case *types.Named:
		switch v := obj.Underlying().(type) {
		case *types.Struct:
			return v.Field(idx)
		case *types.Pointer:
			return getField(v.Elem(), idx)
		default:
			panic(fmt.Sprintf("unexpected type %s", typ))
		}
	case *types.Struct:
		return obj.Field(idx)
	}
	return nil
}

func (c *Checker) topmostScope(scope *types.Scope, pkg *types.Package) (ret *types.Scope) {
	if top, ok := c.topmostCache[scope]; ok {
		return top
	}
	defer func() {
		c.topmostCache[scope] = ret
	}()
	if scope == pkg.Scope() {
		return scope
	}
	if scope.Parent().Parent() == pkg.Scope() {
		return scope
	}
	return c.topmostScope(scope.Parent(), pkg)
}
