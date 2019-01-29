package main

import (
	"fmt"
	"sort"
)

func main() {
	p, err := loadProgram()
	if err != nil {
		panic(err)
	}

	var unused []string
	for _, u := range Check(p) {
		unused = append(unused, u.String())
	}
	// consistent output:
	sort.Strings(unused)
	for _, u := range unused {
		fmt.Println(u)
	}
}
