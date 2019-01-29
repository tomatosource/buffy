package main

import (
	"fmt"
)

func main() {
	p, err := loadProgram()
	if err != nil {
		panic(err)
	}

	for _, unused := range Check(p) {
		fmt.Println(unused.String())
	}
}
