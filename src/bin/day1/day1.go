package main

import (
	"aoc/lib"
	"sort"
)

func main() {
	// read input from either stdin or file
	input := lib.MustReadInput()

	// pass to solve func
	result := solve1(input)

	// print result
	println(result)
}

func absInt(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func solve1(input string) int {
	// read into arrays
	a, b, err := lib.ReadIntoArrays(input)
	if err != nil {
		panic(err)
	}

	sort.Ints(a)
	sort.Ints(b)
	res := 0
	for i := 0; i < len(a); i++ {
		res += absInt(a[i] - b[i])
	}

	return res
}
