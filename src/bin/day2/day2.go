package main

import (
	"aoc/lib"
	"os"
)

func main() {
	// read file
	fname := os.Args[1]
	input, err := os.ReadFile(fname)

	if err != nil {
		panic(err)
	}
	// pass to solve func
	result := solve(string(input))

	// print result
	println(result)
}

func solve(input string) int {
	reports := lib.ReadReports(input)
	res := 0
	for _, report := range reports {
		if lib.IsReportSafe(report) {
			res++
		}
	}
	return res
}
