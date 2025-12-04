package main

import (
	"aoc/lib"
	"fmt"
)

func main() {
	// read input from either stdin or file
	input := lib.MustReadInput()

	// pass to solve func
	result := solve(input)

	// print result
	fmt.Println(result)
}

func solve(input string) int {
	reports := lib.ReadReports(input)
	res := 0
	for _, report := range reports {
		if lib.IsReportKindaSafe(report) {
			fmt.Println("kinda     safe:", report)
			res++
		} else {
			fmt.Println("Kinda NOT safe:", report)
		}
	}
	return res
}
