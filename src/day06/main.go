package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/afshinrudgar/aoc/src/day06/lib"
)

func main() {
	var f *os.File
	var err error
	if len(os.Args) > 1 {
		f, err = os.Open(os.Args[1])
		if err != nil {
			panic(err)
		}
		defer f.Close()
	} else {
		f = os.Stdin
	}

	lines := make([]string, 0)
	reader := bufio.NewReader(f)
	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			break
		}
		lines = append(lines, line)
	}

	fmt.Println("PART 1:", solve(lines, readInput1))
	fmt.Println("PART 2:", solve(lines, readInput2))
}

func readInput1(lines []string) []lib.Problem {
	rows := make([][]int64, len(lines)-1)
	ops := strings.Fields(lines[len(lines)-1])
	for i, line := range lines[:len(lines)-1] {
		nums := make([]int64, 0)
		for _, part := range strings.Fields(line) {
			var n int64
			n, err := strconv.ParseInt(part, 10, 64)
			if err != nil {
				break
			}
			nums = append(nums, n)
		}
		rows[i] = nums
	}
	res := make([]lib.Problem, 0)
	for i, op := range ops {
		operands := make([]int64, 0)
		for _, row := range rows {
			operands = append(operands, row[i])
		}
		res = append(res, lib.New(operands, op))
	}
	return res
}
func readInput2(lines []string) []lib.Problem {
	ops := strings.Fields(lines[len(lines)-1])

	limit := 0
	for _, line := range lines {
		limit = max(limit, len(line))
	}

	res := make([]lib.Problem, 0)
	i := 0
	operands := make([]int64, 0)
	for j := 0; j < limit; j++ {
		num := int64(0)
		for k := 0; k < len(lines)-1; k++ {
			c := lines[k][j]
			if c >= '0' && c <= '9' {
				num = num*10 + int64(c-'0')
			}
		}
		if num == 0 {
			res = append(res, lib.New(operands, ops[i]))
			operands = make([]int64, 0)
			i++
		} else {
			operands = append(operands, num)
		}
	}
	return res
}

func solve(lines []string, fnc func([]string) []lib.Problem) int64 {
	problems := fnc(lines)
	return lib.GrandTotal(problems)
}
