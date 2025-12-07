package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
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

	fmt.Println("PART 1:", solve1(lines))
	fmt.Println("PART 2:", solve2(lines))
}

func readInput1(lines []string) ([][]int64, []string) {
	operands := make([][]int64, 0)
	var ops []string
	for _, s := range lines {
		line := make([]int64, 0)
		for _, part := range strings.Fields(s) {
			var n int64
			n, err := strconv.ParseInt(part, 10, 64)
			if err != nil {
				break
			}
			line = append(line, n)
		}
		if len(line) == 0 {
			ops = strings.Fields(strings.TrimSpace(s))
			break
		}
		operands = append(operands, line)
	}
	return operands, ops
}

func solve1(lines []string) int64 {
	operands, ops := readInput1(lines)
	res := make([]int64, len(operands[0]))
	for i, op := range ops {
		switch op {
		case "+":
			sum := int64(0)
			for _, line := range operands {
				sum += line[i]
			}
			res[i] = sum
			i++
		case "*":
			prod := int64(1)
			for _, line := range operands {
				prod *= line[i]
			}
			res[i] = prod
			i++
		default:
			continue
		}
	}

	grandTotal := int64(0)
	for _, v := range res {
		grandTotal += v
	}
	return grandTotal
}

type Problem struct {
	operands []int64
	op       string
}

func (p Problem) calculate() int64 {
	switch p.op {
	case "+":
		sum := int64(0)
		for _, operand := range p.operands {
			sum += operand
		}
		return sum
	case "*":
		prod := int64(1)
		for _, operand := range p.operands {
			prod *= operand
		}
		return prod
	default:
		panic("invalid operator")
	}
}

type Problems []Problem

func (ps Problems) grandTotal() int64 {
	total := int64(0)
	for _, p := range ps {
		total += p.calculate()
	}
	return total
}

func readInput2(lines []string) Problems {
	ops := strings.Fields(lines[len(lines)-1])

	limit := 0
	for _, line := range lines {
		limit = max(limit, len(line))
	}

	res := make([]Problem, 0)
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
			res = append(res, Problem{operands: operands, op: ops[i]})
			operands = make([]int64, 0)
			i++
		} else {
			operands = append(operands, num)
		}
	}
	return res
}

func solve2(lines []string) int64 {
	problems := readInput2(lines)
	return problems.grandTotal()
}
