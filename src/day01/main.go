package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

const START = 50
const LIMIT = 100

func readInput[T any](f *os.File, fnc func(string) T) []T {
	var res []T
	var s string
	for {
		_, err := fmt.Fscanf(f, "%s", &s)
		if err != nil {
			break
		}
		res = append(res, fnc(s))
	}
	return res
}

func decode(s string) int {
	if strings.HasPrefix(s, "R") {
		num, err := strconv.Atoi(s[1:])
		if err != nil {
			panic(err)
		}
		return num
	} else if strings.HasPrefix(s, "L") {
		num, err := strconv.Atoi(s[1:])
		if err != nil {
			panic(err)
		}
		return -num
	}
	panic("invalid input")
}

func rotate(num int) int {
	for num < 0 {
		num += LIMIT
	}
	return num % LIMIT
}

func dial(seq []int) []int {
	curr := START
	res := make([]int, 1, len(seq)+1)
	res[0] = curr
	for _, v := range seq {
		curr = rotate(curr + v)
		res = append(res, curr)
	}
	return res
}

func countRotations(seq []int) int {
	count := 0
	prev := START
	for _, v := range seq {
		curr := prev + v
		if prev == 0 {
			if curr <= -LIMIT {
				count += -curr / LIMIT
			} else {
				count += curr / LIMIT
			}
		} else if curr <= 0 {
			count += -curr/LIMIT + 1
		} else if curr >= LIMIT {
			count += curr / LIMIT
		}
		prev = rotate(curr)
	}
	return count
}

func countZeroes(seq []int) int {
	count := 0
	for _, v := range seq {
		if v == 0 {
			count++
		}
	}
	return count
}

func main() {
	f, err := os.Open("input/day01/input1.txt")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	seq := readInput(f, decode)
	fmt.Println("PART 1:", countZeroes(dial(seq)))
	fmt.Println("PART 2:", countRotations(seq))
}
