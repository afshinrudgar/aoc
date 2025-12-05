package main

import (
	"fmt"
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
)

type Range struct {
	min uint64
	max uint64
}

func (r Range) minNumDigits() int {
	return len(strconv.FormatUint(r.min, 10))
}

func (r Range) maxNumDigits() int {
	return len(strconv.FormatUint(r.max, 10))
}

func (r Range) produceInvalidIds() []uint64 {
	var res []uint64
	minNumDigits := r.minNumDigits()
	maxNumDigits := r.maxNumDigits()

	for numDigits := minNumDigits; numDigits <= maxNumDigits; numDigits++ {

		if numDigits%2 == 1 {
			continue
		}

		partNumDigits := numDigits / 2
		minPart := uint32(math.Pow10(partNumDigits - 1))
		maxPart := uint32(math.Pow10(partNumDigits)) - 1

		for i := minPart; i <= maxPart; i++ {
			part := strconv.FormatUint(uint64(i), 10)
			num, err := strconv.ParseUint(part+part, 10, 64)
			if err != nil {
				panic(err)
			}
			if num < r.min {
				continue
			}
			if num > r.max {
				break
			}
			res = append(res, num)
		}
	}
	return res
}

func primeFactors(n int) []int {
	if n < 2 {
		return []int{}
	}
	res := []int{n}
	i := 2
	for i*i <= n {
		if n%i == 0 {
			res = append(res, i)
			if i != n/i {
				res = append(res, n/i)
			}
		}
		i++
	}
	return res
}

func (r Range) produceInvalidIds2() []uint64 {
	var res []uint64
	minNumDigits := r.minNumDigits()
	maxNumDigits := r.maxNumDigits()

	for numDigits := minNumDigits; numDigits <= maxNumDigits; numDigits++ {

		for _, factor := range primeFactors(numDigits) {

			partNumDigits := numDigits / factor
			minPart := uint32(math.Pow10(partNumDigits - 1))
			maxPart := uint32(math.Pow10(partNumDigits)) - 1

			for i := minPart; i <= maxPart; i++ {
				part := strconv.FormatUint(uint64(i), 10)
				num, err := strconv.ParseUint(strings.Repeat(part, factor), 10, 64)
				if err != nil {
					panic(err)
				}
				if num < r.min {
					continue
				}
				if num > r.max {
					break
				}
				if !slices.Contains(res, num) {
					res = append(res, num)
				}
			}
		}
	}
	return res
}

func parseRange(s string) Range {
	var min, max uint64
	fmt.Sscanf(s, "%d-%d", &min, &max)
	return Range{min, max}
}

func readInput(f *os.File) []Range {
	var res []Range
	var s string
	fmt.Fscanf(f, "%s", &s)
	for part := range strings.SplitSeq(s, ",") {
		res = append(res, parseRange(part))
	}
	return res
}

func solve1(ranges []Range) uint64 {
	sum := uint64(0)
	for _, r := range ranges {
		for _, invalidId := range r.produceInvalidIds() {
			sum += invalidId
		}
	}
	return sum
}

func solve2(ranges []Range) uint64 {
	sum := uint64(0)
	for _, r := range ranges {
		for _, invalidId := range r.produceInvalidIds2() {
			sum += invalidId
		}
	}
	return sum
}

func main() {
	f, err := os.Open("input/day02/input.txt")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	ranges := readInput(f)

	fmt.Println("PART 1:", solve1(ranges))
	fmt.Println("PART 2:", solve2(ranges))
}
