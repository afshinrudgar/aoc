package main

import (
	"bufio"
	"fmt"
	"os"
	"slices"
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

	lines := readLines(f)
	fmt.Println("PART 1:", solve1(lines))
	fmt.Println("PART 2:", solve2(lines))
}

func readLines(f *os.File) []string {
	lines := make([]string, 0)
	reader := bufio.NewReader(f)
	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			break
		}
		lines = append(lines, strings.TrimSpace(line))
	}
	return lines
}

func splitOn(beams []int, pos int) []int {
	newBeams := make([]int, 0)
	for _, idx := range beams {
		if idx != pos {
			newBeams = append(newBeams, idx)
		}
	}
	if !slices.Contains(newBeams, pos-1) {
		newBeams = append(newBeams, pos-1)
	}
	if !slices.Contains(newBeams, pos+1) {
		newBeams = append(newBeams, pos+1)
	}
	return newBeams
}

func solve1(lines []string) int {
	var beams []int = []int{strings.Index(lines[0], "S")}
	splitCount := 0
	for _, line := range lines[1:] {
		for _, idx := range beams {
			if line[idx] == '^' {
				beams = splitOn(beams, idx)
				splitCount++
			}
		}
	}
	return splitCount
}

func countTimelines(lines []string, idx int, beam int, cache [][]int) int {
	if idx == len(lines) {
		return 1
	}

	if cache[idx][beam] != 0 {
		return cache[idx][beam]
	}

	res := 0
	if lines[idx][beam] == '^' {
		if beam > 0 {
			res += countTimelines(lines, idx+1, beam-1, cache)
		}
		if beam < len(lines[idx])-1 {
			res += countTimelines(lines, idx+1, beam+1, cache)
		}
	} else {
		res += countTimelines(lines, idx+1, beam, cache)
	}
	cache[idx][beam] = res
	return res
}

func solve2(lines []string) int {
	originalBeam := strings.Index(lines[0], "S")
	cache := make([][]int, len(lines))
	for i := range len(lines) {
		cache[i] = make([]int, len(lines[i]))
	}
	return countTimelines(lines, 1, originalBeam, cache)
}
