package main

import (
	"aoc/lib"
	"fmt"
	"sort"
)

func main() {
	// read input from either stdin or file
	input := lib.MustReadInput()

	// pass to solve func
	result := solve(input)

	// print result
	fmt.Println(result)
}

// binarySearch performs a binary search on a sorted slice of integers
// nums: sorted slice of integers to search in
// target: value to search for
// returns: index of target if found, -1 if not found
func binarySearch(nums []int, target int) int {
	left, right := 0, len(nums)-1 // Initialize left and right bounds
	for left <= right {
		middle := (left + right) / 2 // Calculate midpoint
		if nums[middle] == target {
			return middle // Found target
		} else if nums[middle] < target {
			left = middle + 1 // Target is in right half
		} else {
			right = middle - 1 // Target is in left half
		}
	}
	return -1 // Target not found
}

func binaryCount(nums []int, target int) int {
	i := binarySearch(nums, target)
	if i == -1 {
		return 0
	}

	minus := 0
	plus := 0
	for i-minus-1 >= 0 && nums[i-minus-1] == target {
		minus++
	}
	for i+plus+1 < len(nums) && nums[i+plus+1] == target {
		plus++
	}

	return plus + 1 + minus
}

func solve(input string) int {
	a, b, err := lib.ReadIntoArrays(input)
	if err != nil {
		panic(err)
	}
	sort.Ints(b)
	res := 0

	for _, num := range a {
		res += (num * binaryCount(b, num))
	}
	return res
}
