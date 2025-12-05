package main

import (
	"slices"
	"testing"
)

func TestPrimeFactors(t *testing.T) {
	tests := []struct {
		n        int
		expected []int
	}{
		{1, []int{}},
		{2, []int{2}},
		{3, []int{3}},
		{4, []int{2, 4}},
		{6, []int{2, 3, 6}},
		{12, []int{2, 3, 4, 6, 12}},
		{18, []int{2, 3, 6, 9, 18}},
		{25, []int{5, 25}},
		{17, []int{17}},
		{100, []int{2, 4, 5, 10, 20, 25, 50, 100}},
		{210, []int{2, 3, 5, 6, 7, 10, 14, 15, 21, 30, 35, 42, 70, 105, 210}},
	}

	for _, tt := range tests {
		res := primeFactors(tt.n)
		if !slices.Equal(res, tt.expected) {
			t.Errorf("primeFactors(%d) = %v, want %v", tt.n, res, tt.expected)
		}
	}
}
