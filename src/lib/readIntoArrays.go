package lib

import (
	"encoding/csv"
	"fmt"
	"strconv"
	"strings"
)

func ReadCsvIntoArrays(input string) ([][]int, error) {
	reader := csv.NewReader(strings.NewReader(input))
	records, err := reader.ReadAll()
	if err != nil {
		return nil, fmt.Errorf("failed to read CSV: %w", err)
	}

	result := make([][]int, 0, len(records))
	for i, record := range records {
		row := make([]int, 0, len(record))
		for j, val := range record {
			val = strings.TrimSpace(val)
			if val == "" {
				continue
			}
			num, err := strconv.Atoi(val)
			if err != nil {
				return nil, fmt.Errorf("invalid number at row %d, column %d: %s", i+1, j+1, val)
			}
			row = append(row, num)
		}
		if len(row) > 0 {
			result = append(result, row)
		}
	}

	return result, nil
}

func ReadIntoArrays(input string) ([]int, []int, error) {
	// First try CSV format
	arrays, err := ReadCsvIntoArrays(input)
	if err != nil {
		// If CSV fails, try space-separated format
		arrays = make([][]int, 0)
		for _, line := range strings.Split(strings.TrimSpace(input), "\n") {
			if line == "" {
				continue
			}

			nums := strings.Fields(line)
			if len(nums) != 2 {
				return nil, nil, fmt.Errorf("expected 2 numbers per line, got %d numbers in line: %s", len(nums), line)
			}

			row := make([]int, 2)
			for i, numStr := range nums {
				num, err := strconv.Atoi(numStr)
				if err != nil {
					return nil, nil, fmt.Errorf("invalid number at row %d, column %d: %s", len(arrays)+1, i+1, numStr)
				}
				row[i] = num
			}
			arrays = append(arrays, row)
		}
	}

	// Validate that we have at least one row
	if len(arrays) == 0 {
		return nil, nil, fmt.Errorf("no valid data found in input")
	}

	// Split into two separate arrays
	col1 := make([]int, len(arrays))
	col2 := make([]int, len(arrays))
	for i, row := range arrays {
		col1[i] = row[0]
		col2[i] = row[1]
	}

	return col1, col2, nil
}
