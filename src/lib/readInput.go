package lib

import (
	"fmt"
	"io"
	"log"
	"os"
)

// ReadInput reads from either stdin or a file specified in os.Args[1]
// Returns the input as a string and any error encountered
func ReadInput() (string, error) {
	// Check if we're getting data from stdin
	stat, _ := os.Stdin.Stat()
	if (stat.Mode() & os.ModeCharDevice) == 0 {
		// Read from stdin
		bytes, err := io.ReadAll(os.Stdin)
		if err != nil {
			return "", fmt.Errorf("failed to read from stdin: %w", err)
		}
		return string(bytes), nil
	}

	// No stdin, check for filename argument
	if len(os.Args) < 2 {
		return "", fmt.Errorf("usage: %s <input_file> or provide input via stdin", os.Args[0])
	}

	// Read from file
	bytes, err := os.ReadFile(os.Args[1])
	if err != nil {
		return "", fmt.Errorf("failed to read file %s: %w", os.Args[1], err)
	}
	return string(bytes), nil
}

// MustReadInput is like ReadInput but panics on error
func MustReadInput() string {
	input, err := ReadInput()
	if err != nil {
		log.Fatal(err)
	}
	return input
}
