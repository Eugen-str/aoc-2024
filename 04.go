package main

import (
	"bufio"
	"fmt"
	"os"
	"slices"
)

func get_input(filename string) ([]string, int, int) {
	var lines []string

	file, err := os.Open(filename)

	if err != nil {
		panic(err)
	}

	read := bufio.NewScanner(file)
	n_lines := 0
	for read.Scan() {
		line := read.Text()
		lines = append(lines, "..."+line+"...")
		n_lines++
	}
	s_len := len(lines[0])

	var padding []byte = make([]byte, s_len)
	for i := 0; i < s_len; i++ {
		padding[i] = '.'
	}

	str_padding := string(padding[:])

	lines = slices.Insert(lines, 0, str_padding)
	lines = slices.Insert(lines, 0, str_padding)
	lines = slices.Insert(lines, 0, str_padding)

	lines = append(lines, str_padding)
	lines = append(lines, str_padding)
	lines = append(lines, str_padding)

	return lines, s_len, n_lines
}

func match_part1(i int, j int, dx int, dy int, input []string) bool {
	str := "XMAS"

	for l := 0; l < 4; l++ {
		if input[j][i] != str[l] {
			return false
		}
		i += dx
		j += dy
	}
	return true
}

func solve_part1(input []string, x int, y int) int {
	res := 0
	for j := 0; j < y+3; j++ {
		for i := 0; i < x; i++ {

			if input[j][i] == 'X' {
				for dx := -1; dx <= 1; dx++ {
					for dy := -1; dy <= 1; dy++ {
						if match_part1(i, j, dx, dy, input) {
							res++
						}
					}
				}
			}

		}
	}
	return res
}

func match_part2(i int, j int, input []string) bool {
	var a1 []byte = make([]byte, 3)
	var a2 []byte = make([]byte, 3)

	for l := 0; l < 3; l++ {
		a1[l] = input[j+l][i+l]
		a2[l] = input[j+l][i+2-l]
	}

	s1 := string(a1[:])
	s2 := string(a2[:])

	return (s1 == "MAS" || s1 == "SAM") && (s2 == "MAS" || s2 == "SAM")
}

func solve_part2(input []string, x int, y int) int {
	res := 0
	for j := 0; j < y+3; j++ {
		for i := 0; i < x; i++ {

			if input[j][i] == 'M' || input[j][i] == 'S' {
				if match_part2(i, j, input) {
					res++
				}
			}

		}
	}
	return res
}

func main() {
	input, str_len, n_lines := get_input("inputs/04.txt")

	fmt.Println(solve_part1(input, str_len, n_lines))
	fmt.Println(solve_part2(input, str_len, n_lines))
}
