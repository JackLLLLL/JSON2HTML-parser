package main

import (
	"os"
	"io/ioutil"
	"fmt"
)

func main()  {
	// Get input file name
	filename := os.Args[1]

	// Parser to get list of tokens
	tokens := parser(filename)

	// Output tokens in Html format
	colorizer(tokens)
}

func parser(filename string) []string {
	// Read in file line by line
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	// New tokens
	var tokens []string

	for l := 0; l < len(content); l++ {
		c := content[l]
		switch c {
		
		case '{':
			tokens = append(tokens, "{")
		case '}':
			tokens = append(tokens, "}")
		case '[':
			tokens = append(tokens, "[")
		case ']':
			tokens = append(tokens, "]")
		case ':':
			tokens = append(tokens, ":")
		case ',':
			tokens = append(tokens, ",")
		case 't':
			tokens = append(tokens, "true")
			l += 3
		case 'f':
			tokens = append(tokens, "false")
			l += 4
		case 'n':
			tokens = append(tokens, "null")
			l += 3
		case '"':
			temp := "&quot;"
			for l++; l < len(content) && content[l] != '"'; l++ {
				switch content[l] {
				case '\\':
					if content[l+1] == '"' {
						temp += "<span style=\"color:Gray\">&quot;</span>"
						l++
					} else {
						temp += "<span style=\"color:Gray\">"
						temp += "\\"
						for l++; l < len(content) && content[l] != ' ' && content[l] != '\\' && content[l] != '"'; l++ {
							temp += string(content[l])
						}
						temp += "</span>"
						l--
					}
				case '>':
					temp += "&gt;"
				case '<':
					temp += "&lt;"
				case '&':
					temp += "&amp;"
				case '\'':
					temp += "&apos;"
				default:
					temp += string(content[l])
				}
			}
			temp += "&quot;"
			tokens = append(tokens, temp)
		case '-', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0':
			temp := ""
			for ; l < len(content) && content[l] != ',' && content[l] != ']' && content[l] != '}' && content[l] != '\t' && content[l] != '\n' && content[l] != '\r'; l++ {
				temp += string(content[l])
			}
			l--
			tokens = append(tokens, temp)
		}
	}

	return tokens
}

func colorizer(tokens []string)  {
	// To preserve indentation
	fmt.Printf("<span style=\"font-family:monospace; white-space:pre\">\n")

	// Colorize each token
	indent := ""
	for i:=0; i< len(tokens); i++ {
		s := tokens[i][0]
		switch s {
		case '{':
			fmt.Printf("\n%s", indent)
			fmt.Printf("<span style=\"color:red\">")
			fmt.Printf("%s", tokens[i])
			fmt.Printf("</span>")
			fmt.Printf("\n")
			indent += "\t"
			fmt.Printf("%s", indent)
		case '}':
			indent = indent[0:len(indent)-1]
			fmt.Printf("\n%s", indent)
			fmt.Printf("<span style=\"color:red\">")
			fmt.Printf("%s", tokens[i])
			fmt.Printf("</span>")
			fmt.Printf("\n")
			fmt.Printf("%s", indent)
		case '[':
			fmt.Printf("<span style=\"color:DodgerBlue\">")
			fmt.Printf("%s ", tokens[i])
			fmt.Printf("</span>")
		case ']':
			fmt.Printf("<span style=\"color:DodgerBlue\">")
			fmt.Printf("%s ", tokens[i])
			fmt.Printf("</span>")
		case ':':
			fmt.Printf("<span style=\"color:Tomato\">")
			fmt.Printf("%s ", tokens[i])
			fmt.Printf("</span>")
		case ',':
			fmt.Printf("<span style=\"color:Violet\">")
			fmt.Printf("%s", tokens[i])
			fmt.Printf("</span>")
			fmt.Printf("\n")
			fmt.Printf("%s", indent)
		case '&':
			fmt.Printf("<span style=\"color:MediumSeaGreen\">")
			fmt.Printf("%s ", tokens[i])
			fmt.Printf("</span>")
		case 't', 'f', 'n':
			fmt.Printf("<span style=\"color:SlateBlue\">")
			fmt.Printf("%s ", tokens[i])
			fmt.Printf("</span>")
		default:
			fmt.Printf("<span style=\"color:Orange\">")
			fmt.Printf("%s ", tokens[i])
			fmt.Printf("</span>")
		}
	}

	// To preserve indentation
	fmt.Printf("</span>")
}
