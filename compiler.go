package main

import "fmt"
import "errors"
import "strconv"
import "os"
import "os/exec"
import "io/ioutil"
import "time"
import "github.com/davecgh/go-spew/spew"

type Token struct {
    Type TokenType
    Content string    // the token itself, e.g. a number 3.7 is stored here as "3.7"
    LineNumber int    // first line is line 1
    Column int        // first character of a line is in column 1
}

type TokenType uint

const IndentSpaces = 4


type TopDef struct {

}

type Statement interface {

}


type ReaderData interface {
    ReaderData()
}

type ParenList struct {
    elements []ReaderData
    LineNumber int
    Column int
}

type SquareList struct {
    elements []ReaderData
    LineNumber int
    Column int
}

type CurlyList struct {
    elements []ReaderData
    LineNumber int
    Column int
}

type Catena struct {
    elements []ReaderData
    LineNumber int
    Column int
}

type Symbol struct {
    Content string
    LineNumber int
    Column int
}

type NumberAtom struct {
    Content string
    LineNumber int
    Column int
}

type StringAtom struct {
    Content string      // includes enclosing quote marks 
    LineNumber int
    Column int
}

type SigilAtom struct {
    Content string
    LineNumber int
    Column int
}

func (a ParenList) ReaderData() {}
func (a SquareList) ReaderData() {}
func (a CurlyList) ReaderData() {}
func (a Catena) ReaderData() {}
func (a Symbol) ReaderData() {}
func (a NumberAtom) ReaderData() {}
func (a StringAtom) ReaderData() {}
func (a SigilAtom) ReaderData() {}

var debug = fmt.Println  // alias for debug printing
var itoa = strconv.Itoa 

// not including nil, true, false, and the operators
var reservedWords = []string{
    "func",
    "efunc",
    "meth",
    "emeth",
    "struct",
    "estruct",
    "interface",
    "einterface",
    "var",
    "evar",
    "const",
    "econst",
    "if",
    "for",
    "while",
    "switch",
    "select",
    "return",
}

var operatorWords = []string{
    "add",
    "sub",
    "mul",
    "div",
    "mod",
    "eq",
    "neq",
    "not",
    "lt",
    "gt",
    "lte",
    "gte",
    "append",
    "make",
    "new",
    "len",
    "or",
    "and",
    "xor",
    "bor",
    "band",
    "bxor",
    "inc",
    "dec",
    // assignment operators
    "as",
    "asadd",
    "asmul",
    "asdiv",
    "assub",
    "asinc",
    "asdec",     
}

var sigils = []rune{
    '.',
    ',',
    '~',
    '!',
    '@',
    '#',
    '$',
    '%',
    '^',
    '&',
    '*',
    '-',
    '+',
    '=',
    '/',
    '\\',
    ':',
    '\'',
}

const(
    Word TokenType = iota
    Newline
    Indentation
    Spaces
    OpenParen
    CloseParen
    OpenSquare
    CloseSquare
    OpenCurly
    CloseCurly
    NumberLiteral
    StringLiteral
    Sigil
)

// returns true if rune is a letter of the English alphabet
func isAlpha(r rune) bool {
    return (r >= 65 && r <= 90) || (r >= 97 && r <= 122)
}

// returns true if rune is a numeral
func isNumeral(r rune) bool {
    return (r >= 48 && r <= 57)
}

func isSigil(r rune) bool {
    for _, s := range sigils {
        if r == s {
            return true
        }
    }
    return false
}

func lex(code string) ([]Token, error) {
    tokens := make([]Token, 0)
    runes := []rune(code)
    line := 1
    column := 1
    for i := 0; i < len(runes); {
        r := runes[i]
        if r >= 128 {
            return nil, errors.New("File improperly contains a non-ASCII character at line " + itoa(line) + " and column " + itoa(column))
        }
        if r == '\n' {
            tokens = append(tokens, Token{Newline, "\n", line, column})
            line += 1
            column = 1
            i++
        } else if r == ';' {  // start of a comment
            for runes[i] != '\n' {
                column++
                i++
            }
            tokens = append(tokens, Token{Newline, "\n", line, column})
            i++
            line += 1
            column = 1
        } else if r == '(' {
            tokens = append(tokens, Token{OpenParen, "(", line, column})
            column++
            i++
        } else if r == ')' {
            tokens = append(tokens, Token{CloseParen, ")", line, column})
            column++
            i++
        } else if r == '[' {
            tokens = append(tokens, Token{OpenSquare, "[", line, column})
            column++
            i++
        } else if r == ']' {
            tokens = append(tokens, Token{CloseSquare, "]", line, column})
            column++
            i++
        } else if r == '{' {
            tokens = append(tokens, Token{OpenParen, "{", line, column})
            column++
            i++
        } else if r == '}' {
            tokens = append(tokens, Token{CloseParen, "}", line, column})
            column++
            i++
        } else if r == ' ' {
            var tokenType TokenType = Spaces
            if i > 0 && runes[i - 1] == '\n' {
                tokenType = Indentation
            }
            firstIdx := i
            for i < len(runes) && runes[i] == ' ' {
                column++
                i++
            }
            content := string(runes[firstIdx:i])
            if tokenType == Indentation && len(content) % IndentSpaces != 0 {
                return nil, errors.New("Indentation on line " + itoa(line) + " is not a multiple of " + itoa(IndentSpaces) + " spaces.")    
            }
            tokens = append(tokens, Token{tokenType, content, line, column})
        } else if r == '\t' {
            return nil, errors.New("File improperly contains a tab character: line " + itoa(line) + " and column " + itoa(column))
        } else if r == '`' {  // start of a string
            prev := r
            endIdx := i + 1
            endColumn := column
            endLine := line
            for { 
                if endIdx >= len(runes) {
                    return nil, errors.New("String literal not closed by end of file on line " + itoa(line) + " and column " + itoa(column))
                }
                current := runes[endIdx]
                if current == '\n' {
                    endLine++
                    endColumn = 1
                } else {
                    endColumn++
                }
                if current == '`' && prev != '\\' {  // end of the string
                    endIdx++
                    break
                }
                prev = current
                endIdx++
            }
            tokens = append(tokens, Token{StringLiteral, string(runes[i: endIdx]), line, column})
            column = endColumn
            line = endLine
            i = endIdx
        } else if isNumeral(r) {   // start of a number
            endIdx := i + 1
            for isNumeral(runes[endIdx]) {
                endIdx++
            }
            tokens = append(tokens, Token{NumberLiteral, string(runes[i: endIdx]), line, column})
            column += (endIdx - i)
            i = endIdx
        } else if isAlpha(r) || r == '_' {  // start of a word
            endIdx := i + 1
            for isAlpha(runes[endIdx]) || runes[endIdx] == '_' {
                endIdx++
            }

            content := string(runes[i: endIdx])

            tokens = append(tokens, Token{Word, content, line, column})            
            column += (endIdx - i)
            i = endIdx
        } else if isSigil(r) {
            tokens = append(tokens, Token{Sigil, string(r), line, column})
            column++
            i++
        } else {
            return nil, errors.New("Unexpected character " + string(r) + " at line " + itoa(line) + ", column " + itoa(column))
        }
    }
    return tokens, nil
}

func read(tokens []Token) ([]ReaderData, error) {
    readerData := make([]ReaderData, 0)
    for i := 0; i < len(tokens); {
        t := tokens[i]
        switch t.Type {
        case Word:
            rd, nTokens, err := readLine(tokens[i:], 0, false)
            if err != nil {
                return nil, err
            }
            readerData = append(readerData, rd...)
            i += nTokens
        case Indentation:
            i++
            if i < len(tokens) && tokens[i].Type == Newline {
                i++  // ignore blank lines
            } else {
                return nil, errors.New("Unexpected indentation at top-level of code on line " + itoa(t.LineNumber))
            }
        case Newline:
            i++ // ignore blank lines
        default:
            return nil, errors.New("Unexpected token at top-level of code on line " + itoa(t.LineNumber))
        }
    }
    return readerData, nil
}

// on lines with leading open delimiters, a recursive call is made with leadingOpenDelimiters true
func readLine(tokens []Token, indentation int, leadingOpenDelimiters bool) ([]ReaderData, int, error) {
    switch tokens[0].Type {
    case OpenParen, OpenSquare, OpenCurly:
        i := 0
        for ; i < len(tokens); i++ {
            t := tokens[i]
            switch t.Type {
            case OpenParen, OpenSquare, OpenCurly:
            default:
                break
            }
        }
        openDelimiters := tokens[0:i]
        readerData, n, err := readLine(tokens[i:], indentation, true)
        if err != nil {
            return nil, 0, err
        }
        nTokens := n + i
        for i := len(openDelimiters) - 1; i > 0; i-- {
            d := openDelimiters[i]
            switch d.Type {
            case OpenParen:
                readerData = []ReaderData{ParenList{readerData, d.LineNumber, d.Column}}
            case OpenSquare:
                readerData = []ReaderData{SquareList{readerData, d.LineNumber, d.Column}}
            case OpenCurly:
                readerData = []ReaderData{CurlyList{readerData, d.LineNumber, d.Column}}
            default:
                return nil, 0, errors.New("Internal error with leading open delimiters. Line " + itoa(d.LineNumber))
            }
        }
        return readerData, nTokens, nil
    case Word, Sigil, NumberLiteral, StringLiteral:
        readerData := make([]ReaderData, 0)
        i := 0
        Loop:
        for {
            t := tokens[i]
            switch t.Type {
            case Word, Sigil, NumberLiteral, StringLiteral:
                atom, n, err := readAtom(tokens[i:])
                if err != nil {
                    return nil, 0, err
                }
                i += n
                readerData = append(readerData, atom)
            case Spaces:
                i++
            case Newline:
                break Loop
            case OpenParen, OpenSquare, OpenCurly:
                list, n, err := readList(tokens[i:])
                if err != nil {
                    return nil, 0, err
                }
                i += n
                if !(tokens[i].Type == Spaces || tokens[i].Type == Newline) {
                    return nil, 0, errors.New("List on line " + itoa(t.LineNumber) + " column " + 
                        itoa(t.Column) + " should be followed by spaces or a newline.")
                }
                readerData = append(readerData, list)
            default:
                return nil, 0, errors.New("Unexpected token on line " + itoa(t.LineNumber) + " column " + itoa(t.Column))
            }
        }

        bodyIndentation := indentation + IndentSpaces
        Loop2:
        for i < len(tokens) {
            t := tokens[i]
            switch t.Type {
            case Newline:
                i++    // blank line
            case Indentation:
                if tokens[i + 1].Type == Newline {   // blank line
                    i += 2
                } else {
                    thisIndent := len(t.Content)
                    if thisIndent == bodyIndentation {
                        i++ // the indent token
                        data, n, err := readLine(tokens[i:], bodyIndentation, false)
                        if err != nil {
                            return nil, 0, err
                        }
                        readerData = append(readerData, data...)
                        i += n
                    } else if thisIndent > bodyIndentation {
                        return nil, 0, errors.New("Improper indentation in on line " + itoa(t.LineNumber))
                    } else {
                        break Loop2
                    }
                }
            default:
                break Loop2
            }
        }

        // if implicit open paren
        if !leadingOpenDelimiters && tokens[0].Type == Word && 
                (tokens[1].Type == Spaces || tokens[1].Type == Newline) {
            readerData = []ReaderData{ParenList{readerData, tokens[0].LineNumber, tokens[0].Column}}
        }

        return readerData, i, nil
    default:
        return nil, 0, errors.New("Unexpected token reading line " + itoa(tokens[0].LineNumber))
    }

    return nil, 0, nil
}

func readAtom(tokens []Token) (ReaderData, int, error) {
    i := 0
    elements := make([]ReaderData, 0)
    Loop:
    for i < len(tokens) {
        t := tokens[i]
        switch t.Type {
        case Word:
            elements = append(elements, Symbol{t.Content, t.LineNumber, t.Column})
            i++
        case Sigil:
            elements = append(elements, SigilAtom{t.Content, t.LineNumber, t.Column})
            i++
        case NumberLiteral:
            elements = append(elements, NumberAtom{t.Content, t.LineNumber, t.Column})
            i++
        case StringLiteral:
            elements = append(elements, StringAtom{t.Content, t.LineNumber, t.Column})
            i++
        case Spaces, Newline, CloseParen, CloseSquare, CloseCurly:
            break Loop
        case OpenParen, OpenSquare, OpenCurly:
            list, n, err := readList(tokens[i:])
            if err != nil {
                return nil, 0, err
            }
            i += n
            elements = append(elements, list)
        default:
            return nil, 0, errors.New("Unexpected atom token: line " + itoa(t.LineNumber) + " column " + itoa(t.Column))
        }
    }

    if len(elements) > 1 {
        return Catena{elements, tokens[0].LineNumber, tokens[0].Column}, i, nil
    } else if len(elements) == 1 {
        return elements[0], i, nil
    } else {
        return nil, 0, errors.New("Expected atom but got other token on line " + itoa(tokens[0].LineNumber))
    }

}

func readList(tokens []Token) (ReaderData, int, error) {
    var endDelimiter TokenType
    t := tokens[0]
    switch t.Type {
    case OpenParen:
        endDelimiter = CloseParen
    case OpenSquare:
        endDelimiter = CloseSquare
    case OpenCurly:
        endDelimiter = CloseCurly
    default:
        return nil, 0, errors.New("Internal error. Expecting ( [ or {. Line " + itoa(t.LineNumber))
    }

    var nTokens int
    elements := make([]ReaderData, 0)
    Loop:
    for i := 1; i < len(tokens); {
        t := tokens[i]
        switch t.Type {
        case endDelimiter:
            nTokens = i + 1
            break Loop
        case Word, Sigil, NumberLiteral, StringLiteral:
            atom, n, err := readAtom(tokens[i:])
            if err != nil {
                return nil, 0, err
            }
            i += n
            elements = append(elements, atom)
        case Spaces:
            i++
        case OpenParen, OpenSquare, OpenCurly:
            list, n, err := readList(tokens[i:])
            if err != nil {
                return nil, 0, err
            }
            i += n
            elements = append(elements, list)
        case Newline:
            return nil, 0, errors.New("List not properly closed on line " + itoa(t.LineNumber))
        }
    }

    var list ReaderData
    switch t.Type {
    case OpenParen:
        list = ParenList{elements, t.LineNumber, t.Column}
    case OpenSquare:
        list = SquareList{elements, t.LineNumber, t.Column}
    case OpenCurly:
        list = CurlyList{elements, t.LineNumber, t.Column}
    }
    return list, nTokens, nil
}

func parse(readerData []ReaderData) ([]TopDef, error) {
    topDefs := make([]TopDef, 0)

    return topDefs, nil
}

func compile(top []TopDef) (string, error) {
    var code string

    return code, nil
}

func main() {
    // if len(os.Args) != 2 {
    //     fmt.Println("Must specify a .animvs file.")
    //     return
    // }
    // inputFilename := os.Args[1]
    inputFilename := "example.animvs"

    start := time.Now()

    data, err := ioutil.ReadFile(inputFilename)
    if err != nil {
        fmt.Println(err)
        return
    }

    data = append(data, '\n')
    tokens, err := lex(string(data))
    if err != nil {
        fmt.Println(err)
        return
    }

    readerData, err := read(tokens)
    if err != nil {
        fmt.Println(err)
        return
    }

    spew.Dump(readerData)

    elapsed := time.Since(start)
    debug("Time: ", elapsed)

    return

    topDefs, err := parse(readerData)
    if err != nil {
        fmt.Println(err)
        return
    }

    code, err := compile(topDefs)
    if err != nil {
        fmt.Println(err)
        return
    }

    outputFilename := inputFilename + ".go"
    err = ioutil.WriteFile(outputFilename, []byte(code), os.ModePerm)
    if err != nil {
        fmt.Println(err)
        return
    }

    err = exec.Command("go", "fmt", outputFilename).Run()
    if err != nil {
        fmt.Println(err)
        return
    }

    cmd := exec.Command("go", "run", outputFilename)
    cmd.Stdin = os.Stdin
    cmd.Stdout = os.Stdout
    cmd.Stderr = os.Stderr
    err = cmd.Run()
    if err != nil {
        fmt.Println(err)
        return
    }
}