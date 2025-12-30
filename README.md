# Status Update: December 30th,2025
Gauntlet has not been abandoned; is being re-written in order to improve maintainability and introduce feautures that will best serve the Golang community. 

# What's Gauntlet?

Gauntlet is a programming language designed to address many of Golang’s shortcomings. It transpiles exclusively to Go code, fully supports all of its features, and integrates seamlessly with its entire ecosystem — without the need for bindings.

---

# Documentation

Full documentation for the language, including several examples, can be found [here](https://gauntletlang.gitbook.io/docs).

---

# Contributing
Contributions would be greatly appreciated. Check out the [contributor's guide](CONTRIBUTING.md).

---

# Gauntlet Example

```fs
package main

// Seamless interop with the entire golang ecosystem
import "fmt" as fmt
import "os" as os
import "strings" as strings
import "strconv" as strconv


// Explicit export keyword
export fun getTrimmedFileLines(fileName: String): ([]String, Error) {
  // try-with syntax replaces verbose `err != nil` error handling
  let fileContent, err = try os.readFile(fileName) with (null, err)
  
  // Type conversion
  let fileContentStrVersion = (String)(fileContent) 

  let trimmedLines = 
    // Pipes feed output of last function into next one
    fileContentStrVersion
    |> strings.trimSpace(_)
    |> strings.split(_, "\n")

  // `nil` is equal to `null` in Gauntlet
  return (trimmedLines, null)
    
}


fun main(): Unit {
  // No 'unused variable' errors
  let a = 1 

  // force-with syntax will panic if err != nil
  let lines, err = force getTrimmedFileLines("example.txt") with err
  
  // Ternary operator
  let properWord = @String len(lines) > 1 ? "lines" : "line"

  let stringLength = lines |> len(_) |> strconv.itoa(_)

  fmt.println("There are " + stringLength + " " + properWord + ".")
  fmt.println("Here they are:")

  // Simplified for-loops
  for let i, line in lines {
    fmt.println("Line " + strconv.itoa(i + 1) + " is:")
    fmt.println(line)
  }

}

```
