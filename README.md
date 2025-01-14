# LinksParser
## Description
LinksParser is a supporting piece of software written in OCaml which uses the Links programming language core library to produce Abstract Syntax Trees (AST) in a useable format. The main objective of this library is to aid the implementation of LinksLSP a Language Server Protocol for Links as extension in VSCode. 
## Functionality
LinksParser operates by creating a TCP server and listening for incoming data. Currently, this is hardcoded to listen on localhost on port 8081. It takes on input the file location to a Links file and outputs the appropriate AST as a String in JSON format. 

Note: Range information in the AST is 1-indexed (Thanks VSCode).
## Support Capability
While the AST generation works to an extent, it should not be assumed that this is complete. As this is constructed to support the development of LinksLSP, AST generation is implemented to aid the primary objective.

## Running
1. Clone repo
2. Compile project with `dune build`
3. Execute parser via `dune exec ./parser/parser.exe`
4. Communicate over localhost:8081 to send the path to a Links file (make sure to end the payload with a newline char: `\n`)


## Example Parse
```
{
  "type": "Node",
  "value": "Fun",
  "range": {
    "start": {
      "line": 1,
      "character": 2
    },
    "end": {
      "line": 3,
      "character": 2
    }
  },
  "children": [
    {
      "type": "Leaf",
      "value": "Binder: fun1",
      "range": {
        "start": {
          "line": 1,
          "character": 2
        },
        "end": {
          "line": 3,
          "character": 2
        }
      }
    },
    {
      "type": "Node",
      "value": "NormalFunlit",
      "range": {
        "start": {
          "line": 1,
          "character": 2
        },
        "end": {
          "line": 3,
          "character": 2
        }
      },
      "children": [
        {
          "type": "Leaf",
          "value": "Variable: x",
          "range": {
            "start": {
              "line": 1,
              "character": 11
            },
            "end": {
              "line": 1,
              "character": 12
            }
          }
        },
        {
          "type": "Node",
          "value": "Block",
          "range": {
            "start": {
              "line": 1,
              "character": 14
            },
            "end": {
              "line": 3,
              "character": 2
            }
          },
          "children": [
            {
              "type": "Leaf",
              "value": "Variable: x",
              "range": {
                "start": {
                  "line": 2,
                  "character": 5
                },
                "end": {
                  "line": 2,
                  "character": 6
                }
              }
            }
          ]
        }
      ]
    }
  ]
}
```