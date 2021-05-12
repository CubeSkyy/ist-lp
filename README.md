# Sudoku Solver

Project for the 2016-2017 Logic for Programming Course in Instituto Superior Técnico. 

This project solves a sudoku puzzle using a logic programming language.

## Prerequisites

[SWI Prolog](https://www.swi-prolog.org/)

## Running the program

Open a SWI Prolog terminal and load the "Projecto.pl" file.

A puzzle can be solved using the following commands:
```prolog
exemplo_puzzle(11,[[[],[9],[],[7],[],[],[8],[6],[]],
 [[],[3],[1],[],[],[5],[],[2],[]],
 [[8],[],[6],[],[],[],[],[],[]],
 [[],[],[7],[],[5],[],[],[],[6]],
 [[],[],[],[3],[],[7],[],[],[]],
 [[5],[],[],[],[1],[],[7],[],[]],
 [[],[],[],[],[],[],[1],[],[9]],
 [[],[2],[],[6],[],[],[3],[5],[]],
 [[],[5],[4],[],[],[8],[],[7],[]]]).
```

```prolog
exemplo_puzzle(11,P),resolve(P,S),escreve_puzzle(S).
```

Output:

```python
[[2],[9],[5],[7],[4],[3],[8],[6],[1]]
[[4],[3],[1],[8],[6],[5],[9],[2],[7]]
[[8],[7],[6],[1],[9],[2],[5],[4],[3]]
[[3],[8],[7],[4],[5],[9],[2],[1],[6]]
[[6],[1],[2],[3],[8],[7],[4],[9],[5]]
[[5],[4],[9],[2],[1],[6],[7],[3],[8]]
[[7],[6],[3],[5],[2],[4],[1],[8],[9]]
[[9],[2],[8],[6],[7],[1],[3],[5],[4]]
[[1],[5],[4],[9],[3],[8],[6],[7],[2]]
```
