const fs = require('fs')
const input = fs.readFileSync('day04/input').toString().split("\n")

const draw = input[0].split(',').map(x => Number(x))

class BingoEntry {
    wasFound = false
    number
    row
    column

    constructor(number, row, column) {
        this.number = number
        this.row = row
        this.column = column
    }
}

class BingoBoard {
    entries = []
    maxRow
    maxColumn
    winningRow = -1
    winningColumn = -1

    constructor(stringEntryRows) {
        this.maxColumn = stringEntryRows.length
        stringEntryRows.forEach((row, index) => {
            const newRows = row.split(' ').filter(x => x.length > 0).map(x => Number(x))
            this.maxRow = newRows.length
            this.entries = this.entries.concat(newRows.map((entry, y) => new BingoEntry(entry, index, y)))
        })
    }

    playNumber(number) {
        this.entries.filter(entry => entry.number === number).forEach(x => x.wasFound = true)
        const checkedRows = this.check(this.maxRow, 'row')
        const checkedColumns = this.check(this.maxColumn, 'column')

        this.winningRow = checkedRows.b ? checkedRows.index : this.winningRow
        this.winningColumn = checkedColumns.b ? checkedColumns.index : this.winningColumn

        return checkedRows.b || checkedColumns.b
    }

    check(maxValue, posRef) {
        let oneIsAllTrue = false
        let allTrueIndex = -1
        for(let i = 0; i < maxValue; i++) {
            const toCheck = this.entries.filter(x => x[posRef] === i) 
            const allTrue = toCheck.map(x => x.wasFound).reduce((prev, curr) => prev && curr, true)

            if (allTrue) {
                oneIsAllTrue = true
                allTrueIndex = i
            }
        }

        return { b: oneIsAllTrue, index: allTrueIndex }
    }

    getWinningSum() {
        return this.entries.filter(x => !x.wasFound).reduce((p, c) => p + c.number, 0)
    }

    hasWon() {
        return this.winningColumn !== -1 || this.winningRow !== -1
    }
}

const boardDataRows = input.slice(1)
const boardData = []

let lastBoardIndex = 0
boardDataRows.forEach(row => {
    if (row.length === 0) {
        lastBoardIndex++
    } else {
        const oldData = boardData[lastBoardIndex] 

        if (!Array.isArray(oldData)) {
            boardData[lastBoardIndex] = new Array()
        }
        boardData[lastBoardIndex].push(row)
    }
})

let boards = []
boardData.slice(1).forEach(data => {
    boards.push(new BingoBoard(data)) 
})

let winningBoard
let winningNumber
draw.forEach(num => {
    if (winningBoard === undefined) {
        const playResults = boards.map(b => b.playNumber(num))
        const oneBoardWinned = playResults.reduce((p, c) => p || c, false)

        if (oneBoardWinned) {
            winningBoard = boards[playResults.findIndex(x => x === true)]
            winningNumber = num
        }
    }
})

console.log('part one: ', winningBoard.getWinningSum() * winningNumber)

boards = []
boardData.slice(1).forEach(data => {
    boards.push(new BingoBoard(data)) 
})

winningBoard = undefined
draw.forEach(num => {
    if (winningBoard === undefined) {
        const boardsThatHaveNotWon = boards.filter(b => !b.hasWon())
        boardsThatHaveNotWon.forEach(b => b.playNumber(num))

        if (boardsThatHaveNotWon.length === 1 && boardsThatHaveNotWon[0].hasWon()) {
            winningBoard = boardsThatHaveNotWon[0]
            winningNumber = num
        }

    }
})
console.log('part two: ', winningBoard.getWinningSum() * winningNumber)




