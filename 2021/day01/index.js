const fs = require('fs')
const input = fs.readFileSync('day01/input').toString().split("\n").map(x => Number(x))

const result = sum(increased(input))

console.log('part one: ', result)

const zipped = input.map((x, index) => {
    return [x, input[index+1], input[index+2]]
}).map(x => x.reduce((p, c) => p + c, 0))

const result2 = sum(increased(zipped))

function increased(arr) {
    return arr.map((x, index) => { 
        if (index == 0) return 0

        if (arr[index] > arr[index - 1]) {
            return 1
        } else { return 0 }
    })
}

function sum(arr) {
    return arr.reduce((p, c) => p + c, 0)
}

console.log('part two: ', result2)
