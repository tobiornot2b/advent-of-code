const fs = require('fs')
const input = fs.readFileSync('day02/input').toString().split("\n")

let hor = 0
let depth = 0

input.forEach(elem => {
    const splitted = elem.split(" ")
    const count = Number(splitted[1])

    switch(splitted[0]) {
        case "forward":
            hor += count
            break;
        case "up":
            depth -= count
            break;
        case "down":
            depth += count 
            break;
    }
})

console.log(hor * depth)

let aim = 0
hor = 0
depth = 0

input.forEach(elem => {
    const splitted = elem.split(" ")
    const count = Number(splitted[1])

    switch(splitted[0]) {
        case "forward":
            hor += count
            depth += aim * count
            break;
        case "up":
            aim -= count
            break;
        case "down":
            aim += count 
            break;
    }
})

console.log(hor * depth)

function sum(arr) {
    return arr.reduce((p, c) => p + c, 0)
}

