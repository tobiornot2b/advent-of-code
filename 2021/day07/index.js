const fs = require('fs')
const input = fs.readFileSync('day07/input').toString().split(",").map(x => Number(x))

const positions = input.sort((a, b) => a - b)

const costs = positions.map(x => calculateCost(positions, x))

console.log('part 2: ', costs.sort((a,b) => a - b)[0])

function calculateCost(arr, elem) {
    let totalCosts = 0
    arr.forEach(e => {
        const cost = sum(Math.abs(elem - e))
        totalCosts += cost
    })

    return totalCosts
}

function sum(num) {
    return (num * (num + 1)) / 2
}







