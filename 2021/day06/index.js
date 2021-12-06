const fs = require('fs')
const input = fs.readFileSync('day06/input').toString().split(",").map(x => Number(x))

function simulate(days, _ages) {
    const ages = [..._ages]

    for(let j = 0; j < days; j++) {
        let newFishes = 0
        for(let i = 0; i < ages.length; i++) {
            if (i === 0) {
                newFishes += ages[i]     
            } else {
                ages[i - 1] += ages[i]
            }

            ages[i] = 0
        }

        ages[6] += newFishes
        ages[8] += newFishes
    }

    return ages.reduce((p, c) => p + c, 0)
}

const ages = [0, 0, 0, 0, 0, 0, 0, 0, 0]
input.forEach(x => ages[x]++)
console.log('part 1: ', simulate(80, ages))
console.log('part 1: ', simulate(256, ages))






