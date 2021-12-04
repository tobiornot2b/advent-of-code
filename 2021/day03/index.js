const fs = require('fs')
const input0 = fs.readFileSync('day03/input').toString().split("\n")
const input = input0.map(x => x.split(''))

let gamma = ""
let epsilon = ""

const eachBinary = new Array(input[0].length).fill(null).map(() => [])

for(let j=0; j < input.length; j++) {
    const row = input[j]
    for (let i=0; i < row.length; i++) {
        eachBinary[i].push(Number(row[i]))
    }
}

eachBinary.forEach(binary => {
    const mostCommon = findMostCommonSimple(binary)

    if (mostCommon == 1) { 
        gamma += 1
        epsilon += 0
    } else  {
        gamma += 0
        epsilon += 1
    }
})

function findMostCommonSimple(arr) {
    let count1 = 0
    let count0 = 0

    arr.forEach(num => {
        if (num === 1) count1++
        else count0++
    })

    if (count1 > count0) { 
        return 1
    } else if (count1 < count0)  {
        return 0
    } else {
        return -1 // equal
    }
}

function findMostCommon(_arr, index) {
    let count1 = 0
    let count0 = 0

    const arr = getArrayOnIndex(_arr, index)

    arr.forEach(num => {
        if (num == 1) count1++
        else count0++
    })

    if (count1 > count0) { 
        return 1
    } else if (count1 < count0)  {
        return 0
    } else {
        return -1 // equal
    }
}

function getArrayOnIndex(arr, index) {
    const newArr = []

    for (let i = 0; i < arr.length; i++) {
        const elem = arr[i]

        newArr.push(elem[index])
    }

    return newArr
}

console.log('part one: ', parseInt(gamma, 2) * parseInt(epsilon, 2))

function getIndexesWithNumber(arr, index, num) {
    const indexes = [] 

    for(let i = 0; i < arr.length; i++) {
        if (arr[i][index] == num) {
            indexes.push(i)
        }
    }

    return indexes
}

function rec(arr, index, num, num1) {
    if (arr.length === 1) {
        return arr
    }

    const mostCommon = findMostCommon(arr, index)

    let indexes
    if (mostCommon === 1 || mostCommon === -1) {
        indexes = getIndexesWithNumber(arr, index, num) 
    } else {
        indexes = getIndexesWithNumber(arr, index, num1)
    }

    const newArr = arr.filter((e, index) => indexes.includes(index))

    return rec(newArr, ++index, num, num1)
}

const oxy = parseInt(rec(input0, 0, 1, 0), 2)
const co2 = parseInt(rec(input0, 0, 0, 1), 2)

console.log('part two: ', oxy * co2)


