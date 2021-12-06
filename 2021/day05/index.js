const fs = require('fs')
const input = fs.readFileSync('day05/input').toString().split("\n").filter(x => x.length > 0)

class Line {
    start
    end

    constructor(startX, startY, endX, endY) {
        this.start = {
            x: Number(startX),
            y: Number(startY)
        }
        this.end = {
            x: Number(endX),
            y: Number(endY)
        }
    }

    getPoints() {
        if (this.start.x === this.end.x) {
            // horizontal
            if (this.start.y > this.end.y) {
                // right -> left
                return this.countDown(this.start, this.end, 'y')
            } else {
                // left -> right
                return this.countUp(this.start, this.end, 'y')
            }
        } else if (this.start.y === this.end.y) {
            // vertical
            if (this.start.x > this.end.x) {
                // bottom -> top
                return this.countDown(this.start, this.end, 'x')
            } else {
                // top -> bottom
                return this.countUp(this.start, this.end, 'x')
            }

        } else {
            // diagonal
            if (this.start.x > this.end.x && this.start.y > this.end.y) {
                // right top -> left bottom 
                return this.countDiagonal(this.start, this.end, '-', '-')
            } else if (this.start.x < this.end.x && this.start.y < this.end.y) {
                // left top -> right bottom
                return this.countDiagonal(this.start, this.end, '+', '+')
            } else if (this.start.x < this.end.x && this.start.y > this.end.y) {
                // left bottom -> right top
                return this.countDiagonal(this.start, this.end, '+', '-')
            } else if (this.start.x > this.end.x && this.start.y < this.end.y) {
                // right bottom -> left top
                return this.countDiagonal(this.start, this.end, '-', '+')
            }
        }
    }

    countDiagonal(start, end, xOp, yOp) {
        let count = {...start}
        const result = [end]

        while(count.x !== end.x && count.y !== end.y) {
            result.push({...count})
            count.x = xOp === '+' ? count.x + 1 : count.x - 1
            count.y = yOp === '+' ? count.y + 1 : count.y - 1
        }

        return result
    }

    countUp(start, end, coord) {
        let count = start
        const result = [end]

        while(count[coord] < end[coord]) {
            result.push({...count})
            count[coord] = count[coord] + 1
        }

        return result
    }

    countDown(start, end, coord) {
        let count = start
        const result = [end]

        while(count[coord] > end[coord]) {
            result.push({...count})
            count[coord] = count[coord] - 1
        }

        return result
    }
}

const lines = input.map(x => {
    const points = x.split(" -> ")
    const start = points[0].split(',')
    const end = points[1].split(',')

    return new Line(start[0], start[1], end[0], end[1])
})


const res = new Array(1000).fill(null).map(x => new Array(1000).fill(null).map(() => 0))
const horVertLines = lines.map(x => x.getPoints())

horVertLines.forEach(line => {
    line.forEach(({x, y}) => {
        res[x][y]++
    })
})

const part1 = res.map(row => {
    return row.map(x => x > 1 ? 1 : 0).reduce((p, c) => p + c, 0)
}).reduce((p, c) => p + c, 0)

console.log(part1)








