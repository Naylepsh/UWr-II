let canvas = document.getElementById("myCanvas");
let ctx = canvas.getContext("2d");
document.addEventListener("mousemove", mouseMoveHandler, false);
document.addEventListener("click", gameStarter, false);

const rectSize = 40;
const maxWidth = canvas.width;
const maxHeight = canvas.height;
const rectScoreWorth = 50;
const rectScorePunishment = Math.floor(rectScoreWorth * 2);
const maxRects = 10;
let startTime;
let accTime;
let isMouseInCenter;
let wasMouseInCenter;
let rects;
let rectsEaten;
let hasToGoBack;
let hasGameStarted;
let isGameOver;
let allScores = [];
let timesPunished;

class Rectangle {
    constructor(x, y){
        this.x = x;
        this.y = y;
        this.size = rectSize;
        this.usedBefore = false;
        this.isProtected = false;
    }

    topLeft(){
        return [this.x - this.size / 2, this.y - this.size / 2];
    }

    topRight(){
        return [this.x + this.size / 2, this.y - this.size / 2];
    }

    botLeft(){
        return [this.x - this.size / 2, this.y + this.size / 2];
    }

    botRight(){
        return [this.x + this.size / 2, this.y + this.size / 2];
    }

    containsPoint(x,y){
        return (x >= this.topLeft()[0] &&
                x <= this.topRight()[0] &&
                y >= this.topLeft()[1] &&
                y <= this.botLeft()[1]);
    }

    containsRect(rect){
        let contains = false;
        const points = [rect.topLeft(), rect.topRight(), rect.botLeft(), rect.botRight()];
        let i;
        for (i = 0; i < 4; i++){
            let point = points[i];
            if (this.containsPoint(point[0], point[1]))
                contains = true;
        }
        return contains;
    }

    draw(){
        let tl = this.topLeft();
        ctx.beginPath();
        ctx.rect(tl[0], tl[1], rectSize, rectSize);
        if (this.usedBefore)
            ctx.fillStyle = "red";
        else
            ctx.fillStyle = "green";
        ctx.fill();
        ctx.closePath();
    }
}

function spawnRects(n){
    let i = 0;
    while (i < n){
        let x = Math.floor(Math.random() * (maxWidth - 2*rectSize)) + rectSize;
        let y = Math.floor(Math.random() * (maxHeight - 2*rectSize)) + rectSize;
        let rect = new Rectangle(x, y);
        let j;
        let canBePlaced = true;
        // check if any of other currently 'approved' rectangles take it's space
        for (j = 0; j < rects.length; j++){
            if (rects[j].containsRect(rect))
                canBePlaced = false;
        }
        if (canBePlaced){
            i += 1;
            rects.push(rect);
        }
    }
}

function spawnCenterRect(){
    let x = maxWidth / 2;
    let y = maxHeight / 2;
    let rect = new Rectangle(x,y);
    rects.push(rect);
}

function drawRects(){
    let i;
    for (i = 0; i < rects.length; i++){
        rects[i].draw();
    }
    return i;
}

function mouseMoveHandler(e) {
    if (hasGameStarted && !isGameOver) {
        let x = e.clientX;
        let y = e.clientY;
        let i;
        isMouseInCenter = false;
        for (i = 0; i < rects.length; i++) {
            if (rects[i].containsPoint(x, y)) {
                if (i > 0) {
                    if (rects[i].usedBefore && !rects[i].isProtected) {
                        timesPunished += 1;
                        giveProtection(rects[i]);
                    }
                    else if (rects[i].isProtected) {
                        //do nothing atm
                    }
                    else if (hasToGoBack) {
                        //do nothing atm
                    }
                    else {
                        eatRect(rects[i]);
                        giveProtection(rects[i]);
                    }
                }
                if (i === 0) {
                    hasToGoBack = false;
                    isMouseInCenter = true;
                    if (!wasMouseInCenter) {
                        wasMouseInCenter = true;
                    }
                }
                updateScore();
            }
        }
        if (!isMouseInCenter) {
            wasMouseInCenter = false;
        }
    }
}

function gameStarter(e){
    let x = e.clientX;
    let y = e.clientY;
    if (rects[0].containsPoint(x,y) && !hasGameStarted){
        hasGameStarted = true;
    }
    else if (rects[0].containsPoint(x,y) && hasGameStarted) {
        newGame();
    }
}

function updateScore() {
    document.getElementById("score").innerHTML = countScore();
}

function eatRect(rect) {
    rect.usedBefore = true;
    hasToGoBack = true;
    rectsEaten += 1;
    rect.draw();
    score += rectScoreWorth;
    if (rectsEaten === maxRects){
        gameOver();
    }
}

function giveProtection(rect){
    rect.isProtected = true;
    setTimeout(function(){ rect.isProtected = false; }, 2000);
}

function timer(){
    if (!isGameOver) {
        let t = new Date().getTime();
        if (isMouseInCenter && !wasMouseInCenter) {
            accTime = accTime + t - startTime;
            startTime = t;
        }
        else if (isMouseInCenter && wasMouseInCenter) {
            startTime = t;
        }
        else {
            accTime = accTime + t - startTime;
        }
        updateScore();
    }
}

function countScore(){
    return Math.floor(rectsEaten / accTime * 1000000 - timesPunished * rectScorePunishment);
}

function gameOver(){
    isGameOver = true;
    const score = countScore();
    allScores.push(score);
    document.getElementById("score").innerHTML = score;
    document.getElementById("block1").style.backgroundColor = "maroon";
    document.getElementById("all_scores").innerText += "\n" + score;
}

function newGame(){
    ctx.clearRect(0,0,canvas.width,canvas.height);
    document.getElementById("score").innerHTML = "";
    document.getElementById("block1").style.backgroundColor = "wheat";
    startTime = new Date().getTime();
    accTime = 0;
    isMouseInCenter = true;
    wasMouseInCenter = true;
    hasToGoBack = false;
    hasGameStarted = true;
    rectsEaten = 0;
    rects = [];
    hasGameStarted = false;
    isGameOver = false;
    timesPunished = 0;
    setInterval(timer, 500);

    spawnCenterRect();
    spawnRects(maxRects);
    drawRects();
}

newGame();