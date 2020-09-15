const maxX = 600;
const maxY = 500;
const minX = 80;
const minY = 80;


class Rectangle {
    constructor(x, y, width, height){
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
    }

    topLeft(){
        return [this.x, this.y];
    }

    topRight(){
        return [this.x + this.width, this.y];
    }

    botLeft(){
        return [this.x, this.y + this.height];
    }

    botRight(){
        return [this.x + this.width, this.y + this.height];
    }

    containsPoint(x,y){
        return (x >= this.topLeft()[0] &&
            x <= this.topRight()[0] &&
            y >= this.topLeft()[1] &&
            y <= this.botLeft()[1]);
    }

    doesOverlapWith(rect2){
        let overlap = false;
        let i = 0;
        let maxI = 100;
        while (!overlap && i < maxI){
            let x = getRandomInt(this.topRight()[0] - this.topLeft()[0]) + this.topLeft()[0];
            let y = getRandomInt(this.botRight()[1] - this.topLeft()[1]) + this.topLeft()[1];
            overlap = rect2.containsPoint(x, y);
            i += 1;
        }
        return overlap;
    }
}

function spawnRects(n){
    let rects = [];
    let i = 0;
    while (i < n){
        let x = getRandomInt(maxX - minX) + 2*minX;
        let y = getRandomInt(maxY - minY);
        let width = Math.max(getRandomInt(maxX - x), minX);
        let height = Math.max(getRandomInt(maxY - y), 20);
        let rect = new Rectangle(x, y, width, height);
        let j;
        let canBePlaced = true;
        for (j = 0; j < rects.length; j++){
            if (rects[j].doesOverlapWith(rect)) {
                canBePlaced = false;
            }
        }
        if (canBePlaced){
            i += 1;
            rects.push(rect);
        }
    }
    placeRects(rects);
}

function spawnOtherRects() {
    const n = 10;
    const width = (maxX - minX) / n;
    const height = maxY / n;
    let rects = [];
    let i;
    for (i = 0; i < n; i++){
        let j;
        for (j = 0; j < 10; j++) {
            let rect = Rectangle(minX + i*width, j*height, width, height);
            rects.push(rect);
        }
    }
    placeRects(rects);
}

function placeRects(rects){
    let i;
    for (i = 0; i < rects.length; i++){

        let rect = document.createElement("div");
        rect.id = "rect"+i;
        rect.style.position = "absolute";
        rect.style.left = rects[i].topLeft()[0] + "px";
        rect.style.top = rects[i].topLeft()[1] + "px";
        rect.style.width = rects[i].width + "px";
        rect.style.height = rects[i].height + "px";
        rect.ondragover = (e)=>allowDrop(e);
        rect.ondrop = (e)=>dropColor(e, rect);
        $("#drawing").append(rect);

        /*
        let rect = document.createElement("div");
        let id = "rect"+i;
        rect.id = "rect"+i;
        $("#drawing").get(0).appendChild(rect);
        $("#"+id).css("position", "absolute");
        $("#"+id).css("left",rects[i].topLeft()[0]+"px");
        $("#"+id).css("top",rects[i].topLeft()[1]+"px");
        $("#"+id).css("width",rects[i].width);
        $("#"+id).css("height",rects[i].height);
        $("#"+id).on("dragover", (e)=>allowDrop(e));
        $("#"+id).on("drop", (e)=>dropColor(e, rect));
        */
    }
}

function getRandomInt(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

function addSetOfDragFunctions(){
    let colors = ["colorBlack", "colorLightGrey", "colorDarkGrey",
    "colorBlue", "colorGreen", "colorRed", "colorYellow"];
    let i;
    for (i = 0; i < colors.length; i++){
        let elem = document.querySelector("." +colors[i]);
        elem.draggable = true;
        elem.ondragstart = drag
    }
}

function allowDrop(e)
{
    e.preventDefault();
    return false;
}

function drag(e)
{
    let elem = document.querySelector('.'+e.target.className);
    let style = getComputedStyle(elem);
    e.dataTransfer.setData("Text",style.backgroundColor);
}

function dropColor(event, target)
{
    event.preventDefault();
    target.style.backgroundColor = event.dataTransfer.getData("Text");
}
/*
$(document).ready(function(){
    spawnRects(7);
    addSetOfDragFunctions();
});*/

addSetOfDragFunctions();
spawnRects(7);
//spawnOtherRects();
