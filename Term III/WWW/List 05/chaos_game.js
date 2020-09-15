$(document).ready(function() {
    let canvas = $("#myCanvas").get(0);
    let canvasCtx = canvas.getContext("2d");
    let width;
    let height;
    let centerX;
    let centerY;
    let points;
    let recentPoint;
    let maxIter = 100000;


    function initSize(){
        canvas.width = 800;
        canvas.height = 600;
        width = $(canvas).width();
        height = $(canvas).height();
        centerX = canvas.width / 2;
        centerY = canvas.height / 2;
        //console.log(width);
        //console.log(height);
    }

    function initPoints() {
        points = [
            [centerX, centerY - height / 2],
            [centerX - width / 2, centerY + height / 2],
            [centerX + width / 2, centerY + height / 2]];
        recentPoint = [centerX, centerY];
    }

    function getRandomInt(max) {
        return Math.floor(Math.random() * Math.floor(max));
    }

    function randomPoint() {
        /*
        1)Take a random point from starting 3 points
        2)Make a new point placed in the middle between that random point and the recent point
         */
        let i = getRandomInt(points.length);
        let x = (points[i][0] + recentPoint[0]) / 2;
        let y = (points[i][1] + recentPoint[1]) / 2;
        return [x,y];
    }

    function drawPoint(point) {
        canvasCtx.fillRect(Math.floor(point[0]), Math.floor(point[1]), 1, 1);
    }

    function run(n) {
        let i;
        for (i = 0; i < n; i++) {
            recentPoint = randomPoint();
            drawPoint(recentPoint);
            //console.log(recentPoint)
        }
    }

    function start() {
        initSize();
        canvasCtx.clearRect(0, 0, canvas.width, canvas.height);
        initPoints();
        run();
    }

    start();
    window.setInterval(function() {
        run(maxIter/1000);
    }, 100);
});