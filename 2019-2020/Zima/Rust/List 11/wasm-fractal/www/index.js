import {Mandelbrot} from "wasm-fractal";

function createCanvas() {
  let canvas = document.createElement("canvas");
  canvas.width=1024;
  canvas.height=1024;
  document.body.appendChild(canvas);
  return canvas;
}

let animationId = null;

const isPaused = () => {
  return animationId === null;
};

const playPauseButton = document.getElementById("play-pause");

const play = () => {
  playPauseButton.textContent = "⏸";
  renderLoop();
};

const pause = () => {
  playPauseButton.textContent = "▶";
  cancelAnimationFrame(animationId);
  animationId = null;
};

playPauseButton.addEventListener("click", event => {
  if (isPaused()) {
    play();
  } else {
    pause();
  }
});

let canvas = createCanvas();
let ctx = canvas.getContext("2d");
let fractal = Mandelbrot.new(canvas.width, canvas.height);

let magnificationFactor = 200;
let panX = 1.0;
let panY = 0.3;

const renderLoop = () => {
  let p1 = performance.now();
  let image = fractal.to_image(magnificationFactor, panX, panY);
  ctx.putImageData(image, 0, 0);
  
  magnificationFactor += 2;
  let p2 = performance.now();
  console.log(p2 - p1,'ms');
  animationId = requestAnimationFrame(renderLoop);
};

play();