import {Mandelbrot} from "wasm-fractal";

let animationId = null;

const isPaused = () => {
  return animationId === null;
};

const $ = elem => document.querySelector(elem);

const playPauseButton = $("#play-pause");

const play = () => {
  playPauseButton.textContent = "⏸";
  renderLoop();
};

const pause = () => {
  playPauseButton.textContent = "▶";
  cancelAnimationFrame(animationId);
  animationId = null;
};

const renderLoop = () => {
  // let p1 = performance.now();
  let image = fractal.to_image(magnificationFactor, panX, panY);
  ctx.putImageData(image, 0, 0);
  
  magnificationFactor += factorIncrease;
  if (factorIncrease < 10000) {
    factorIncrease += 2;
  }
  // console.log(performance.now() - p1);
  animationId = requestAnimationFrame(renderLoop);
};

playPauseButton.addEventListener("click", event => {
  if (isPaused()) {
    play();
  } else {
    pause();
  }
});

const reset = (event) => {
  const panXValue = Number($('#panX').value);
  const panYValue = Number($('#panY').value);
  const initMagnificationValue = Number($('#initialMagnification').value);
  const maxIterValue = Number($('#maxIter').value);
  panX = panXValue ? panXValue : 1.0;
  panY = panYValue ? panYValue : 0.3;
  magnificationFactor = initMagnificationValue ? initMagnificationValue : 200;
  maxIter = maxIterValue ? maxIterValue : 50;
  factorIncrease = 1;
  // fractal = Mandelbrot.new(canvas.width, canvas.height, maxIter);
  magnificationFactor = 200;
}

$('#reset').addEventListener('click', reset);

let canvas = document.querySelector('canvas');
canvas.width = 1024;
canvas.height = 1024;
let ctx = canvas.getContext("2d");
let maxIter = 50;
let magnificationFactor = 200;
let panX = 1.0;
let panY = 0.3;
let factorIncrease = 1;
let fractal = Mandelbrot.new(canvas.width, canvas.height, maxIter);
play();
