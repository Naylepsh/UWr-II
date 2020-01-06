import {Mandelbrot} from "wasm-fractal";

function createCanvas() {
  let canvas = document.createElement("canvas");
  canvas.width=1024;
  canvas.height=1024;
  document.body.appendChild(canvas);
  return canvas;
}

let canvas = createCanvas();
let ctx = canvas.getContext("2d");
let fractal = Mandelbrot.new(canvas.width, canvas.height);
let image = fractal.to_image(200, 2.0, 1.5);
ctx.putImageData(image, 0, 0);
// wasm.draw(200);
// for (let magnificationFactor = 200; magnificationFactor < 20000; magnificationFactor += 50) {
//   setTimeout(() => {
//     ctx.clearRect(0, 0, canvas.width, canvas.height);
//     draw(magnificationFactor);
//   }, 3000);
// }